--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Mildly safe here and "necessary" because of ekg's internal store. When
-- register values to sample from 'GHC.Stats', we have to convert each observee
-- to an opaque 'Ekg.Value'. Then, when reading back from the store, we get that
-- same opaque 'Ekg.Value' sum-type.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.Metrics
    (
    -- * Metrics
      Metrics(..)
    , empty

    -- * Sensors
    , Sampler
    , Sensors
    , init
    , recordSession
    ) where

import Prelude hiding
    ( init, max, min )

import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.HashMap.Strict
    ( HashMap )
import Data.Int
    ( Int64 )
import Data.Ratio
    ( (%) )
import Data.Scientific
    ( Scientific, fromRationalRepetendLimited )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import GHC.Stats
    ( RTSStats (..), getRTSStats, getRTSStatsEnabled )
import Ogmios.Metrics.Trace
    ( TraceMetrics (..) )
import System.Metrics.Counter
    ( Counter )
import System.Metrics.Distribution
    ( Distribution )
import System.Metrics.Gauge
    ( Gauge )
import System.Time.Clock
    ( nominalDiffTimeToMilliseconds, timed )

import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as Map
import qualified System.Metrics as Ekg
import qualified System.Metrics.Counter as Ekg.Counter
import qualified System.Metrics.Distribution as Ekg.Distribution
import qualified System.Metrics.Gauge as Ekg.Gauge

-- | Application metrics. See also 'init' to get a sampler for those values.
data Metrics = Metrics
    { productivity :: Scientific
        -- ^ Proportion of the time spent doing actual work (vs garbage collecting)
    , maxHeapSize :: Integer
        -- ^ Maximum live data in the heap, in KB
    , activeConnections :: Integer
        -- ^ Number of currently active connections
    , totalConnections :: Integer
        -- ^ Total connections since the last restart
    , sessionDurations :: DistributionStats
        -- ^ Statistics about the duration of each session, in ms
    } deriving (Generic, Eq, Show)

instance ToJSON Metrics where
    toJSON = genericToJSON Json.defaultOptions

-- | Empty metrics, for initialization.
empty :: Metrics
empty = Metrics 0 0 0 0 emptyDistribution

-- | Simplistic representation of a statistical distribution of values.
data DistributionStats = DistributionStats
    { mean :: Double
    , min :: Double
    , max :: Double
    } deriving (Generic, Eq, Show)

-- | Create an empty distribution with all values to 0
emptyDistribution :: DistributionStats
emptyDistribution = DistributionStats 0 0 0

instance ToJSON DistributionStats where
    toJSON = genericToJSON Json.defaultOptions

-- | An interface for capturing application metrics and measuring all sort of
-- things that are application-specific.
data Sensors = Sensors
    { activeConnectionsGauge :: Gauge
    , totalConnectionsCounter :: Counter
    , sessionDurationsDistribution :: Distribution
    }

-- | A type alias to make signature a little more explicit.
type Sampler m = m Metrics

-- | Record some metrics about a given session.
recordSession :: Sensors -> IO a -> IO a
recordSession sensors session = do
    Ekg.Counter.inc (totalConnectionsCounter sensors)
    Ekg.Gauge.inc (activeConnectionsGauge sensors)
    (a, duration) <- timed session
    Ekg.Distribution.add (sessionDurationsDistribution sensors) (ms duration)
    Ekg.Gauge.dec (activeConnectionsGauge sensors)
    return a
  where
    ms = fromIntegral . nominalDiffTimeToMilliseconds

-- | Register and activate application sensors, measuring _things_.
init :: Tracer IO TraceMetrics -> IO (Sensors, Sampler IO)
init tr = do
    store <- Ekg.newStore

    getRTSStatsEnabled >>= \case
        False -> traceWith tr (MetricsRuntimeStatsDisabled "run with '+RTS -T'")
        True  -> registerGroup getRTSStats store $ Map.fromList
            [ (maxHeapSizeId, Ekg.Gauge . int64 . max_live_bytes)
            , (cpuTimeId, Ekg.Counter . cpu_ns)
            , (gcCpuTimeId, Ekg.Counter . gc_cpu_ns)
            ]

    activeConnectionsGauge <- Ekg.Gauge.new
    totalConnectionsCounter <- Ekg.Counter.new
    sessionDurationsDistribution <- Ekg.Distribution.new

    let sensors = Sensors
            { activeConnectionsGauge
            , totalConnectionsCounter
            , sessionDurationsDistribution
            }

    return (sensors, sample store sensors)
  where
    maxHeapSizeId = "maxHeapSize"
    cpuTimeId = "cpuTime"
    gcCpuTimeId = "gcCpuTime"

    sample store sensors = do
        activeConnections <- toInteger
            <$> Ekg.Gauge.read (activeConnectionsGauge sensors)
        totalConnections  <- toInteger
            <$> Ekg.Counter.read (totalConnectionsCounter sensors)
        sessionDurations  <- toDistributionStats
            <$> Ekg.Distribution.read (sessionDurationsDistribution sensors)

        Ekg.sampleAll store <&> \metrics -> Metrics
            { activeConnections

            , totalConnections

            , sessionDurations

            , maxHeapSize = maxHeapSizeId `Map.lookup` metrics
                & maybe 0 (\(Ekg.Gauge g) -> (`div` 1024) $ toInteger g)

            , productivity =
                let
                    cpuTime = cpuTimeId `Map.lookup` metrics
                        & maybe 0 (\(Ekg.Counter c) -> toInteger c)

                    gcCpuTime = gcCpuTimeId `Map.lookup` metrics
                        & maybe 0 (\(Ekg.Counter c) -> toInteger c)
                in
                    if cpuTime + gcCpuTime == 0
                    then 1
                    else toApproxScientific $ cpuTime % (cpuTime + gcCpuTime)

            }

--
-- Helpers
--

-- | Conversion to 'int64', relatively safe.
int64 :: Integral a => a -> Int64
int64 = fromIntegral

-- | Convert an Ekg's internal 'Distribution.Stats' our own 'DistributionStats'
toDistributionStats :: Ekg.Distribution.Stats -> DistributionStats
toDistributionStats distr = DistributionStats
    { mean = Ekg.Distribution.mean distr
    , min  = Ekg.Distribution.min distr
    , max  = Ekg.Distribution.max distr
    }

-- | Convert a 'Rational' to a 'Scientific', dicarding remainders.
toApproxScientific :: Rational -> Scientific
toApproxScientific = either fst fst . fromRationalRepetendLimited 3

-- | Like ekg's 'registerGroup', but with some arguments flipped.
registerGroup
    :: IO a
    -- ^ Action to sample the metric group
    -> Ekg.Store
    -- ^ Metric store
    -> HashMap Text (a -> Ekg.Value)
    -- ^ Metric names and getter functions.
    -> IO ()
registerGroup sample store names =
    Ekg.registerGroup names sample store
