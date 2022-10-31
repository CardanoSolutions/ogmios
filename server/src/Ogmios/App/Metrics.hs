--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}

-- Mildly safe here and "necessary" because of ekg's internal store. When
-- register values to sample from 'GHC.Stats', we have to convert each observee
-- to an opaque 'Ekg.Value'. Then, when reading back from the store, we get that
-- same opaque 'Ekg.Value' sum-type.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.App.Metrics
    ( -- * Metrics
      Metrics(..)
    , RuntimeStats (..)

      -- * Sensors
    , Sensors (..)
    , newSensors
    , recordSession

      -- * Sampler
    , Sampler
    , newSampler
    , sample

      -- * Logging
    , TraceMetrics (..)
    ) where

import Ogmios.Prelude hiding
    ( max
    , min
    )

import Ogmios.Control.MonadClock
    ( MonadClock (..)
    , diffTimeToMilliseconds
    )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics (Counter, Distribution, Gauge)
    , count
    , decrement
    , increment
    , readCounter
    , readDistribution
    , readGauge
    , record
    )
import Ogmios.Data.Metrics
    ( DistributionStats (..)
    , Metrics (..)
    , RuntimeStats (..)
    , Sampler
    )

import qualified Ogmios.Control.MonadMetrics as Metrics

import Data.Aeson
    ( ToJSON (..)
    , genericToEncoding
    )
import Data.Time.Clock
    ( DiffTime
    )
import GHC.Stats
    ( GCDetails (..)
    , RTSStats (..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import Relude.Extra.Map
    ( lookup
    )

import qualified Data.Aeson as Json
import qualified System.Metrics as Ekg
import qualified System.Metrics.Counter as Ekg.Counter
import qualified System.Metrics.Distribution as Ekg.Distribution
import qualified System.Metrics.Gauge as Ekg.Gauge

--
-- Sensors
--

-- | An interface for capturing application metrics and measuring all sort of
-- things that are application-specific.
data Sensors (m :: Type -> Type) = Sensors
    { activeConnectionsGauge :: Gauge m
    , totalConnectionsCounter :: Counter m
    , sessionDurationsDistribution :: Distribution m
    , totalMessagesCounter :: Counter m
    , totalUnroutedCounter :: Counter m
    } deriving stock (Generic)

-- | Initialize new application sensors in 'IO'.
newSensors
    :: forall m.
        ( Gauge m ~ Ekg.Gauge.Gauge
        , Counter m ~ Ekg.Counter.Counter
        , Distribution m ~ Ekg.Distribution.Distribution
        )
    => IO (Sensors m)
newSensors = do
    activeConnectionsGauge <- Ekg.Gauge.new
    totalConnectionsCounter <- Ekg.Counter.new
    sessionDurationsDistribution <- Ekg.Distribution.new
    totalMessagesCounter <- Ekg.Counter.new
    totalUnroutedCounter <- Ekg.Counter.new
    pure Sensors
        { activeConnectionsGauge
        , totalConnectionsCounter
        , sessionDurationsDistribution
        , totalMessagesCounter
        , totalUnroutedCounter
        }

-- | Record some metrics about a given session. In particular, we have:
--
-- - A counter incremented for each connection
-- - A gauge counting the number of live connections
-- - A distribution of the sessions' durations
--
recordSession
    :: (MonadClock m, MonadMetrics m)
    => Sensors m
    -> m a
    -> m a
recordSession sensors session = do
    count (totalConnectionsCounter sensors)
    increment (activeConnectionsGauge sensors)

    (a, duration) <- second ms <$> timed session

    record (sessionDurationsDistribution sensors) duration
    decrement (activeConnectionsGauge sensors)

    return a
  where
    ms :: DiffTime -> Double
    ms = fromIntegral . diffTimeToMilliseconds

--
-- Sampler
--

-- | Construct a sampler within the application context.
newSampler
    :: forall m. (MonadIO m)
    => Logger TraceMetrics
    -> IO (Sampler RuntimeStats m)
newSampler tr = do
    store <- liftIO Ekg.newStore

    liftIO getRTSStatsEnabled >>= \case
        False -> logWith tr (MetricsRuntimeStatsDisabled "run with '+RTS -T'")
        True  -> registerGroup getRTSStats store $ fromList
            [ (maxHeapSizeId, Ekg.Gauge . fromIntegral . max_live_bytes)
            , (currentHeapSizeId, Ekg.Gauge . fromIntegral . gcdetails_live_bytes . gc)
            , (cpuTimeId, Ekg.Counter . cpu_ns)
            , (gcCpuTimeId, Ekg.Counter . gc_cpu_ns)
            ]

    return (sampler store)
  where
    maxHeapSizeId = "maxHeapSize"
    currentHeapSizeId = "currentHeapSize"
    cpuTimeId = "cpuTime"
    gcCpuTimeId = "gcCpuTime"

    sampler store = liftIO $
        Ekg.sampleAll store <&> \measures -> RuntimeStats
            { maxHeapSize = maxHeapSizeId `lookup` measures
                & maybe 0 (\(Ekg.Gauge g) -> (`div` 1024) $ toInteger g)

            , currentHeapSize = currentHeapSizeId `lookup` measures
                & maybe 0 (\(Ekg.Gauge g) -> (`div` 1024) $ toInteger g)

            , cpuTime = cpuTimeId `lookup` measures
                & maybe 0 (\(Ekg.Counter c) -> toInteger c)

            , gcCpuTime = gcCpuTimeId `lookup` measures
                & maybe 0 (\(Ekg.Counter c) -> toInteger c)
            }

-- | Sample 'Metrics' at a given point in time.
sample
    :: forall m.
        ( MonadMetrics m
        )
    => Sampler RuntimeStats m
    -> Sensors m
    -> m Metrics
sample sampleRuntime sensors = do
    activeConnections <- readGauge (activeConnectionsGauge sensors)
    totalConnections  <- readCounter (totalConnectionsCounter sensors)
    sessionDurations  <- readDistribution mkStats (sessionDurationsDistribution sensors)
    totalMessages     <- readCounter (totalMessagesCounter sensors)
    totalUnrouted     <- readCounter (totalUnroutedCounter sensors)

    sampleRuntime <&> \runtimeStats -> Metrics
        { runtimeStats
        , activeConnections
        , totalConnections
        , sessionDurations
        , totalMessages
        , totalUnrouted
        }
  where
    mkStats distr = DistributionStats
        { mean = Metrics.mean distr
        , min  = Metrics.min distr
        , max  = Metrics.max distr
        }

--
-- Helpers
--

-- | Like ekg's 'registerGroup', but with some arguments flipped.
registerGroup
    :: MonadIO m
    => IO a
    -- ^ Action to sample the metric group
    -> Ekg.Store
    -- ^ Metric store
    -> HashMap Text (a -> Ekg.Value)
    -- ^ Metric names and getter functions.
    -> m ()
registerGroup sampler store names =
    liftIO $ Ekg.registerGroup names sampler store

--
-- Logging
--

data TraceMetrics where
    MetricsRuntimeStatsDisabled
        :: { recommendation :: Text }
        -> TraceMetrics
    deriving stock (Generic, Show)

-- NOTE: This instance is hand-written because 'TraceMetrics' has only one
-- field and we want to force tagging of single constructors. If it included
-- more than one constructors, we could simply go with:
--
--     deriving anyclass (ToJSON)
--
instance ToJSON TraceMetrics where
    toEncoding = genericToEncoding
        (Json.defaultOptions
            { Json.tagSingleConstructors = True
            }
        )

instance HasSeverityAnnotation TraceMetrics where
    getSeverityAnnotation = \case
        MetricsRuntimeStatsDisabled{} -> Warning
