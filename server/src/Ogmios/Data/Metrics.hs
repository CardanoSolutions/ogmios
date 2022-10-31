--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Metrics
    (
    -- * Metrics
      Metrics (..)
    , emptyMetrics

    -- ** RuntimeStats
    , Sampler
    , RuntimeStats (..)
    , emptyRuntimeStats

    -- ** DistributionStats
    , DistributionStats (..)
    , emptyDistributionStats
    ) where

import Ogmios.Prelude

import Data.Aeson
    ( ToJSON (..)
    , genericToJSON
    )

import qualified Data.Aeson as Json

--
-- Metrics
--

-- | Application metrics. Metrics shall be sampled at regular interval and fed
-- into a monitoring system such as 'promotheus'. They aim at capturing useful
-- measurements of the application runtime.
data Metrics = Metrics
    { runtimeStats :: !RuntimeStats
    -- ^ Proportion of the time spent doing actual work (vs garbage collecting)
    , activeConnections :: !Integer
    -- ^ Number of currently active connections
    , totalConnections :: !Integer
    -- ^ Total connections since the last restart
    , sessionDurations :: !DistributionStats
    -- ^ Statistics about the duration of each session, in ms
    , totalMessages :: !Integer
    -- ^ Total number of messages processed through websockets
    , totalUnrouted :: !Integer
    -- ^ Total number of messages which couldn't be routed through the protocol
    } deriving (Generic, Eq, Show)

instance ToJSON Metrics where
    toJSON = genericToJSON Json.defaultOptions

-- | Empty 'Metrics', for initialization.
emptyMetrics :: Metrics
emptyMetrics = Metrics emptyRuntimeStats 0 0 emptyDistributionStats 0 0

--
-- RuntimeStats
--

-- | A type alias to make signature a little more explicit.
type Sampler what m = m what

-- | Runtime statistics we're interested in capturing.
--
-- NOTE: GC = Garbage Collector
data RuntimeStats = RuntimeStats
    { maxHeapSize :: !Integer
        -- ^ Maximum live data in the heap, in KB
    , currentHeapSize :: !Integer
        -- ^ Current live data in the heap, in KB
    , cpuTime :: !Integer
        -- ^ Total CPU time (at the previous GC), in ns
    , gcCpuTime :: !Integer
        -- ^ Total CPU time used by the GC, in ns
    } deriving (Generic, Eq, Show)

-- | Empty 'RuntimeStats', for initialization.
emptyRuntimeStats :: RuntimeStats
emptyRuntimeStats = RuntimeStats 0 0 0 0

instance ToJSON RuntimeStats where
    toJSON = genericToJSON Json.defaultOptions

--
-- DistributionStats
--

-- | Simplistic representation of a statistical distribution of values.
data DistributionStats = DistributionStats
    { mean :: !Double
    , min :: !Double
    , max :: !Double
    } deriving (Generic, Eq, Show)

-- | Create an empty distribution with all values to 0
emptyDistributionStats :: DistributionStats
emptyDistributionStats = DistributionStats 0 0 0

instance ToJSON DistributionStats where
    toJSON = genericToJSON Json.defaultOptions
