--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Ogmios.App.Metrics.Prometheus where

import Ogmios.Prelude

import Ogmios.App.Metrics
    ( RuntimeStats(..), Metrics(..) )
import Ogmios.Data.Health
    ( ConnectionStatus (..), Health (..), Tip (..), SlotInEpoch (..), NetworkSynchronization (..), EpochNo (..) )

import qualified Ogmios.Data.Metrics as Metrics

import Cardano.Slotting.Block (BlockNo(..))
import Cardano.Slotting.Slot (SlotNo(..))

import Data.ByteString.Builder (Builder)

import System.Metrics.Prometheus.Metric (MetricSample(..))
import System.Metrics.Prometheus.Metric.Counter (CounterSample(..))
import System.Metrics.Prometheus.Metric.Gauge (GaugeSample(..))
import System.Metrics.Prometheus.MetricId (Labels(..), MetricId(..), makeName)
import System.Metrics.Prometheus.Registry (RegistrySample (..))
import System.Metrics.Prometheus.Encode.Text (encodeMetrics)

import qualified Data.Map
import qualified Data.Scientific

-- | Convert `Health` status and metrics into
-- Prometheus format builder
asPrometheusMetrics :: Health block -> Builder
asPrometheusMetrics Health{..} =
  let Metrics{..} = metrics
      RuntimeStats{..} = runtimeStats

      gaugeSample :: Double -> MetricSample
      gaugeSample = GaugeMetricSample . GaugeSample

      counterSample :: Int -> MetricSample
      counterSample = CounterMetricSample . CounterSample

      converted :: [(Text, MetricSample)]
      converted =
        [ ("connected", if connectionStatus == Connected then gaugeSample 1 else gaugeSample 0) ]
        <> (maybe mempty (\(NetworkSynchronization ns) -> pure ("network_synchronization", gaugeSample $ Data.Scientific.toRealFloat ns)) networkSynchronization)
        <> (maybe mempty (\e -> pure ("current_epoch", counterSample $ fromEnum $ unEpochNo e)) currentEpoch)
        <> (maybe mempty (\s -> pure ("slot_in_epoch", counterSample $ fromEnum $ getSlotInEpoch s)) slotInEpoch)
        <>
        [ -- Metrics
          ("active_connections", gaugeSample $ fromInteger activeConnections)
        , ("total_connections", counterSample $ fromInteger totalConnections)
        , ("total_messages", counterSample $ fromInteger totalMessages)
        , ("total_unrouted", counterSample $ fromInteger totalUnrouted)
          -- Runtime stats
        , ("max_heap_size", gaugeSample $ fromInteger maxHeapSize)
        , ("current_heap_size", gaugeSample $ fromInteger currentHeapSize)
        , ("cpu_time", counterSample $ fromInteger cpuTime)
        , ("gc_cpu_time", counterSample $ fromInteger gcCpuTime)
          -- Session durations
        , ("session_duration_mean", gaugeSample $ Metrics.mean sessionDurations)
        , ("session_duration_mix", gaugeSample $ Metrics.min sessionDurations)
        , ("session_duration_max", gaugeSample $ Metrics.max sessionDurations)
        ]
        ++
        case lastKnownTip of
          TipGenesis -> pure ("tip_at_genesis", gaugeSample 1)
          Tip slot _hash block -> [
              ("tip_block", counterSample $ fromEnum $ unBlockNo block)
            , ("tip_slot", counterSample $ fromEnum $ unSlotNo slot)
            ]

  in encodeMetrics
   $ RegistrySample
   $ Data.Map.mapKeys
      (\k -> (MetricId (makeName $ "ogmios_" <> k) (Labels mempty)))
   $ Data.Map.fromList
     converted
