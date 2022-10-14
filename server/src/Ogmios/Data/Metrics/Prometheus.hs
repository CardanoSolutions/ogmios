--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

module Ogmios.Data.Metrics.Prometheus where

import Ogmios.Prelude

import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Data.ByteString.Builder
    ( Builder
    )
import Ogmios.App.Metrics
    ( Metrics (..)
    , RuntimeStats (..)
    )
import Ogmios.Data.Health
    ( ConnectionStatus (..)
    , EpochNo (..)
    , Health (..)
    , NetworkSynchronization (..)
    , SlotInEpoch (..)
    , Tip (..)
    )
import System.Metrics.Prometheus.Encode.Text
    ( encodeMetrics
    )
import System.Metrics.Prometheus.Metric
    ( MetricSample (..)
    )
import System.Metrics.Prometheus.Metric.Counter
    ( CounterSample (..)
    )
import System.Metrics.Prometheus.Metric.Gauge
    ( GaugeSample (..)
    )
import System.Metrics.Prometheus.MetricId
    ( Labels (..)
    , MetricId (..)
    , makeName
    )
import System.Metrics.Prometheus.Registry
    ( RegistrySample (..)
    )

import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Ogmios.Data.Metrics as Metrics

-- | Convert `Health` status and metrics into Prometheus format builder
mkPrometheusMetrics :: Health block -> Builder
mkPrometheusMetrics Health{..} =
    prometheusMetrics
    & Map.fromList
    & Map.mapKeys (\k -> (MetricId (makeName $ "ogmios_" <> k) (Labels mempty)))
    & RegistrySample
    & encodeMetrics
  where
    Metrics{..} = metrics
    RuntimeStats{..} = runtimeStats

    mkGauge :: Double -> MetricSample
    mkGauge = GaugeMetricSample . GaugeSample

    mkCounter :: Int -> MetricSample
    mkCounter = CounterMetricSample . CounterSample

    prometheusMetrics :: [(Text, MetricSample)]
    prometheusMetrics = mconcat
        [ [ ( "connected"
            , mkGauge $ case connectionStatus of
                Connected -> 1
                Disconnected -> 0
            )
          ]

        , [ ( "network_synchronization"
            , mkGauge $ Scientific.toRealFloat ns
            ) | Just (NetworkSynchronization ns) <- [networkSynchronization]
          ]

        , [ ( "current_epoch"
            , mkCounter $ fromEnum $ unEpochNo e
            ) | Just e <- [currentEpoch]
          ]

        , [ ( "slot_in_epoch"
            , mkCounter $ fromEnum $ getSlotInEpoch s
            ) | Just s <- [slotInEpoch]
          ]

        , [ ( "active_connections"
            , mkGauge $ fromInteger activeConnections
            )
          ]

        , [ ( "total_connections"
            , mkCounter $ fromInteger totalConnections
            )
          ]

        , [ ( "total_messages"
            , mkCounter $ fromInteger totalMessages
            )
          ]

        , [ ( "total_unrouted"
            , mkCounter $ fromInteger totalUnrouted
            )
          ]

        , [ ( "max_heap_size"
            , mkGauge $ fromInteger maxHeapSize
            )
          ]

        , [ ( "current_heap_size"
            , mkGauge $ fromInteger currentHeapSize
            )
          ]

        , [ ( "cpu_time"
            , mkCounter $ fromInteger cpuTime
            )
          ]

        , [ ( "gc_cpu_time"
            , mkCounter $ fromInteger gcCpuTime
            )
          ]

        , [ ( "session_duration_mean"
            , mkGauge $ Metrics.mean sessionDurations
            )
          ]

        , [ ( "session_duration_min"
            , mkGauge $ Metrics.min sessionDurations
            )
          ]

        , [ ( "session_duration_max"
            , mkGauge $ Metrics.max sessionDurations
            )
          ]

        , case lastKnownTip of
            TipGenesis ->
                [ ( "tip_at_genesis"
                  , mkGauge 1
                  )
                ]
            Tip slot _hash block ->
                [ ( "tip_block"
                  , mkCounter $ fromEnum $ unBlockNo block
                  )
                , ( "tip_slot"
                  , mkCounter $ fromEnum $ unSlotNo slot
                  )
                ]
        ]
