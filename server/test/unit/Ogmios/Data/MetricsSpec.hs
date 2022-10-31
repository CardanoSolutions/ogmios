-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.MetricsSpec
    ( spec
    ) where

import Ogmios.Prelude hiding
    ( max
    , min
    )

import Ogmios.Data.Metrics
    ( DistributionStats (..)
    , Metrics (..)
    , RuntimeStats (..)
    , emptyDistributionStats
    , emptyMetrics
    , emptyRuntimeStats
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , shouldBe
    )
import Test.Instances.Util
    ( eqShowJson
    )

spec :: Spec
spec = parallel $ do
    context "Eq/Show/Json" $ do
        eqShowJson "Metrics" emptyMetrics $ \metrics -> do
            runtimeStats metrics `shouldBe` emptyRuntimeStats
            activeConnections metrics `shouldBe` 0
            totalConnections metrics `shouldBe` 0
            sessionDurations metrics `shouldBe` emptyDistributionStats
            totalMessages metrics `shouldBe` 0
            totalUnrouted metrics `shouldBe` 0

        eqShowJson "RuntimeStats" emptyRuntimeStats $ \stats -> do
            maxHeapSize stats `shouldBe` 0
            currentHeapSize stats `shouldBe` 0
            cpuTime stats `shouldBe` 0
            gcCpuTime stats `shouldBe` 0

        eqShowJson "DistributionStats" emptyDistributionStats $ \stats -> do
            mean stats `shouldBe` 0
            max stats `shouldBe` 0
            min stats `shouldBe` 0
