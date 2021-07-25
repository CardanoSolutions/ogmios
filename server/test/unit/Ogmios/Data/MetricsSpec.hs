-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.MetricsSpec
    ( spec
    ) where

import Ogmios.Prelude hiding
    ( max, min )

import Data.Aeson
    ( ToJSON (..) )
import Ogmios.Data.Metrics
    ( DistributionStats (..)
    , Metrics (..)
    , RuntimeStats (..)
    , emptyDistributionStats
    , emptyMetrics
    , emptyRuntimeStats
    )
import Test.Hspec
    ( Expectation, Spec, SpecWith, context, parallel, shouldBe, specify )

spec :: Spec
spec = parallel $ do
    context "empty smart-constructors" $ do
        cover "Metrics" emptyMetrics $ \metrics -> do
            runtimeStats metrics `shouldBe` emptyRuntimeStats
            activeConnections metrics `shouldBe` 0
            totalConnections metrics `shouldBe` 0
            sessionDurations metrics `shouldBe` emptyDistributionStats
            totalMessages metrics `shouldBe` 0
            totalUnrouted metrics `shouldBe` 0

        cover "RuntimeStats" emptyRuntimeStats $ \stats -> do
            maxHeapSize stats `shouldBe` 0
            currentHeapSize stats `shouldBe` 0
            cpuTime stats `shouldBe` 0
            gcCpuTime stats `shouldBe` 0

        cover "DistributionStats" emptyDistributionStats $ \stats -> do
            mean stats `shouldBe` 0
            max stats `shouldBe` 0
            min stats `shouldBe` 0

--
-- Helpers
--

cover :: (Eq a, Show a, ToJSON a) => String -> a -> (a -> Expectation) -> SpecWith ()
cover lbl zero predicate =
    specify (lbl <> " / " <> show zero <> " / " <> show (toJSON zero)) $
        predicate zero >> shouldBe zero zero
