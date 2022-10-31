-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.HealthSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios
    ()

import Cardano.Network.Protocol.NodeToClient
    ( Block
    )
import Data.Time.Clock
    ( UTCTime (..)
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import Ogmios.Data.Health
    ( ConnectionStatus (..)
    , Health (..)
    , NetworkSynchronization (..)
    , RelativeTime (..)
    , SystemStart (..)
    , Tip (..)
    , emptyHealth
    , mkNetworkSynchronization
    )
import Ogmios.Data.Metrics
    ( emptyMetrics
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    , runIO
    , shouldBe
    , specify
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Instances.Util
    ( eqShowJson
    )
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , disjoin
    , expectFailure
    , forAll
    , property
    , (===)
    , (==>)
    )

import qualified Data.Aeson as Json

spec :: Spec
spec = parallel $ do
    context "Eq/Show/Json" $ do
        time <- runIO getCurrentTime
        eqShowJson "Health" (emptyHealth time) $ \(health :: Health Block) -> do
            startTime health `shouldBe` time
            lastKnownTip health `shouldBe` TipGenesis
            lastTipUpdate health `shouldBe` Nothing
            networkSynchronization health `shouldBe` Nothing
            currentEra health `shouldBe` Nothing
            metrics health `shouldBe` emptyMetrics
            connectionStatus health `shouldBe` Disconnected
            currentEpoch health `shouldBe` Nothing
            slotInEpoch health `shouldBe` Nothing

    context "NetworkSynchronization" $ do
        let matrix =
                [ ( NetworkSynchronization 1,        "1.00000" )
                , ( NetworkSynchronization 0.1,      "0.10000" )
                , ( NetworkSynchronization 0.01,     "0.01000" )
                , ( NetworkSynchronization 0.001,    "0.00100" )
                , ( NetworkSynchronization 0.0001,   "0.00010" )
                , ( NetworkSynchronization 0.00001,  "0.00001" )
                , ( NetworkSynchronization 0.000001, "0.00000" )
                ]
        context "is encoded as plain decimals, no scientific notation" $ do
            forM_ matrix $ \(networkSync, expectation) ->
                let title = show networkSync <> " -> " <> show expectation in
                specify title $ Json.encode networkSync `shouldBe` expectation
        context "properties" $ do
            prop "not always equal to 1"
                (expectFailure prop_alwaysEqualTo1)
            prop "always between 0 and 1"
                prop_boundedNetworkSynchronization
            prop "increasing relative time increases synchronization"
                prop_monotonicallyIncreasingRelativeTime
            prop "is precise with a margin of 60s, rounded up"
                prop_isPreciseWithMargin

prop_boundedNetworkSynchronization
    :: Property
prop_boundedNetworkSynchronization =
    forAll genSystemStart $ \start ->
    forAll (genCurrentTime (getSystemStart start)) $ \now  ->
    forAll genRelativeTime $ \rel ->
        let sync = mkNetworkSynchronization start now rel
         in counterexample (show sync)
          $ sync >= NetworkSynchronization 0
         && sync <= NetworkSynchronization 1

prop_alwaysEqualTo1
    :: Property
prop_alwaysEqualTo1 =
    forAll genSystemStart $ \start ->
    forAll (genCurrentTime (getSystemStart start)) $ \now  ->
    forAll genRelativeTime $ \rel ->
        mkNetworkSynchronization start now rel === NetworkSynchronization 1

prop_monotonicallyIncreasingRelativeTime
    :: Property
prop_monotonicallyIncreasingRelativeTime =
    forAll genSystemStart  $ \start ->
    forAll genRelativeTime $ \(RelativeTime rel) -> rel /= 0 ==>
        let now = addUTCTime (3 * rel) (getSystemStart start)
         in disjoin
            [ property $
                mkNetworkSynchronization start now (RelativeTime rel)
                  <
                mkNetworkSynchronization start now (RelativeTime $ 2 * rel)

            , mkNetworkSynchronization start now (RelativeTime rel)
                ===
              NetworkSynchronization 1
            ]

prop_isPreciseWithMargin
    :: Property
prop_isPreciseWithMargin =
    forAll genSystemStart $ \systemStart ->
    forAll (genCurrentTime (getSystemStart systemStart)) $ \currentTime ->
    forAll (choose (1, 60)) $ \δ ->
        let
            relativeTime = round $ currentTime `diffUTCTime` getSystemStart systemStart
            mkRelativeTime = RelativeTime . fromInteger . (* 1_000_000_000_000)
            sync = mkNetworkSynchronization systemStart currentTime . mkRelativeTime
          in
            sync relativeTime === sync (relativeTime - δ)

--
-- Generators
--

genSystemStart
    :: Gen SystemStart
genSystemStart =
    fmap SystemStart (UTCTime <$> genDay <*> genDaytime)
  where
    genDay = toEnum <$> choose (50000, 60000)
    genDaytime = toEnum <$> choose (0, 86400)

genCurrentTime
    :: UTCTime
    -> Gen UTCTime
genCurrentTime inf = do
    RelativeTime offset <- genRelativeTime
    pure $ addUTCTime offset inf

genRelativeTime
    :: Gen RelativeTime
genRelativeTime = do
    RelativeTime . fromIntegral <$> choose @Integer (0, 31536000)
