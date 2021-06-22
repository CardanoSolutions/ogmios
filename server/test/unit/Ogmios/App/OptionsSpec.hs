-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.App.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios.App.Options
    ( EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , mkSystemStart
    , parseNetworkParameters
    )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )

import qualified Paths_ogmios

spec :: Spec
spec = parallel $ do
    context "parseNetworkParameters" $ do
        specify "mainnet" $ do
            params <- parseNetworkParameters =<< getConfigFile "mainnet"
            networkMagic  params `shouldBe` NetworkMagic 764824073
            systemStart   params `shouldBe` mkSystemStart 1506203091
            slotsPerEpoch params `shouldBe` EpochSlots 432000

        specify "testnet" $ do
            params <- parseNetworkParameters =<< getConfigFile "testnet"
            networkMagic  params `shouldBe` NetworkMagic 1097911063
            systemStart   params `shouldBe` mkSystemStart 1563999616
            slotsPerEpoch params `shouldBe` EpochSlots 432000

--
-- Helper
--

getConfigFile :: String -> IO FilePath
getConfigFile network =
    "config/network/" <> network <> "/cardano-node/config.json"
    & Paths_ogmios.getDataFileName
