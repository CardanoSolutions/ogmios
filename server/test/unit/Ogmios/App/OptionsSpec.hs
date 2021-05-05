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
    , SystemStart (..)
    , defaultSlotsPerEpoch
    , mainnetNetworkParameters
    , mkSystemStart
    , parseNetworkParameters
    , stagingNetworkParameters
    , testnetNetworkParameters
    )

import Test.Hspec
    ( Spec, context, parallel )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , elements
    , forAll
    , property
    , (===)
    )

spec :: Spec
spec = parallel $ do
    context "parseNetworkParameters" $ do
        parallel $ prop "Parse any combination of 2 colon-separated naturals"
            prop_any2ParseNetworkParameters

        parallel $ prop "Parse any combination of 3 colon-separated naturals"
            prop_any3ParseNetworkParameters

        parallel $ prop  "Parse well-known networks strings"
            prop_wellKnownParseNetworkParameters

prop_any3ParseNetworkParameters :: Property
prop_any3ParseNetworkParameters =
    forAll genNetworkParameters $ \((a, magic), (b, start), (c, slots)) -> do
        case parseNetworkParameters (intercalate ":" [a,b,c]) of
            Nothing ->
                property False
            Just params -> conjoin
                [ networkMagic params === magic
                , systemStart params === start
                , slotsPerEpoch params === slots
                ]
  where
    genNetworkParameters = (,,)
        <$> genNetworkMagic
        <*> genSystemStart
        <*> genSlotsPerEpoch

prop_any2ParseNetworkParameters :: Property
prop_any2ParseNetworkParameters =
    forAll genNetworkParameters $ \((a, magic), (b, start)) -> do
        case parseNetworkParameters (intercalate ":" [a,b]) of
            Nothing ->
                property False
            Just params -> conjoin
                [ networkMagic params === magic
                , systemStart params === start
                , slotsPerEpoch params === defaultSlotsPerEpoch
                ]
  where
    genNetworkParameters = (,)
        <$> genNetworkMagic
        <*> genSystemStart


prop_wellKnownParseNetworkParameters :: Property
prop_wellKnownParseNetworkParameters =
    forAll genNetworkParameters $ \(str, params) -> do
        parseNetworkParameters str === Just params
  where
    genNetworkParameters = elements
        [ ( "mainnet", mainnetNetworkParameters )
        , ( "testnet", testnetNetworkParameters )
        , ( "staging", stagingNetworkParameters )
        ]

--
-- Generators
--

genNetworkMagic :: Gen (String, NetworkMagic)
genNetworkMagic = do
    m <- arbitrary
    pure (show m, NetworkMagic m)

genSystemStart :: Gen (String, SystemStart)
genSystemStart = do
    t <- choose (0, fromIntegral (maxBound :: Word32))
    pure (show t, mkSystemStart t)

genSlotsPerEpoch :: Gen (String, EpochSlots)
genSlotsPerEpoch = do
    s <- choose (0, 100000)
    pure (show s, EpochSlots s)
