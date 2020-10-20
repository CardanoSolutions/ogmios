-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.BridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Ogmios.Bridge
    ( FindIntersectResponse (..)
    , RequestNextResponse (..)
    , SubmitTxResponse (..)
    )
import Test.Hspec
    ( Spec, SpecWith, describe, it )
import Test.Hspec.Json.Schema
    ( validateToJSON )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Positive (..)
    , Property
    , choose
    , counterexample
    , genericShrink
    , oneof
    , property
    , withMaxSuccess
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import Cardano.Types.Json.Orphans
    ()

-- import Test.Consensus.Shelley.Generators
--     ()

import qualified Codec.Json.Wsp.Handler as Wsp

import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..), HeaderHash, Point (..), SlotNo (..), Tip (..) )

import Test.Consensus.Cardano.Generators
    ()

import qualified Ouroboros.Network.Point as Point

spec :: Spec
spec =
    describe "validate ToJSON instances against JSON-schema" $ do
        validateToJSON (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            "../ogmios.wsp.json#/properties/FindIntersectResponse"

--        test (validateAllToJSON @(RequestNextResponse ByronBlock))
--            "ogmios.wsp.json#/properties/RequestNextResponse"
--        test (validateAllToJSON @(SubmitTxResponse ApplyMempoolPayloadErr))
--            "ogmios.wsp.json#/properties/SubmitTxResponse"

--
-- Instances
--

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = Wsp.Response Nothing <$> arbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

-- instance Arbitrary (RequestNextResponse ByronBlock) where
--     shrink = genericShrink
--     arbitrary = genericArbitrary
--
-- instance Arbitrary (SubmitTxResponse ApplyMempoolPayloadErr) where
--     arbitrary = oneof
--         [ pure (SubmitTxResponse SubmitSuccess)
--         , SubmitTxResponse . SubmitFail. MempoolTxErr <$> arbitrary
--         ]

-- instance Arbitrary UTxOValidationError where
--     arbitrary = hedgehog genUTxOValidationError

instance Arbitrary (Point Block) where
    arbitrary = oneof
        [ pure (Point Point.Origin)
        , Point . Point.At <$> genPoint
        ]

instance Arbitrary (Tip Block) where
    arbitrary = oneof
        [ pure TipGenesis
        , Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo
        ]

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> choose (1, 100000)

genBlockNo :: Gen BlockNo
genBlockNo = BlockNo <$> arbitrary

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = arbitrary

genPoint :: Gen (Point.Block SlotNo (HeaderHash Block))
genPoint = Point.Block <$> genSlotNo <*> genHeaderHash
