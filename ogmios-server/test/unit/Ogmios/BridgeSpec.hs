-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.BridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Types.Json.Orphans
    ( QueryResult
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetFilteredUTxO
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    )
import Data.Aeson
    ( toJSON )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import Data.Proxy
    ( Proxy (..) )
import Data.SOP.Strict
    ( NS (..) )
import Ogmios.Bridge
    ( FindIntersectResponse (..)
    , QueryResponse (..)
    , RequestNextResponse (..)
    , SomeQuery (..)
    , SubmitTxResponse (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras, HardForkBlock (..), ShelleyEra )
import Ouroboros.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..), Mismatch (..), MismatchEraInfo (..), singleEraInfo )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Ledger.Ledger
    ( Query (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..), HeaderHash, Point (..), SlotNo (..), Tip (..) )
import Test.Hspec
    ( Spec, SpecWith, context, expectationFailure, specify )
import Test.Hspec.Json.Schema
    ( SchemaRef (..), prop_validateToJSON, validateToJSON )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , Gen
    , Positive (..)
    , Property
    , Result (..)
    , choose
    , counterexample
    , elements
    , forAllBlind
    , genericShrink
    , oneof
    , property
    , quickCheckWithResult
    , withMaxSuccess
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import Test.Consensus.Cardano.Generators
    ()

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T
import qualified Ouroboros.Network.Point as Point
import qualified Test.QuickCheck as QC

spec :: Spec
spec = do
    context "validate ToJSON instances against JSON-schema" $ do
        validateToJSON (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            "../ogmios.wsp.json#/properties/FindIntersectResponse"

        validateToJSON (arbitrary @(Wsp.Response (RequestNextResponse Block)))
            "../ogmios.wsp.json#/properties/RequestNextResponse"

--      validateToJSON (arbitrary @(SubmitTxResponse ApplyMempoolPayloadErr)))
--            "../ogmios.wsp.json#/properties/SubmitTxResponse"

    context "validate FromJSON instances of Local State Queries" $ do
        validateQuery [aesonQQ|"ledgerTip"|]
            (parseGetLedgerTip genPointResult)
            "../ogmios.wsp.json#/properties/QueryResponse[ledgerTip]"

        validateQuery [aesonQQ|"currentEpoch"|]
            (parseGetEpochNo genEpochResult)
            "../ogmios.wsp.json#/properties/QueryResponse[currentEpoch]"

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = Wsp.Response Nothing <$> arbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (RequestNextResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

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

instance Arbitrary Block where
    arbitrary = oneof
        [ BlockByron <$> arbitrary
        , BlockShelley <$> arbitrary
        ]

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> arbitrary

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> choose (1, 100000)

genBlockNo :: Gen BlockNo
genBlockNo = BlockNo <$> arbitrary

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = arbitrary

genPoint :: Gen (Point.Block SlotNo (HeaderHash Block))
genPoint = Point.Block <$> genSlotNo <*> genHeaderHash

--
-- Local State Queries
--

-- | Parse a given query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateQuery
    :: Json.Value
    -> (Json.Value -> Json.Parser (SomeQuery Gen Block))
    -> SchemaRef
    -> SpecWith ()
validateQuery json parser ref = specify (T.unpack $ getSchemaRef ref) $ do
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right (SomeQuery{genResult,encodeResult}) -> do
            let toResponse = Wsp.Response Nothing . QueryResponse . encodeResult
            let args = QC.stdArgs { chatty = False }
            let prop = withMaxSuccess 100 $ forAllBlind
                    (genResult Proxy)
                    (prop_validateToJSON (toJSON . toResponse) ref)
            quickCheckWithResult args prop >>= \case
                Success{} -> pure ()
                Failure{output} -> expectationFailure output
                GaveUp{output} -> expectationFailure output
                NoExpectedFailure{output} -> expectationFailure output

type ShelleyBlock_ = ShelleyBlock (ShelleyEra StandardCrypto)

genMismatchEraInfo
    :: Gen (MismatchEraInfo (CardanoEras StandardCrypto))
genMismatchEraInfo = MismatchEraInfo <$> elements
    [ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))
    , MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)
    ]
  where
    eraInfoByron =
        singleEraInfo (Proxy @ByronBlock)
    eraInfoShelley =
        singleEraInfo (Proxy @(ShelleyBlock (ShelleyEra StandardCrypto)))

genPointResult
    :: Proxy (QueryResult StandardCrypto (Point ShelleyBlock_))
    -> Gen (QueryResult StandardCrypto (Point ShelleyBlock_))
genPointResult _ = oneof
    [ Left <$> genMismatchEraInfo
    , Right <$> arbitrary
    ]

genEpochResult
    :: Proxy (QueryResult StandardCrypto EpochNo)
    -> Gen (QueryResult StandardCrypto EpochNo)
genEpochResult _ = oneof
    [ Left <$> genMismatchEraInfo
    , Right <$> genEpochNo
    ]
