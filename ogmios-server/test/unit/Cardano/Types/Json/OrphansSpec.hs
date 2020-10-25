-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Types.Json.OrphansSpec
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
import Control.Monad
    ( (>=>) )
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
    ( CardanoEras
    , HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra)
    , HardForkBlock (..)
    , ShelleyEra
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..), Mismatch (..), MismatchEraInfo (..), singleEraInfo )
import Ouroboros.Consensus.HardFork.Combinator.Mempool
    ( HardForkApplyTxErr (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Ledger.Ledger
    ( NonMyopicMemberRewards (..), Query (..) )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( BlockNo (..), HeaderHash, Point (..), SlotNo (..), Tip (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )
import Shelley.Spec.Ledger.Delegation.Certificates
    ( PoolDistr )
import Shelley.Spec.Ledger.PParams
    ( PParams, ProposedPPUpdates )
import Shelley.Spec.Ledger.UTxO
    ( UTxO )
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
    , frequency
    , genericShrink
    , oneof
    , property
    , quickCheckResult
    , scale
    , withMaxSuccess
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Shelley.Spec.Ledger.Serialisation.Generators
    ( genPParams )

import Test.Consensus.Cardano.Generators
    ()

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T
import qualified Ouroboros.Network.Point as Point

type ApplyTxErr = HardForkApplyTxErr (CardanoEras StandardCrypto)

queryRef :: SchemaRef
queryRef = "../ogmios.wsp.json#/properties/Query/properties/args/properties/query"

spec :: Spec
spec = do
    context "validate chain-sync req/res against JSON-schema" $ do
        validateToJSON (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            "../ogmios.wsp.json#/properties/FindIntersectResponse"

        validateToJSON (arbitrary @(Wsp.Response (RequestNextResponse Block)))
            "../ogmios.wsp.json#/properties/RequestNextResponse"

    context "validate tx submission req/res against JSON-schema" $ do
        validateToJSON (arbitrary @(Wsp.Response (SubmitTxResponse ApplyTxErr)))
            "../ogmios.wsp.json#/properties/SubmitTxResponse"

    context "validate local state queries against JSON-schema" $ do
        validateQuery
            [aesonQQ|"ledgerTip"|]
            (parseGetLedgerTip genPointResult)
            "../ogmios.wsp.json#/properties/QueryResponse[ledgerTip]"

        validateQuery
            [aesonQQ|"currentEpoch"|]
            (parseGetEpochNo genEpochResult)
            "../ogmios.wsp.json#/properties/QueryResponse[currentEpoch]"

        validateQuery
            [aesonQQ|{ "nonMyopicMemberRewards": [14, 42] }|]
            (parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult)
            "../ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|
            { "nonMyopicMemberRewards":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                ]
            }|]
            (parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult)
            "../ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|"currentProtocolParameters"|]
            (parseGetCurrentPParams genPParamsResult)
            "../ogmios.wsp.json#/properties/QueryResponse[currentProtocolParameters]"

        validateQuery
            [aesonQQ|"proposedProtocolParameters"|]
            (parseGetProposedPParamsUpdates genProposedPParamsResult)
            "../ogmios.wsp.json#/properties/QueryResponse[proposedProtocolParameters]"

        validateQuery
            [aesonQQ|"stakeDistribution"|]
            (parseGetStakeDistribution genPoolDistrResult)
            "../ogmios.wsp.json#/properties/QueryResponse[stakeDistribution]"

        validateQuery
            [aesonQQ|"utxo"|]
            (parseGetUTxO genUTxOResult)
            "../ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo": []
            }|]
            (parseGetFilteredUTxO genUTxOResult)
            "../ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo":
                [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
                , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
                ]
            }|]
            (parseGetFilteredUTxO genUTxOResult)
            "../ogmios.wsp.json#/properties/QueryResponse[utxo]"

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = Wsp.Response Nothing <$> arbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (RequestNextResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (SubmitTxResponse ApplyTxErr) where
    arbitrary = SubmitTxResponse <$> frequency
        [ ( 1, pure SubmitSuccess)
        , ( 1, SubmitFail . HardForkApplyTxErrWrongEra <$> genMismatchEraInfo)
        , (10, SubmitFail . ApplyTxErrShelley <$> scale (`mod` 10) arbitrary)
        ]

instance Arbitrary (Point Block) where
    arbitrary = frequency
        [ (1, pure (Point Point.Origin))
        , (10, Point . Point.At <$> genPoint)
        ]

instance Arbitrary (Tip Block) where
    arbitrary = frequency
        [ (1, pure TipGenesis)
        , (10, Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo)
        ]

instance Arbitrary Block where
    arbitrary = scale (`mod` 10) $ oneof
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
validateQuery json parser resultRef = specify (T.unpack $ getSchemaRef resultRef) $ do
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON id queryRef json
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right (SomeQuery{genResult,encodeResult}) -> do
            let toResponse = Wsp.Response Nothing . QueryResponse . encodeResult
            runQuickCheck $ withMaxSuccess 100 $ forAllBlind
                (genResult Proxy)
                (prop_validateToJSON (toJSON . toResponse) resultRef)

-- | Simple run a QuickCheck property
runQuickCheck :: Property -> IO ()
runQuickCheck = quickCheckResult >=> \case
    Success{} -> pure ()
    Failure{output} -> expectationFailure output
    GaveUp{output} -> expectationFailure output
    NoExpectedFailure{output} -> expectationFailure output

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
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (Point (ShelleyBlock era)))
    -> Gen (QueryResult crypto (Point (ShelleyBlock era)))
genPointResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genEpochResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto EpochNo)
    -> Gen (QueryResult crypto EpochNo)
genEpochResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genEpochNo)
    ]

genNonMyopicMemberRewardsResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (NonMyopicMemberRewards era))
    -> Gen (QueryResult crypto (NonMyopicMemberRewards era))
genNonMyopicMemberRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (PParams era))
    -> Gen (QueryResult crypto (PParams era))
genPParamsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genPParams (Proxy @era))
    ]

genProposedPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (ProposedPPUpdates era))
    -> Gen (QueryResult crypto (ProposedPPUpdates era))
genProposedPParamsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genPoolDistrResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (PoolDistr era))
    -> Gen (QueryResult crypto (PoolDistr era))
genPoolDistrResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> scale (`mod` 10) arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ ShelleyEra crypto)
    => Proxy (QueryResult crypto (UTxO era))
    -> Gen (QueryResult crypto (UTxO era))
genUTxOResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> scale (`mod` 10) arbitrary)
    ]
