-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.JsonSpec
    ( spec
    ) where

import Relude

import Cardano.Network.Protocol.NodeToClient
    ( Block, SubmitTxError (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Control.Monad
    ( void, (>=>) )
import Data.Aeson
    ( parseJSON, toJSON )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.SOP.Strict
    ( NS (..) )
import Ogmios.Data.Json
    ( Json
    , encodeAcquireFailure
    , encodeBlock
    , encodeHardForkApplyTxErr
    , encodePoint
    , encodeTip
    , jsonToByteString
    )
import Ogmios.Data.Json.Query
    ( QueryResult
    , SomeQuery (..)
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersectResponse (..)
    , RequestNextResponse (..)
    , _encodeFindIntersectResponse
    , _encodeRequestNextResponse
    )
import Ogmios.Data.Protocol.StateQuery
    ( QueryResponse (..), _encodeQueryResponse )
import Ogmios.Data.Protocol.TxSubmission
    ( SubmitTxResponse (..), _encodeSubmitTxResponse )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , CardanoEras
    , GenTx
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
import Ouroboros.Consensus.Shelley.Ledger.Query
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
    ( Spec
    , SpecWith
    , context
    , expectationFailure
    , it
    , parallel
    , shouldBe
    , specify
    )
import Test.Hspec.Json.Schema
    ( SchemaRef (..), prop_validateToJSON )
import Test.Hspec.QuickCheck
    ( prop )
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
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators
    ( genPParams )

import Test.Consensus.Cardano.Generators
    ()

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Ouroboros.Network.Point as Point

type ApplyTxErr = HardForkApplyTxErr (CardanoEras StandardCrypto)

queryRef :: SchemaRef
queryRef = "ogmios.wsp.json#/properties/Query/properties/args/properties/query"

jsonifierToAeson
    :: Json
    -> Json.Value
jsonifierToAeson =
    fromJust . Json.decodeStrict . jsonToByteString

-- | Generate arbitrary value of a data-type and verify they match a given
-- schema.
validateToJSON
    :: Gen a
    -> (a -> Json)
    -> SchemaRef
    -> SpecWith ()
validateToJSON arbitrary encode ref
    = parallel
    $ it (toString $ getSchemaRef ref)
    $ withMaxSuccess 100
    $ forAllBlind arbitrary (prop_validateToJSON (jsonifierToAeson . encode) ref)

spec :: Spec
spec = do
    context "validate chain-sync req/res against JSON-schema" $ do
        validateToJSON
            (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            (_encodeFindIntersectResponse encodePoint encodeTip)
            "ogmios.wsp.json#/properties/FindIntersectResponse"

        validateToJSON
            (arbitrary @(Wsp.Response (RequestNextResponse Block)))
            (_encodeRequestNextResponse encodeBlock encodePoint encodeTip)
            "ogmios.wsp.json#/properties/RequestNextResponse"

    context "validate tx submission req/res against JSON-schema" $ do
        prop "deserialise signed transactions" prop_parseSubmitTx

        validateToJSON
            (arbitrary @(Wsp.Response (SubmitTxResponse Block)))
            (_encodeSubmitTxResponse (Proxy @Block) encodeHardForkApplyTxErr)
            "ogmios.wsp.json#/properties/SubmitTxResponse"

    context "validate local state queries against JSON-schema" $ do
        validateQuery
            [aesonQQ|"ledgerTip"|]
            ( parseGetLedgerTip genPointResult
            ) "ogmios.wsp.json#/properties/QueryResponse[ledgerTip]"

        validateQuery
            [aesonQQ|"currentEpoch"|]
            ( parseGetEpochNo genEpochResult
            ) "ogmios.wsp.json#/properties/QueryResponse[currentEpoch]"

        validateQuery
            [aesonQQ|{ "nonMyopicMemberRewards": [14, 42] }|]
            ( parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult
            ) "ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|
            { "nonMyopicMemberRewards":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                ]
            }|]
            ( parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult
            ) "ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|"currentProtocolParameters"|]
            ( parseGetCurrentPParams genPParamsResult
            ) "ogmios.wsp.json#/properties/QueryResponse[currentProtocolParameters]"

        validateQuery
            [aesonQQ|"proposedProtocolParameters"|]
            ( parseGetProposedPParamsUpdates genProposedPParamsResult
            ) "ogmios.wsp.json#/properties/QueryResponse[proposedProtocolParameters]"

        validateQuery
            [aesonQQ|"stakeDistribution"|]
            ( parseGetStakeDistribution genPoolDistrResult
            ) "ogmios.wsp.json#/properties/QueryResponse[stakeDistribution]"

        validateQuery
            [aesonQQ|"utxo"|]
            ( parseGetUTxO genUTxOResult
            ) "ogmios.wsp.json#/properties/QueryResponse[utxo]"

-- TODO: re-enabled 'parseGetFilteredUTxO'
--        validateQuery
--            [aesonQQ|
--            { "utxo": []
--            }|]
--            (parseGetFilteredUTxO genUTxOResult)
--            "ogmios.wsp.json#/properties/QueryResponse[utxo]"
--
--        validateQuery
--            [aesonQQ|
--            { "utxo":
--                [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
--                , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
--                ]
--            }|]
--            (parseGetFilteredUTxO genUTxOResult)
--            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = Wsp.Response Nothing <$> arbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (RequestNextResponse Block) where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto))) where
    arbitrary = frequency
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
-- Local Tx Submit
--

newtype SerializedTx = SerializedTx Text deriving (Eq, Show)

prop_parseSubmitTx
    :: SerializedTx
    -> Property
prop_parseSubmitTx (SerializedTx bytes) =
    void result === Right ()
  where
    result = Json.parseEither (parseJSON @(GenTx Block)) (Json.String bytes)

instance Arbitrary SerializedTx where
    arbitrary = SerializedTx <$> elements
        -- base16, with CBOR-in-CBOR prefix.
        [ "D81859011F\
          \83a400818258200000000000000000000000000000000000000000000000000000\
          \000000000000000182825839010101010101010101010101010101010101010101\
          \010101010101010101010101010101010101010101010101010101010101010101\
          \0101011a001e848082583901020202020202020202020202020202020202020202\
          \020202020202020202020202020202020202020202020202020202020202020202\
          \02021a0078175c021a0001faa403191e46a1008182582001000000000000000000\
          \000000000000000000000000000000000000000000005840d7af60ae33d2af3514\
          \11c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9\
          \bd52076f2bb36bf38dfe034a9f04658e9f56197ab80ff6"

        -- base64, not wrapped.
        , "g6QAgoJYIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIJYIAAAAAAA\
          \AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQGDglg5AQICAgICAgICAgICAgIC\
          \AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICGgBbjYCC\
          \WDkBAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMD\
          \AwMDAwMDAwMDAwMaAFuNgIJYOQEEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQE\
          \BAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBBoAeAHgAhoAAhAgAxkeRqEAgoJY\
          \IBMK6CIB1wcub7/AoYhPtUY2VU0UlFt5kSXPfOONR39RWEBYNf94xvxeRGahecpl\
          \n6hcmbij+6CD8/P0K6Ng1HnGTvFpkUtSreSbGacgj9Y6bmehnEBrSCZgj9xTBwJV\
          \BsMHglggAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABYQOjnaezQ88U4\
          \8KWldKHIgXdfCG1vTIRbgb6beJVXKL/6fvpUKXxqXXMze9YoAgWxdZwT951Mk/KY\
          \cfxRt4rrqA72"

        -- base16, with bootstrap witnesses
        , "83a4008182582000000000000000000000000000000000000000000000000000\
          \0000000000000000018282583901010101010101010101010101010101010101\
          \0101010101010101010101010101010101010101010101010101010101010101\
          \0101010101011a001e8480825839010202020202020202020202020202020202\
          \0202020202020202020202020202020202020202020202020202020202020202\
          \020202020202021a0078175c021a0001faa403191e46a1028184582001000000\
          \000000000000000000000000000000000000000000000000000000005840d7af\
          \60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884\
          \023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80f5820\
          \0000000000000000000000000000000000000000000000000000000000000000\
          \41a0f6"

        -- base64, with bootstrap witnesses
        , "g6QAgoJYIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIJYIAAAAAAA\
          \AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQGDglg5AQICAgICAgICAgICAgIC\
          \AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICGgBbjYCC\
          \WDkBAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMD\
          \AwMDAwMDAwMDAwMaAFuNgIJYOQEEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQE\
          \BAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBBoAeAHgAhoAAhAgAxkeRqECgoRY\
          \IAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAWEDo52ns0PPFOPClpXSh\
          \yIF3Xwhtb0yEW4G+m3iVVyi/+n76VCl8al1zM3vWKAIFsXWcE/edTJPymHH8UbeK\
          \66gOWCAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEGghFggEwroIgHX\
          \By5vv8ChiE+1RjZVTRSUW3mRJc98441Hf1FYQFg1/3jG/F5EZqF5ymWfqFyZuKP7\
          \oIPz8/Qro2DUecZO8WmRS1Kt5JsZpyCP1jpuZ6GcQGtIJmCP3FMHAlUGwwdYIAEB\
          \AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBQaD2"
        ]

--
-- Local State Query
--

-- | Parse a given query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateQuery
    :: Json.Value
    -> (Json.Value -> Json.Parser (SomeQuery Gen Block))
    -> SchemaRef
    -> SpecWith ()
validateQuery json parser resultRef = specify (toString $ getSchemaRef resultRef) $ do
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON id queryRef json
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right (SomeQuery{genResult,encodeResult}) -> do
            let toResponse = Wsp.Response Nothing . QueryResponse . encodeResult
            let encode = jsonifierToAeson . _encodeQueryResponse encodeAcquireFailure . toResponse
            runQuickCheck $ withMaxSuccess 100 $ forAllBlind
                (genResult Proxy)
                (prop_validateToJSON encode resultRef)

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
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ AllegraEra crypto)
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
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (NonMyopicMemberRewards crypto))
    -> Gen (QueryResult crypto (NonMyopicMemberRewards crypto))
genNonMyopicMemberRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ AllegraEra crypto)
    => Proxy (QueryResult crypto (PParams era))
    -> Gen (QueryResult crypto (PParams era))
genPParamsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genPParams (Proxy @era))
    ]

genProposedPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ AllegraEra crypto)
    => Proxy (QueryResult crypto (ProposedPPUpdates era))
    -> Gen (QueryResult crypto (ProposedPPUpdates era))
genProposedPParamsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> arbitrary)
    ]

genPoolDistrResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (PoolDistr crypto))
    -> Gen (QueryResult crypto (PoolDistr crypto))
genPoolDistrResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> scale (`mod` 10) arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, era ~ AllegraEra crypto)
    => Proxy (QueryResult crypto (UTxO era))
    -> Gen (QueryResult crypto (UTxO era))
genUTxOResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> scale (`mod` 10) arbitrary)
    ]
