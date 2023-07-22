-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.JsonSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Era
    ( Era
    )
import Cardano.Network.Protocol.NodeToClient
    ( Block
    , GenTxId
    )
import Cardano.Slotting.Time
    ( mkSlotLength
    )
import Control.Monad.Class.MonadAsync
    ( forConcurrently_
    )
import Data.Aeson
    ( parseJSON
    , toJSON
    , (.=)
    )
import Data.Aeson.QQ.Simple
    ( aesonQQ
    )
import Data.Maybe
    ( fromJust
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO (..)
    , Upgrade (..)
    )
import Ogmios.Data.Json
    ( Json
    , decodeUtxo
    , decodeWith
    , encodeAcquireExpired
    , encodeBlock
    , encodeExUnits
    , encodePoint
    , encodeScriptFailure
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTranslationError
    , encodeTx
    , encodeTxId
    , encodeTxIn
    , inefficientEncodingToValue
    , jsonToByteString
    , stringifyRdmrPtr
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Json.Prelude
    ( encodeSlotLength
    )
import Ogmios.Data.Json.Query
    ( QueryInEra
    , ShelleyBasedEra (..)
    , SomeQuery (..)
    , SomeShelleyEra (..)
    , parseQueryLedgerEpoch
    , parseQueryLedgerEraStart
    , parseQueryLedgerEraSummaries
    , parseQueryLedgerLiveStakeDistribution
    , parseQueryLedgerProjectedRewards
    , parseQueryLedgerProposedProtocolParameters
    , parseQueryLedgerProtocolParameters
    , parseQueryLedgerRewardAccountSummaries
    , parseQueryLedgerRewardsProvenance
    , parseQueryLedgerStakePoolParameters
    , parseQueryLedgerStakePools
    , parseQueryLedgerTip
    , parseQueryLedgerUtxo
    , parseQueryLedgerUtxoByAddress
    , parseQueryLedgerUtxoByOutputReference
    , parseQueryNetworkBlockHeight
    , parseQueryNetworkGenesisConfiguration
    , parseQueryNetworkStartTime
    , parseQueryNetworkTip
    )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersection
    , FindIntersectionResponse (..)
    , NextBlock
    , NextBlockResponse (..)
    , _decodeFindIntersection
    , _decodeNextBlock
    , _encodeFindIntersection
    , _encodeFindIntersectionResponse
    , _encodeNextBlock
    , _encodeNextBlockResponse
    )
import Ogmios.Data.Protocol.StateQuery
    ( AcquireLedgerState
    , AcquireLedgerStateResponse (..)
    , QueryLedgerStateResponse (..)
    , ReleaseLedgerState
    , ReleaseLedgerStateResponse (..)
    , _decodeAcquireLedgerState
    , _decodeReleaseLedgerState
    , _encodeAcquireLedgerState
    , _encodeAcquireLedgerStateResponse
    , _encodeQueryLedgerStateResponse
    , _encodeReleaseLedgerState
    , _encodeReleaseLedgerStateResponse
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AcquireMempool
    , AcquireMempoolResponse (..)
    , HasTransaction
    , HasTransactionResponse (..)
    , MempoolSizeAndCapacity
    , NextTransaction
    , NextTransactionFields (..)
    , NextTransactionResponse (..)
    , ReleaseMempool
    , ReleaseMempoolResponse (..)
    , SizeOfMempool
    , SizeOfMempoolResponse (..)
    , _decodeAcquireMempool
    , _decodeHasTransaction
    , _decodeNextTransaction
    , _decodeReleaseMempool
    , _decodeSizeOfMempool
    , _encodeAcquireMempool
    , _encodeAcquireMempoolResponse
    , _encodeHasTransaction
    , _encodeHasTransactionResponse
    , _encodeNextTransaction
    , _encodeNextTransactionResponse
    , _encodeReleaseMempool
    , _encodeReleaseMempoolResponse
    , _encodeSizeOfMempool
    , _encodeSizeOfMempoolResponse
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTransactionResponse
    , SubmitTransactionResponse (..)
    , _encodeEvaluateTransactionResponse
    , _encodeSubmitTransactionResponse
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras
    , GenTx
    , HardForkApplyTxErr (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAlonzo
    )
import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..)
    )
import System.Directory
    ( createDirectoryIfMissing
    )
import Test.Generators
    ( genAcquireFailure
    , genBlock
    , genBlockNo
    , genBoundResult
    , genData
    , genDelegationAndRewardsResult
    , genEpochResult
    , genEvaluateTransactionResponse
    , genGenesisConfig
    , genHardForkApplyTxErr
    , genInterpreterResult
    , genMempoolSizeAndCapacity
    , genNonMyopicMemberRewardsResult
    , genPParamsResult
    , genPoint
    , genPointResultPraos
    , genPointResultTPraos
    , genPoolDistrResult
    , genPoolIdsResult
    , genPoolParametersResult
    , genProposedPParamsResult
    , genRewardsProvenanceResult
    , genSubmitResult
    , genSystemStart
    , genTip
    , genTipNoGenesis
    , genTx
    , genTxId
    , genUTxOResult
    , genUtxoBabbage
    , genWithOrigin
    , generateWith
    , reasonablySized
    , shrinkUtxo
    )
import Test.Hspec
    ( Spec
    , SpecWith
    , context
    , expectationFailure
    , parallel
    , runIO
    , shouldBe
    , shouldContain
    , specify
    )
import Test.Hspec.Json.Schema
    ( SchemaRef (..)
    , prop_validateToJSON
    , unsafeReadSchemaRef
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.Path.Util
    ( getProjectRoot
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , Gen
    , Property
    , Result (..)
    , conjoin
    , counterexample
    , elements
    , forAll
    , forAllBlind
    , forAllShrinkBlind
    , frequency
    , genericShrink
    , oneof
    , property
    , quickCheckWithResult
    , vectorOf
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )

import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage

import qualified Cardano.Ledger.Alonzo.Scripts.Data as Ledger
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.QuickCheck as QC

encodingToValue
    :: Json
    -> Json.Value
encodingToValue =
    fromJust . Json.decodeStrict . jsonToByteString

-- | Generate arbitrary value of a data-type and verify they match a given
-- schema.
validateToJSON
    :: Gen a
    -> (a -> Json)
    -> (Int, String)
    -> SchemaRef
    -> SpecWith ()
validateToJSON gen encode (n, vectorFilePath) ref = parallel $ do
    runIO $ generateTestVectors (n, vectorFilePath) gen encode
    refs <- runIO $ unsafeReadSchemaRef ref
    specify (toString $ getSchemaRef ref) $ withMaxSuccess n $ forAllBlind gen $
        prop_validateToJSON (encodingToValue . encode) refs

-- | Similar to 'validateToJSON', but also check that the produce value can be
-- decoded back to the expected form.
validateFromJSON
    :: (Eq a, Show a)
    => Gen a
    -> (a -> Json, Json.Value -> Json.Parser a)
    -> (Int, String)
    -> SchemaRef
    -> SpecWith ()
validateFromJSON gen (encode, decode) (n, vectorFilePath) ref = parallel $ do
    runIO $ generateTestVectors (n, vectorFilePath) gen encode
    refs <- runIO $ unsafeReadSchemaRef ref
    specify (toString $ getSchemaRef ref) $ withMaxSuccess n $ forAllBlind gen $ \a ->
        let leftSide = decodeWith decode (jsonToByteString (encode a)) in
        conjoin
        [ prop_validateToJSON (encodingToValue . encode) refs a
        , leftSide == Just a
            & counterexample
                (decodeUtf8 $ Json.encodePretty $ inefficientEncodingToValue $ encode a)
            & counterexample
                ("Got:  " <> show leftSide)
            & counterexample
                ("Want: " <> show (Just a))
        ]

spec :: Spec
spec = do
    context "JSON roundtrips" $ do
        prop "encodeUtxo / decodeUtxo (Babbage)" $ forAllShrinkBlind genUtxoBabbage shrinkUtxo $ \utxo ->
            let encoded = inefficientEncodingToValue (Babbage.encodeUtxo utxo) in
            case Json.parse decodeUtxo encoded of
                Json.Error e ->
                    property False
                        & counterexample e
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)
                Json.Success (UTxOInBabbageEra utxo') ->
                    utxo' === utxo
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)
                Json.Success (UTxOInConwayEra utxo') ->
                    utxo' === upgrade utxo
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)

        specify "Golden: Utxo_1.json" $ do
            json <- decodeFileThrow "Utxo_1.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e -> do
                    show e `shouldContain` "couldn't decode plutus script"
                    show e `shouldContain` "Please drop 'd8184c820249'"
                Json.Success UTxOInBabbageEra{} ->
                    fail "successfully decoded an invalid payload (as Babbage Utxo)?"
                Json.Success UTxOInConwayEra{} ->
                    fail "successfully decoded an invalid payload( as Conway Utxo)?"

        specify "Golden: Utxo_2.json" $ do
            json <- decodeFileThrow "Utxo_2.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e ->
                    fail (show e)
                Json.Success UTxOInBabbageEra{} ->
                    pure ()
                Json.Success _ ->
                    fail "successfully decoded Babbage Utxo as another era?"

        specify "Golden: Utxo_3.json" $ do
            json <- decodeFileThrow "Utxo_3.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e ->
                    fail (show e)
                Json.Success UTxOInBabbageEra{} ->
                    pure ()
                Json.Success _ ->
                    fail "successfully decoded Babbage Utxo as another era?"

        specify "Golden: Utxo_4.json" $ do
            json <- decodeFileThrow "Utxo_4.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e -> do
                    show e `shouldContain` "couldn't decode plutus script"
                    show e `shouldContain` "Please drop '820249'"
                Json.Success UTxOInBabbageEra{} ->
                    fail "successfully decoded an invalid payload( as Babbage Utxo)?"
                Json.Success UTxOInConwayEra{} ->
                    fail "successfully decoded an invalid payload (as Conway Utxo)?"

        context "Data / BinaryData" $ do
            prop "arbitrary" $
                forAll genData propBinaryDataRoundtrip

            prop "Golden (1)" $
                propBinaryDataRoundtrip $ unsafeDataFromBytes
                    "D8668219019E8201D8668219010182D866821903158140D8668219020C\
                    \83230505"

            prop "Golden (2)" $
                propBinaryDataRoundtrip $ unsafeDataFromBytes
                    "D8798441FFD87982D87982D87982D87981581CC279A3FB3B4E62BBC78E\
                    \288783B58045D4AE82A18867D8352D02775AD87981D87981D87981581C\
                    \121FD22E0B57AC206FEFC763F8BFA0771919F5218B40691EEA4514D0D8\
                    \7A80D87A801A002625A0D87983D879801A000F4240D879811A000FA92E"

    context "SlotLength" $ do
        let matrix = [ ( mkSlotLength 1, Json.double 1.0 )
                     , ( mkSlotLength 0.1, Json.double 0.1 )
                     ]
        forM_ matrix $ \(slotLength, json) ->
            specify (show slotLength <> " → " <> show json) $ do
                encodeSlotLength slotLength `shouldBe` json

    context "validate chain-sync request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request (FindIntersection Block)))
            (_encodeFindIntersection encodePoint, _decodeFindIntersection)
            (10, "FindIntersection")
            "ogmios.json#/properties/FindIntersection"

        validateToJSON
            (arbitrary @(Rpc.Response (FindIntersectionResponse Block)))
            (_encodeFindIntersectionResponse encodePoint encodeTip)
            (10, "FindIntersectionResponse")
            "ogmios.json#/properties/FindIntersectionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request NextBlock))
            (_encodeNextBlock, _decodeNextBlock)
            (3, "NextBlock")
            "ogmios.json#/properties/NextBlock"

        validateToJSON
            (arbitrary @(Rpc.Response (NextBlockResponse Block)))
            (_encodeNextBlockResponse encodeBlock encodePoint encodeTip)
            (200, "NextBlockResponse")
            "ogmios.json#/properties/NextBlockResponse"

    context "validate transaction submission request/response against JSON-schema" $ do
        prop "deserialise signed transactions" prop_parseSubmitTransaction

        validateToJSON
            (arbitrary @(Rpc.Response (SubmitTransactionResponse Block)))
            (_encodeSubmitTransactionResponse (Proxy @Block) encodeTxId encodeSubmitTransactionError)
            (200, "SubmitTransactionResponse")
            "ogmios.json#/properties/SubmitTransactionResponse"

        validateToJSON
            (arbitrary @(Rpc.Response (EvaluateTransactionResponse Block)))
            (_encodeEvaluateTransactionResponse (Proxy @Block) stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError)
            (100, "EvaluateTransactionResponse")
            "ogmios.json#/properties/EvaluateTransactionResponse"

    context "validate mempool monitoring request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request AcquireMempool))
            (_encodeAcquireMempool, _decodeAcquireMempool)
            (10, "AcquireMempool")
            "ogmios.json#/properties/AcquireMempool"

        validateToJSON
            (arbitrary @(Rpc.Response AcquireMempoolResponse))
            _encodeAcquireMempoolResponse
            (10, "AcquireMempoolResponse")
            "ogmios.json#/properties/AcquireMempoolResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request NextTransaction))
            (_encodeNextTransaction, _decodeNextTransaction)
            (10, "NextTransaction")
            "ogmios.json#/properties/NextTransaction"

        validateToJSON
            (arbitrary @(Rpc.Response (NextTransactionResponse Block)))
            (_encodeNextTransactionResponse encodeTxId encodeTx)
            (10, "NextTransactionResponse")
            "ogmios.json#/properties/NextTransactionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request (HasTransaction Block)))
            (_encodeHasTransaction encodeTxId, _decodeHasTransaction)
            (10, "HasTransaction")
            "ogmios.json#/properties/HasTransaction"

        validateToJSON
            (arbitrary @(Rpc.Response HasTransactionResponse))
            _encodeHasTransactionResponse
            (10, "HasTransactionResponse")
            "ogmios.json#/properties/HasTransactionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request SizeOfMempool))
            (_encodeSizeOfMempool, _decodeSizeOfMempool)
            (10, "SizeOfMempool")
            "ogmios.json#/properties/SizeOfMempool"

        validateToJSON
            (arbitrary @(Rpc.Response SizeOfMempoolResponse))
            _encodeSizeOfMempoolResponse
            (10, "SizeOfMempoolResponse")
            "ogmios.json#/properties/SizeOfMempoolResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request ReleaseMempool))
            (_encodeReleaseMempool, _decodeReleaseMempool)
            (10, "ReleaseMempoolRequest")
            "ogmios.json#/properties/ReleaseMempool"

        validateToJSON
            (arbitrary @(Rpc.Response ReleaseMempoolResponse))
            _encodeReleaseMempoolResponse
            (10, "ReleaseMempoolResponse")
            "ogmios.json#/properties/ReleaseMempoolResponse"

    context "validate acquire request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request (AcquireLedgerState Block)))
            (_encodeAcquireLedgerState encodePoint, _decodeAcquireLedgerState)
            (10, "AcquireLedgerStateRequest")
            "ogmios.json#/properties/AcquireLedgerState"

        validateToJSON
            (arbitrary @(Rpc.Response (AcquireLedgerStateResponse Block)))
            (_encodeAcquireLedgerStateResponse encodePoint encodeAcquireExpired)
            (10, "AcquireLedgerStateResponse")
            "ogmios.json#/properties/AcquireLedgerStateResponse"

    context "validate local state queries against JSON-schema" $ do
        validateLedgerStateQuery 3 "epoch"
            [aesonQQ|{}|]
            (parseQueryLedgerEpoch genEpochResult)

        validateLedgerStateQuery 10 "eraStart"
            [aesonQQ|{}|]
            (parseQueryLedgerEraStart genBoundResult)

        validateLedgerStateQuery 10 "eraSummaries"
            [aesonQQ|{}|]
            (parseQueryLedgerEraSummaries genInterpreterResult)

        validateLedgerStateQuery 10 "tip"
            [aesonQQ|{}|]
            (parseQueryLedgerTip genPointResultTPraos genPointResultPraos)

        validateLedgerStateQuery 10 "projectedRewards"
            [aesonQQ|{ "stake": [14, 42] }|]
            (parseQueryLedgerProjectedRewards genNonMyopicMemberRewardsResult)

        validateLedgerStateQuery 10 "projectedRewards"
            [aesonQQ|
            { "keys":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                , "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                ]
            , "scripts":
                [ "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                ]
            }|]
            (parseQueryLedgerProjectedRewards genNonMyopicMemberRewardsResult)

        validateLedgerStateQuery 10 "rewardAccountSummaries"
            [aesonQQ|
            { "keys":
                [ "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                ]
            , "scripts":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                , "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                ]
            }|]
            (parseQueryLedgerRewardAccountSummaries genDelegationAndRewardsResult)

        validateLedgerStateQuery 30 "protocolParameters"
            [aesonQQ|{}|]
            (parseQueryLedgerProtocolParameters genPParamsResult)

        validateLedgerStateQuery 10 "proposedProtocolParameters"
            [aesonQQ|{}|]
            (parseQueryLedgerProposedProtocolParameters genProposedPParamsResult)

        validateLedgerStateQuery 10 "liveStakeDistribution"
            [aesonQQ|{}|]
            (parseQueryLedgerLiveStakeDistribution genPoolDistrResult)

        validateLedgerStateQuery 10 "utxo"
            [aesonQQ|{}|]
            (parseQueryLedgerUtxo genUTxOResult)

        validateLedgerStateQuery 10 "utxo"
            [aesonQQ|
            { "addresses":
                [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
                , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
                , "82d818582183581cb0574c7a1564697578b840fd7ec9d8963fa1398415f9f2f87737a83da0001ae9e3e303"
                ]
            }|]
            (parseQueryLedgerUtxoByAddress genUTxOResult)

        validateLedgerStateQuery 10 "utxo"
            [aesonQQ|
            { "outputReferences":
                [ { "txId": "141933320b6e5d4522d7d3bf052dd2a26cc7eb58b66ae357f95f83715c8add5b"
                  , "index": 14
                  }
                ]
            }|]
            (parseQueryLedgerUtxoByOutputReference genUTxOResult)

        validateLedgerStateQuery 30 "rewardsProvenance"
            [aesonQQ|{}|]
            (parseQueryLedgerRewardsProvenance genRewardsProvenanceResult)

        validateLedgerStateQuery 10 "stakePools"
            [aesonQQ|{}|]
            (parseQueryLedgerStakePools genPoolIdsResult)

        validateLedgerStateQuery 20 "stakePoolParameters"
            [aesonQQ|
            { "stakePools":
                [ "pool1m80g9t64048p0t6yg9sps6672mgnse8ug2euudccmhkygfnf6tg"
                , "d9de82af557d4e17af444160186b5e56d13864fc42b3ce3718ddec44"
                ]
            }|]
            (parseQueryLedgerStakePoolParameters genPoolParametersResult)

        validateNetworkQuery 10 "blockHeight"
            [aesonQQ|{}|]
            (parseQueryNetworkBlockHeight (const $ genWithOrigin genBlockNo))

        validateNetworkQuery 10 "genesisConfiguration"
            [aesonQQ|{ "era": "shelley" }|]
            (parseQueryNetworkGenesisConfiguration genGenesisConfig)

        validateNetworkQuery 20 "genesisConfiguration"
            [aesonQQ|{ "era": "alonzo" }|]
            (parseQueryNetworkGenesisConfiguration genGenesisConfig)

        validateNetworkQuery 5 "startTime"
            [aesonQQ|{}|]
            (parseQueryNetworkStartTime (const genSystemStart))

        validateNetworkQuery 10 "tip"
            [aesonQQ|{}|]
            (parseQueryNetworkTip (const genPoint))

    context "validate release request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request ReleaseLedgerState))
            (_encodeReleaseLedgerState, _decodeReleaseLedgerState)
            (3, "ReleaseLedgerState")
            "ogmios.json#/properties/ReleaseLedgerState"

        validateToJSON
            (arbitrary @(Rpc.Response ReleaseLedgerStateResponse))
            _encodeReleaseLedgerStateResponse
            (3, "ReleaseLedgerStateResponse")
            "ogmios.json#/properties/ReleaseLedgerStateResponse"

instance Arbitrary a => Arbitrary (Rpc.Response a) where
    arbitrary = oneof
        [ Rpc.Response Nothing <$> arbitrary
        , Rpc.Response (Just $ toJSON @String "st") <$> arbitrary
        ]

instance Arbitrary a => Arbitrary (Rpc.Request a) where
    arbitrary = oneof
        [ Rpc.Request Nothing <$> arbitrary
        , Rpc.Request (Just $ toJSON @String "st") <$> arbitrary
        ]

instance Arbitrary (FindIntersection Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (FindIntersectionResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (NextBlockResponse Block) where
    shrink = genericShrink
    arbitrary = frequency
        [ ( 10, RollForward  <$> reasonablySized arbitrary <*> genTipNoGenesis )
        , (  1, RollBackward <$> arbitrary <*> arbitrary )
        ]

instance Arbitrary NextBlock where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (SubmitTransactionResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (HardForkApplyTxErr (CardanoEras StandardCrypto)) where
    arbitrary = genHardForkApplyTxErr

instance Arbitrary (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto))) where
    arbitrary = reasonablySized genSubmitResult

instance Arbitrary (EvaluateTransactionResponse Block) where
    arbitrary = reasonablySized genEvaluateTransactionResponse

instance Arbitrary (AcquireLedgerState Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (AcquireLedgerStateResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseLedgerState where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseLedgerStateResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary AcquireFailure where
    arbitrary = genAcquireFailure

instance Arbitrary AcquireMempool where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary AcquireMempoolResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTransaction where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTransactionFields where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (NextTransactionResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (HasTransaction Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary HasTransactionResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary SizeOfMempool where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary SizeOfMempoolResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseMempool where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseMempoolResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (Point Block) where
    arbitrary = genPoint

instance Arbitrary (Tip Block) where
    arbitrary = genTip

instance Arbitrary Block where
    arbitrary = reasonablySized genBlock

instance Arbitrary (GenTx Block) where
    arbitrary = genTx

instance Arbitrary (GenTxId Block) where
    arbitrary = genTxId

instance Arbitrary MempoolSizeAndCapacity where
    arbitrary = genMempoolSizeAndCapacity

--
-- Local Transaction Submission
--

newtype SerializedTransaction = SerializedTransaction Text deriving (Eq, Show)

prop_parseSubmitTransaction
    :: SerializedTransaction
    -> Property
prop_parseSubmitTransaction (SerializedTransaction bytes) =
    void result === Right ()
  where
    result = Json.parseEither (parseJSON @(GenTx Block)) (Json.String bytes)

instance Arbitrary SerializedTransaction where
    arbitrary = SerializedTransaction <$> elements
        -- base16, with CBOR-in-CBOR prefix, Mary era
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

        -- base16, with bootstrap witnesses, Mary era
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

        -- base16, not wrapped, Alonzo era
        , "83a4008182582039786f186d94d8dd0b4fcf05d1458b18cd5fd8c6823364612f\
          \4a3c11b77e7cc700018282581d60f8a68cd18e59a6ace848155a0e967af64f4d\
          \00cf8acee8adc95a6b0d1a05f5e10082581d60f8a68cd18e59a6ace848155a0e\
          \967af64f4d00cf8acee8adc95a6b0d1b000000d18635a3cf021a0002a3310318\
          \78a10081825820eb94e8236e2099357fa499bfbc415968691573f25ec77435b7\
          \949f5fdfaa5da05840c8c0c016b714adb318a9495849c8ec647bc9742ef2b4cd\
          \03b9bc8694b65a42dbe3a2275ebcfe482c246fc8fbc34aa8dcebf18a4c3836b3\
          \ce8473e990d61c1506f6"

         -- base16, Babbage era
        , "84A600818258207D93519864ACAD5714A4057FF16950632D45BB0B5644DD81AB\
          \13103464FC76D4000182825839015C2B1A505AAD911F3F2B1932DC37679995B3\
          \C352ECFF08070682E8365E7DD93FF18E14F79BB80924ECFD775081CE0020A19B\
          \7E06070266D11A05D49B93825839015C2B1A505AAD911F3F2B1932DC37679995\
          \B3C352ECFF08070682E8365E7DD93FF18E14F79BB80924ECFD775081CE0020A1\
          \9B7E06070266D1821A001E8480A1581CDF9E841D704A10F1D7709A7B0F0F5205\
          \9B6C20B92E8CA2E11CA88295A14557494E44591B7FFFFFFFFFFFFFFF021A0002\
          \C0ED031A0472486409A1581CDF9E841D704A10F1D7709A7B0F0F52059B6C20B9\
          \2E8CA2E11CA88295A14557494E44591B7FFFFFFFFFFFFFFF0E81581C8DDC4881\
          \655E51EF6B24C8259480D92538914F9BC955F07BCE78556DA200828258207237\
          \FE8383F77820B5A463047065E0AB4214E57BFBF0DEF3C426B68522DE2E2F5840\
          \4D833545F27245EDE7072A8DF2716B519C09CCEE44F3ABFEDDEFB9EF5E1BF4BD\
          \60AA0AF3C3C4479543B2BDA28ECDE71DBCF0463E4AF9425BF7F58F045C9E5305\
          \825820166FB97EF0865FA3FE2C1A85631D5DE79DA97536103D5CD35A80428CEC\
          \C494385840F067A931BDDFE0B0D475E902E61ECEFA124D86E1E7C9973ADD37AE\
          \FF905E5B1BB5129093CEF516969B25B50F8F24887810BB57D5908806F523A524\
          \63E93C9100018182018282051A047248648200581C8DDC4881655E51EF6B24C8\
          \259480D92538914F9BC955F07BCE78556DF5F6"
        ]

propBinaryDataRoundtrip :: Ledger.Data StandardAlonzo -> Property
propBinaryDataRoundtrip dat =
    let json = jsonToByteString (Alonzo.encodeData @StandardAlonzo dat)
     in case B16.decodeBase16 . T.encodeUtf8 <$> Json.decode (toLazy json) of
            Just (Right bytes) ->
                let
                    dataFromBytes = Ledger.makeBinaryData (toShort bytes)
                    originalData  = Ledger.dataToBinaryData dat
                  in conjoin
                    [ dataFromBytes
                        === Right originalData
                    , (Ledger.hashBinaryData <$> dataFromBytes)
                        === Right (Ledger.hashBinaryData originalData)
                    ] & counterexample (decodeUtf8 json)
            _ ->
                property False

unsafeDataFromBytes :: Era era => ByteString -> Ledger.Data era
unsafeDataFromBytes =
    either (error . show) Ledger.binaryDataToData
    . Ledger.makeBinaryData
    . either error toShort
    . B16.decodeBase16

--
-- Local State Query
--

-- | Parse a given query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateLedgerStateQuery
    :: Int
    -> Text
    -> Json.Value
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> SpecWith ()
validateLedgerStateQuery n method json parser = do
  let category = "LedgerState"
  let propName = "Query" <> category <> titleize method
  let requestRef = "ogmios.json#/properties/" <> propName
  let responseRef = requestRef <> "Response"
  parallel $ specify (toString propName) $ do
    queryRefs <- unsafeReadSchemaRef (SchemaRef requestRef)
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON
        (\params -> Json.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("query" <> category <> "/" <> method)
            , "params" .= params
            ]
        )
        queryRefs
        json
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right queryInEra -> do
            let eras = catMaybes $ (\e -> (e,) <$> queryInEra e) <$>
                    [ SomeShelleyEra ShelleyBasedEraShelley
                    , SomeShelleyEra ShelleyBasedEraAllegra
                    , SomeShelleyEra ShelleyBasedEraMary
                    , SomeShelleyEra ShelleyBasedEraAlonzo
                    , SomeShelleyEra ShelleyBasedEraBabbage
                    ]

            let nEras = length eras

            forM_ eras $ \(era, qry) ->  do
                responseRefs <- unsafeReadSchemaRef (SchemaRef responseRef)

                let encodeQueryResponse encodeResult
                        = _encodeQueryLedgerStateResponse encodeAcquireExpired
                        . Rpc.Response Nothing
                        . either QueryEraMismatch QueryResponse
                        . encodeResult

                -- NOTE: Queries are mostly identical between eras, since we run
                -- the test for each era, we can reduce the number of expected
                -- max success. In the end, the property run 1 time per era!
                case qry of
                    SomeStandardQuery _ encodeResult genResult -> do
                        case era of
                            SomeShelleyEra ShelleyBasedEraBabbage -> do
                                generateTestVectors (n, toString propName)
                                    (genResult Proxy)
                                    (encodeQueryResponse encodeResult)
                            _someOtherEra ->
                                pure ()
                        runQuickCheck $ withMaxSuccess (n `div` nEras) $ forAllBlind
                            (genResult Proxy)
                            (prop_validateToJSON
                                (encodingToValue . encodeQueryResponse encodeResult)
                                responseRefs
                            )
                    SomeAdHocQuery _ encodeResult genResult -> do
                        case era of
                            SomeShelleyEra ShelleyBasedEraBabbage -> do
                                generateTestVectors (n, toString propName)
                                    (genResult Proxy)
                                    (encodeQueryResponse encodeResult)
                            _someOtherEra ->
                                pure ()
                        runQuickCheck $ withMaxSuccess (n `div` nEras) $ forAllBlind
                            (genResult Proxy)
                            (prop_validateToJSON
                                (encodingToValue . encodeQueryResponse encodeResult)
                                responseRefs
                            )

                let encodeQueryUnavailableInCurrentEra
                        = encodingToValue
                        . _encodeQueryLedgerStateResponse encodeAcquireExpired
                        . Rpc.Response Nothing

                runQuickCheck $ withMaxSuccess 1 $ forAllBlind
                    (pure QueryUnavailableInCurrentEra)
                    (prop_validateToJSON encodeQueryUnavailableInCurrentEra responseRefs)

-- | Parse a given network query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateNetworkQuery
    :: Int
    -> Text
    -> Json.Value
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> SpecWith ()
validateNetworkQuery n method json parser = do
  let category = "Network"
  let propName = "Query" <> category <> titleize method
  let requestRef = "ogmios.json#/properties/" <> propName
  let responseRef = requestRef <> "Response"
  parallel $ specify (toString propName) $ do
    queryRefs <- unsafeReadSchemaRef (SchemaRef requestRef)
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON
        (\params -> Json.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= ("query" <> category <> "/" <> method)
            , "params" .= params
            ]
        )
        queryRefs
        json
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right queryInEra -> do
            responseRefs <- unsafeReadSchemaRef (SchemaRef responseRef)

            let encodeQueryResponse encodeResult
                    = _encodeQueryLedgerStateResponse encodeAcquireExpired
                    . Rpc.Response Nothing
                    . either QueryEraMismatch QueryResponse
                    . encodeResult

            case queryInEra (SomeShelleyEra ShelleyBasedEraBabbage) of
                Just (SomeStandardQuery _ encodeResult genResult) -> do
                    generateTestVectors (n, toString propName)
                        (genResult Proxy)
                        (encodeQueryResponse encodeResult)
                    runQuickCheck $ withMaxSuccess n $ forAllBlind
                        (genResult Proxy)
                        (prop_validateToJSON
                            (encodingToValue . encodeQueryResponse encodeResult)
                            responseRefs
                        )
                Just (SomeAdHocQuery _ encodeResult genResult) -> do
                    generateTestVectors (n, toString propName)
                        (genResult Proxy)
                        (encodeQueryResponse encodeResult)
                    runQuickCheck $ withMaxSuccess n $ forAllBlind
                        (genResult Proxy)
                        (prop_validateToJSON
                            (encodingToValue . encodeQueryResponse encodeResult)
                            responseRefs
                        )
                Nothing -> do
                    error "Failed to get network query for any era?"

generateTestVectors
    :: (Int, String)
    -> Gen a
    -> (a -> Json)
    -> IO ()
generateTestVectors (0, _) _ _ = pure ()
generateTestVectors (n, filepath) gen encode = do
    let workDir = $(getProjectRoot) <> "/test/vectors/" <> filepath <> "/"
    let padded = reverse . take 3 . reverse . ("00" <>) . show
    let filename f = workDir <> padded f <> ".json"
    let xs = let seed = 14 in generateWith (vectorOf n gen) seed
    createDirectoryIfMissing True workDir
    forConcurrently_ (zip [0..] xs) $ \(i :: Word, x) -> do
        BS.writeFile (filename i) (jsonToByteString $ encode x)

-- | Simple run a QuickCheck property
runQuickCheck :: Property -> IO ()
runQuickCheck = quickCheckWithResult (QC.stdArgs{chatty=False}) >=> \case
    Success{} -> pure ()
    Failure{output} -> expectationFailure output
    GaveUp{output} -> expectationFailure output
    NoExpectedFailure{output} -> expectationFailure output

decodeFileThrow :: FilePath -> IO Json.Value
decodeFileThrow filepath = do
    json <- Json.decodeFileStrict ($(getProjectRoot) <> "/test/golden/" <> filepath)
    maybe (fail $ "Unable to decode JSON file: " <> filepath) pure json

titleize :: Text -> Text
titleize txt =
    T.map Char.toUpper (T.take 1 txt) <> T.drop 1 txt
