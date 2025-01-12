-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.JsonSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , GenTxId
    )
import Cardano.Slotting.Time
    ( RelativeTime (..)
    , mkSlotLength
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
import GHC.Generics
    ( Rep
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO (..)
    , Upgrade (..)
    )
import Ogmios.Data.Json
    ( Json
    , MultiEraDecoder (..)
    , decodeScript
    , decodeUtxo
    , decodeWith
    , encodeAcquireExpired
    , encodeBlock
    , encodeDeserialisationFailure
    , encodeEvaluationError
    , encodeExUnits
    , encodeGenTxId
    , encodeObject
    , encodePoint
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTx
    , encodeTxId
    , inefficientEncodingToValue
    , jsonToByteString
    )
import Ogmios.Data.Json.Ledger.PredicateFailure
    ( encodeScriptPurposeIndexInAnyEra
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Json.Prelude
    ( MetadataFormat (..)
    , encodeRelativeTime
    , encodeSlotLength
    , omitOptionalCbor
    )
import Ogmios.Data.Json.Query
    ( QueryInEra
    , SomeQuery (..)
    , parseQueryLedgerConstitution
    , parseQueryLedgerConstitutionalCommittee
    , parseQueryLedgerDelegateRepresentatives
    , parseQueryLedgerEpoch
    , parseQueryLedgerEraStart
    , parseQueryLedgerEraSummaries
    , parseQueryLedgerGovernanceProposals
    , parseQueryLedgerGovernanceProposalsByProposalReference
    , parseQueryLedgerLiveStakeDistribution
    , parseQueryLedgerProjectedRewards
    , parseQueryLedgerProposedProtocolParameters
    , parseQueryLedgerProtocolParameters
    , parseQueryLedgerRewardAccountSummaries
    , parseQueryLedgerRewardsProvenance
    , parseQueryLedgerStakePools
    , parseQueryLedgerTip
    , parseQueryLedgerTreasuryAndReserves
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
    ( EvaluateTransaction
    , EvaluateTransactionResponse
    , SubmitTransaction
    , SubmitTransactionResponse (..)
    , _encodeEvaluateTransactionResponse
    , _encodeSubmitTransactionResponse
    )
import Ouroboros.Consensus.Cardano.Block
    ( GenTx
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
    ( genAccountStateResult
    , genAcquireFailure
    , genBlock
    , genBlockNo
    , genBoundResult
    , genCommitteeMembersStateResult
    , genConstitutionResult
    , genData
    , genDelegateRepresentativesResult
    , genEpochResult
    , genEvaluateTransactionResponse
    , genGenTxId
    , genGenesisConfig
    , genGovActionStateResult
    , genHardForkApplyTxErr
    , genInterpreterResult
    , genMempoolSizeAndCapacity
    , genNonMyopicMemberRewardsResult
    , genPParamsResult
    , genPoint
    , genPointResultPraos
    , genPointResultTPraos
    , genPoolDistrResult
    , genPoolParametersResult
    , genProposedPParamsResult
    , genRewardAccountSummariesResult
    , genRewardsProvenanceResult
    , genSubmitResult
    , genSystemStart
    , genTip
    , genTipNoGenesis
    , genTx
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

import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Plutus.Data as Ledger
import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
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

        specify "Golden: Script_Native_0.json" $ do
            json <- decodeFileThrow "Script_native_0.json"
            case traverse @[] (Json.parse (decodeScript @(BabbageEra StandardCrypto))) json of
                Json.Error e ->
                    fail (show e)
                Json.Success{}  ->
                    pure ()

        specify "Golden: SubmitTransactionFailure<3133>" $ do
            json <- decodeFileThrow "SubmitTransactionFailure_1.json"
            refs <- unsafeReadSchemaRef "ogmios.json#/properties/SubmitTransactionResponse"
            runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON identity refs json

        specify "Golden: NextTransactionResponse_1" $ do
            json <- decodeFileThrow "NextTransactionResponse_1.json"
            refs <- unsafeReadSchemaRef "ogmios.json#/properties/NextTransactionResponse"
            runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON identity refs json

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
        let matrix = [ ( mkSlotLength 1
                       , Json.pairs $ Json.pair "milliseconds" (Json.integer 1000)
                       )
                     , ( mkSlotLength 0.1
                       , Json.pairs $ Json.pair "milliseconds" (Json.integer 100)
                       )
                     , ( mkSlotLength 42
                       , Json.pairs $ Json.pair "milliseconds" (Json.integer 42000)
                       )
                     ]
        forM_ matrix $ \(slotLength, json) ->
            specify (show slotLength <> " → " <> show json) $ do
                encodeSlotLength slotLength `shouldBe` json

    context "RelativeTime" $ do
        specify "Do not overflow" $ do
            let json = Json.pairs $ Json.pair "seconds" (Json.integer 207360000)
            encodeRelativeTime (RelativeTime 207360000) `shouldBe` json

    context "validate chain-sync request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request (FindIntersection Block)))
            (_encodeFindIntersection encodePoint, _decodeFindIntersection)
            (10, "FindIntersection")
            "ogmios.json#/properties/FindIntersection"

        validateToJSON
            (arbitrary @(Rpc.Response (FindIntersectionResponse Block)))
            (_encodeFindIntersectionResponse Rpc.defaultOptions encodePoint encodeTip)
            (10, "FindIntersectionResponse")
            "ogmios.json#/properties/FindIntersectionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request NextBlock))
            (_encodeNextBlock, _decodeNextBlock)
            (3, "NextBlock")
            "ogmios.json#/properties/NextBlock"

        validateToJSON
            (arbitrary @(Rpc.Response (NextBlockResponse Block)))
            (_encodeNextBlockResponse Rpc.defaultOptions (encodeBlock (MetadataNoSchema, omitOptionalCbor)) encodePoint encodeTip)
            (50, "NextBlockResponse")
            "ogmios.json#/properties/NextBlockResponse"

    context "validate transaction submission request/response against JSON-schema" $ do
        prop "deserialise signed transactions" prop_parseSubmitTransaction
        validateToJSON
            (arbitrary @(Rpc.Response (SubmitTransactionResponse Block)))
            (_encodeSubmitTransactionResponse (Proxy @Block)
                Rpc.defaultOptions
                encodeGenTxId
                encodeSubmitTransactionError
                encodeDeserialisationFailure
            )
            (200, "SubmitTransactionResponse")
            "ogmios.json#/properties/SubmitTransactionResponse"

        validateToJSON
            (arbitrary @(Rpc.Response (EvaluateTransactionResponse Block)))
            (_encodeEvaluateTransactionResponse (Proxy @Block)
                Rpc.defaultOptions
                encodeScriptPurposeIndexInAnyEra
                encodeExUnits
                encodeEvaluationError
                encodeDeserialisationFailure
            )
            (50, "EvaluateTransactionResponse")
            "ogmios.json#/properties/EvaluateTransactionResponse"

    context "validate mempool monitoring request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request AcquireMempool))
            (_encodeAcquireMempool, _decodeAcquireMempool)
            (10, "AcquireMempool")
            "ogmios.json#/properties/AcquireMempool"

        validateToJSON
            (arbitrary @(Rpc.Response AcquireMempoolResponse))
            (_encodeAcquireMempoolResponse Rpc.defaultOptions)
            (10, "AcquireMempoolResponse")
            "ogmios.json#/properties/AcquireMempoolResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request NextTransaction))
            (_encodeNextTransaction, _decodeNextTransaction)
            (10, "NextTransaction")
            "ogmios.json#/properties/NextTransaction"

        validateToJSON
            (arbitrary @(Rpc.Response (NextTransactionResponse Block)))
            (_encodeNextTransactionResponse Rpc.defaultOptions encodeGenTxId (encodeTx (MetadataNoSchema, omitOptionalCbor)))
            (10, "NextTransactionResponse")
            "ogmios.json#/properties/NextTransactionResponse"

        validateToJSON
            (arbitrary @(Rpc.Response (NextTransactionResponse Block)))
            (_encodeNextTransactionResponse Rpc.defaultOptions encodeGenTxId (encodeTx (MetadataDetailedSchema, omitOptionalCbor)))
            (10, "NextTransactionResponse")
            "ogmios.json#/properties/NextTransactionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request (HasTransaction Block)))
            (_encodeHasTransaction (encodeObject . encodeTxId), _decodeHasTransaction)
            (10, "HasTransaction")
            "ogmios.json#/properties/HasTransaction"

        validateToJSON
            (arbitrary @(Rpc.Response HasTransactionResponse))
            (_encodeHasTransactionResponse Rpc.defaultOptions)
            (10, "HasTransactionResponse")
            "ogmios.json#/properties/HasTransactionResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request SizeOfMempool))
            (_encodeSizeOfMempool, _decodeSizeOfMempool)
            (10, "SizeOfMempool")
            "ogmios.json#/properties/SizeOfMempool"

        validateToJSON
            (arbitrary @(Rpc.Response SizeOfMempoolResponse))
            (_encodeSizeOfMempoolResponse Rpc.defaultOptions)
            (10, "SizeOfMempoolResponse")
            "ogmios.json#/properties/SizeOfMempoolResponse"

        validateFromJSON
            (arbitrary @(Rpc.Request ReleaseMempool))
            (_encodeReleaseMempool, _decodeReleaseMempool)
            (10, "ReleaseMempoolRequest")
            "ogmios.json#/properties/ReleaseMempool"

        validateToJSON
            (arbitrary @(Rpc.Response ReleaseMempoolResponse))
            (_encodeReleaseMempoolResponse Rpc.defaultOptions)
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
            (_encodeAcquireLedgerStateResponse Rpc.defaultOptions encodePoint encodeAcquireExpired)
            (10, "AcquireLedgerStateResponse")
            "ogmios.json#/properties/AcquireLedgerStateResponse"

    context "validate local state queries against JSON-schema" $ do
        validateLedgerStateQuery 3 "epoch"
            Nothing
            (parseQueryLedgerEpoch genEpochResult)

        validateLedgerStateQuery 10 "eraStart"
            Nothing
            (parseQueryLedgerEraStart genBoundResult)

        validateLedgerStateQuery 10 "eraSummaries"
            Nothing
            (parseQueryLedgerEraSummaries genInterpreterResult)

        validateLedgerStateQuery 10 "tip"
            Nothing
            (parseQueryLedgerTip genPointResultTPraos genPointResultPraos)

        validateLedgerStateQuery 0 "projectedRewards"
            (Just [aesonQQ|{ "stake": [{ "ada": { "lovelace": 14 } } , { "ada": { "lovelace": 42 } }] }|])
            (parseQueryLedgerProjectedRewards genNonMyopicMemberRewardsResult)

        validateLedgerStateQuery 10 "projectedRewards"
            (Just [aesonQQ|
                { "keys":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                    , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                    , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                    ]
                , "scripts":
                    [ "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                    ]
                }
            |])
            (parseQueryLedgerProjectedRewards genNonMyopicMemberRewardsResult)

        validateLedgerStateQuery 10 "rewardAccountSummaries"
            (Just [aesonQQ|
                { "keys":
                    [ "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                    , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                    , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                    ]
                , "scripts":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                    ]
                }
            |])
            (parseQueryLedgerRewardAccountSummaries genRewardAccountSummariesResult)

        validateLedgerStateQuery 30 "protocolParameters"
            Nothing
            (parseQueryLedgerProtocolParameters genPParamsResult)

        validateLedgerStateQuery 10 "proposedProtocolParameters"
            Nothing
            (parseQueryLedgerProposedProtocolParameters genProposedPParamsResult)

        validateLedgerStateQuery 10 "liveStakeDistribution"
            Nothing
            (parseQueryLedgerLiveStakeDistribution genPoolDistrResult)

        validateLedgerStateQuery 20 "utxo"
            Nothing
            (parseQueryLedgerUtxo genUTxOResult)

        validateLedgerStateQuery 0 "utxo"
            (Just [aesonQQ|
                { "addresses":
                    [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
                    , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
                    , "82d818582183581cb0574c7a1564697578b840fd7ec9d8963fa1398415f9f2f87737a83da0001ae9e3e303"
                    ]
                }
            |])
            (parseQueryLedgerUtxoByAddress genUTxOResult)

        validateLedgerStateQuery 0 "utxo"
            (Just [aesonQQ|
                { "outputReferences":
                    [ { "transaction": { "id": "141933320b6e5d4522d7d3bf052dd2a26cc7eb58b66ae357f95f83715c8add5b" }
                      , "index": 14
                      }
                    ]
                }
            |])
            (parseQueryLedgerUtxoByOutputReference genUTxOResult)

        validateLedgerStateQuery 30 "rewardsProvenance"
            Nothing
            (parseQueryLedgerRewardsProvenance genRewardsProvenanceResult)

        validateLedgerStateQuery 10 "stakePools"
            Nothing
            (parseQueryLedgerStakePools genPoolParametersResult)

        validateLedgerStateQuery 0 "stakePools"
            (Just [aesonQQ|
                {
                    "stakePools": [
                        { "id": "pool1lllmq2jgcqrag5c77lpc5m34fsqn63leadyx9tzx842n66ly3ql" },
                        { "id": "pool1rutq574pcq30mn9xuytgpqyvn69zq2dnycp2fhnw0hsuyqpnh99" }
                    ]
                }
            |])
            (parseQueryLedgerStakePools genPoolParametersResult)

        validateLedgerStateQuery 10 "constitution"
            Nothing
            (parseQueryLedgerConstitution genConstitutionResult)

        validateLedgerStateQuery 10 "constitutionalCommittee"
            Nothing
            (parseQueryLedgerConstitutionalCommittee genCommitteeMembersStateResult)

        validateLedgerStateQuery 10 "treasuryAndReserves"
            Nothing
            (parseQueryLedgerTreasuryAndReserves genAccountStateResult)

        validateLedgerStateQuery 0 "governanceProposals"
            (Just [aesonQQ|{
                "proposals": [
                    {
                        "transaction": { "id": "141933320b6e5d4522d7d3bf052dd2a26cc7eb58b66ae357f95f83715c8add5b" },
                        "index": 14
                    },
                    {
                        "transaction": { "id": "7d3bf052dd2a26cc7eb58b66ae357f95f83715c8add5b141933320b6e5d4522d" },
                        "index": 42
                    }
                ]
            }|])
            (parseQueryLedgerGovernanceProposalsByProposalReference genGovActionStateResult)

        validateLedgerStateQuery 10 "governanceProposals"
            Nothing
            (parseQueryLedgerGovernanceProposals genGovActionStateResult)

        validateLedgerStateQuery 0 "delegateRepresentatives"
            (Just [aesonQQ|
                { "keys":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "drep_vkh12rtxfdsnnftykmpq2sw0uezgmh66zpr8t2mgr0rhmtm06ygr39j"
                    ]
                , "scripts":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "drep_script1mh66zpr8t2mgr0rhmtm065xkvjmp8xjkfdkzq4qulejyst8h2gg"
                    ]
                }
            |])
            (parseQueryLedgerDelegateRepresentatives genDelegateRepresentativesResult)

        validateLedgerStateQuery 0 "delegateRepresentatives"
            (Just [aesonQQ|
                { "keys":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "drep_vkh12rtxfdsnnftykmpq2sw0uezgmh66zpr8t2mgr0rhmtm06ygr39j"
                    ]
                }
            |])
            (parseQueryLedgerDelegateRepresentatives genDelegateRepresentativesResult)

        validateLedgerStateQuery 0 "delegateRepresentatives"
            (Just [aesonQQ|
                { "scripts":
                    [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                    , "drep_script1mh66zpr8t2mgr0rhmtm065xkvjmp8xjkfdkzq4qulejyst8h2gg"
                    ]
                }
            |])
            (parseQueryLedgerDelegateRepresentatives genDelegateRepresentativesResult)

        validateLedgerStateQuery 0 "delegateRepresentatives"
            (Just [aesonQQ|
                {}
            |])
            (parseQueryLedgerDelegateRepresentatives genDelegateRepresentativesResult)

        validateLedgerStateQuery 10 "delegateRepresentatives"
            Nothing
            (parseQueryLedgerDelegateRepresentatives genDelegateRepresentativesResult)

        validateNetworkQuery 10 "blockHeight"
            Nothing
            (parseQueryNetworkBlockHeight (const $ genWithOrigin genBlockNo))

        validateNetworkQuery 10 "genesisConfiguration"
            (Just [aesonQQ|{ "era": "shelley" }|])
            (parseQueryNetworkGenesisConfiguration genGenesisConfig)

        validateNetworkQuery 20 "genesisConfiguration"
            (Just [aesonQQ|{ "era": "alonzo" }|])
            (parseQueryNetworkGenesisConfiguration genGenesisConfig)

        validateNetworkQuery 5 "startTime"
            Nothing
            (parseQueryNetworkStartTime (const genSystemStart))

        validateNetworkQuery 10 "tip"
            Nothing
            (parseQueryNetworkTip (const genPoint))

    context "validate release request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Rpc.Request ReleaseLedgerState))
            (_encodeReleaseLedgerState, _decodeReleaseLedgerState)
            (3, "ReleaseLedgerState")
            "ogmios.json#/properties/ReleaseLedgerState"

        validateToJSON
            (arbitrary @(Rpc.Response ReleaseLedgerStateResponse))
            (_encodeReleaseLedgerStateResponse Rpc.defaultOptions)
            (3, "ReleaseLedgerStateResponse")
            "ogmios.json#/properties/ReleaseLedgerStateResponse"

instance (Arbitrary a, Rpc.GRpcMethodName (Rep a)) => Arbitrary (Rpc.Request a) where
    shrink (Rpc.Request method mirror a) =
        Rpc.Request method mirror <$> shrink a
    arbitrary = oneof
        [ Rpc.Request method Nothing <$> arbitrary
        , liftA2 (Rpc.Request method) (Just . toJSON <$> genNanoId) arbitrary
        ]
      where
        method = Rpc.gRpcMethodName @(Rep a) Rpc.defaultOptions Proxy

class IsRpcResponse res where
    type RpcRequestFor res :: Type

instance (Arbitrary a, Rpc.GRpcMethodName (Rep (RpcRequestFor a))) => Arbitrary (Rpc.Response a) where
    shrink (Rpc.Response method mirror a) =
        Rpc.Response method mirror <$> shrink a
    arbitrary = oneof
        [ Rpc.Response (Just method) Nothing <$> arbitrary
        , liftA2 (Rpc.Response (Just method)) (Just . toJSON <$> genNanoId) arbitrary
        ]
      where
        method = Rpc.gRpcMethodName @(Rep (RpcRequestFor a)) Rpc.defaultOptions Proxy

genNanoId :: Gen String
genNanoId =
  replicateM 12 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

instance Arbitrary (FindIntersection Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse (FindIntersectionResponse Block) where
    type RpcRequestFor (FindIntersectionResponse Block) = FindIntersection Block

instance Arbitrary (FindIntersectionResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextBlock where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse (NextBlockResponse Block) where
    type RpcRequestFor (NextBlockResponse Block) = NextBlock

instance Arbitrary (NextBlockResponse Block) where
    shrink = genericShrink
    arbitrary = frequency
        [ ( 10, RollForward  <$> reasonablySized arbitrary <*> genTipNoGenesis )
        , (  1, RollBackward <$> arbitrary <*> arbitrary )
        ]

instance IsRpcResponse (SubmitTransactionResponse Block) where
    type RpcRequestFor (SubmitTransactionResponse Block) = SubmitTransaction Block

instance Arbitrary (SubmitTransactionResponse Block) where
    shrink = \case
        SubmitTransactionSuccess{} -> []
        SubmitTransactionDeserialisationFailure{} -> []
        SubmitTransactionFailure{} -> []
        SubmitTransactionFailedToUpgrade{} -> []
    arbitrary = frequency
        [ (1, SubmitTransactionSuccess <$> arbitrary)
        , (50, SubmitTransactionFailure <$> genHardForkApplyTxErr)
        , (1, pure $ SubmitTransactionDeserialisationFailure
            [ ( SomeShelleyEra ShelleyBasedEraShelley, Binary.DecoderErrorVoid, 0 )
            , ( SomeShelleyEra ShelleyBasedEraAllegra, Binary.DecoderErrorVoid, 0 )
            , ( SomeShelleyEra ShelleyBasedEraMary,    Binary.DecoderErrorVoid, 0 )
            , ( SomeShelleyEra ShelleyBasedEraAlonzo,  Binary.DecoderErrorVoid, 0 )
            , ( SomeShelleyEra ShelleyBasedEraBabbage, Binary.DecoderErrorVoid, 0 )
            , ( SomeShelleyEra ShelleyBasedEraConway,  Binary.DecoderErrorVoid, 0 )
            ]
          )
        ]

instance Arbitrary (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto))) where
    arbitrary = reasonablySized genSubmitResult

instance IsRpcResponse (EvaluateTransactionResponse Block) where
    type RpcRequestFor (EvaluateTransactionResponse Block) = EvaluateTransaction Block

instance Arbitrary (EvaluateTransactionResponse Block) where
    arbitrary = reasonablySized genEvaluateTransactionResponse

instance Arbitrary (AcquireLedgerState Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse (AcquireLedgerStateResponse Block) where
    type RpcRequestFor (AcquireLedgerStateResponse Block) = AcquireLedgerState Block

instance Arbitrary (AcquireLedgerStateResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseLedgerState where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse ReleaseLedgerStateResponse where
    type RpcRequestFor ReleaseLedgerStateResponse = ReleaseLedgerState

instance Arbitrary ReleaseLedgerStateResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary AcquireFailure where
    arbitrary = genAcquireFailure

instance Arbitrary AcquireMempool where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse AcquireMempoolResponse where
    type RpcRequestFor AcquireMempoolResponse = AcquireMempool

instance Arbitrary AcquireMempoolResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTransaction where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTransactionFields where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance IsRpcResponse (NextTransactionResponse Block) where
    type RpcRequestFor (NextTransactionResponse Block) = NextTransaction

instance Arbitrary (NextTransactionResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (HasTransaction Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse HasTransactionResponse where
    type RpcRequestFor HasTransactionResponse = HasTransaction Block

instance Arbitrary HasTransactionResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary SizeOfMempool where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse SizeOfMempoolResponse where
    type RpcRequestFor SizeOfMempoolResponse = SizeOfMempool

instance Arbitrary SizeOfMempoolResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance IsRpcResponse ReleaseMempoolResponse where
    type RpcRequestFor ReleaseMempoolResponse = ReleaseMempool

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
    arbitrary = genGenTxId

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
    case result of
        Right MultiEraDecoderSuccess{} -> property ()
        _ -> property False
  where
    result = Json.parseEither (parseJSON @(MultiEraDecoder (GenTx Block))) json
    json = Json.Object (Json.singleton "cbor" (Json.String bytes))

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
     in case decodeBase16 . T.encodeUtf8 <$> Json.decode (toLazy json) of
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
    . decodeBase16

--
-- Local State Query
--

-- | Parse a given query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateLedgerStateQuery
    :: Int
    -> Text
    -> Maybe Json.Value
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> SpecWith ()
validateLedgerStateQuery n subMethod params parser = do
  let category = "LedgerState"
  let propName = "Query" <> category <> titleize subMethod
  let requestRef = "ogmios.json#/properties/" <> propName
  let responseRef = requestRef <> "Response"
  let method = "query" <> category <> "/" <> subMethod
  parallel $ specify (toString propName) $ do
    queryRefs <- unsafeReadSchemaRef (SchemaRef requestRef)
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON
        (\_params -> Json.object $
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= method
            ] ++ maybe [] (\x -> ["params" .= x]) _params
        )
        queryRefs
        params
    case Json.parseEither parser (fromMaybe Json.emptyObject params) of
        Left e ->
            expectationFailure $ "failed to parse JSON query params: " <> show e
        Right queryInEra -> do
            let eras = mapMaybe
                    (\e -> (e,) <$> queryInEra e)
                    [ SomeShelleyEra ShelleyBasedEraShelley
                    , SomeShelleyEra ShelleyBasedEraAllegra
                    , SomeShelleyEra ShelleyBasedEraMary
                    , SomeShelleyEra ShelleyBasedEraAlonzo
                    , SomeShelleyEra ShelleyBasedEraBabbage
                    , SomeShelleyEra ShelleyBasedEraConway
                    ]

            let nEras = length eras

            forM_ eras $ \(era, qry) ->  do
                responseRefs <- unsafeReadSchemaRef (SchemaRef responseRef)

                let encodeQueryResponse
                        :: forall result. ()
                        => (result -> Either Json.Encoding Json.Encoding)
                        -> result
                        -> Json.Encoding
                    encodeQueryResponse encodeResult
                            = _encodeQueryLedgerStateResponse Rpc.defaultOptions encodeAcquireExpired
                            . Rpc.Response (Just $ toString method) Nothing
                            . either QueryEraMismatch QueryResponse
                            . encodeResult

                let iterations = (max nEras n) `div` nEras

                -- NOTE: Queries are mostly identical between eras, since we run
                -- the test for each era, we can reduce the number of expected
                -- max success. In the end, the property run 1 time per era!
                case qry of
                    SomeStandardQuery _ encodeResult genResult -> do
                        case era of
                            SomeShelleyEra ShelleyBasedEraConway -> do
                                generateTestVectors (n, toString propName)
                                    (genResult Proxy)
                                    (encodeQueryResponse encodeResult)
                            _someOtherEra ->
                                pure ()
                        runQuickCheck $ withMaxSuccess iterations $ forAllBlind
                            (genResult Proxy)
                            (prop_validateToJSON
                                (encodingToValue . encodeQueryResponse encodeResult)
                                responseRefs
                            )
                    SomeCompoundQuery _ _ _ encodeResult genResult -> do
                        case era of
                            SomeShelleyEra ShelleyBasedEraConway -> do
                                generateTestVectors (n, toString propName)
                                    (genResult Proxy)
                                    (encodeQueryResponse encodeResult)
                            _someOtherEra ->
                                pure ()
                        runQuickCheck $ withMaxSuccess iterations $ forAllBlind
                            (genResult Proxy)
                            (prop_validateToJSON
                                (encodingToValue . encodeQueryResponse encodeResult)
                                responseRefs
                            )
                    SomeAdHocQuery _ encodeResult genResult -> do
                        case era of
                            SomeShelleyEra ShelleyBasedEraConway -> do
                                generateTestVectors (n, toString propName)
                                    (genResult Proxy)
                                    (encodeQueryResponse (Right . encodeResult))
                            _someOtherEra ->
                                pure ()
                        runQuickCheck $ withMaxSuccess iterations $ forAllBlind
                            (genResult Proxy)
                            (prop_validateToJSON
                                (encodingToValue . encodeQueryResponse (Right . encodeResult))
                                responseRefs
                            )

                let encodeQueryUnavailableInCurrentEra
                        = encodingToValue
                        . _encodeQueryLedgerStateResponse Rpc.defaultOptions encodeAcquireExpired
                        . Rpc.Response (Just $ toString method) Nothing

                runQuickCheck $ withMaxSuccess 1 $ forAllBlind
                    (pure QueryUnavailableInCurrentEra)
                    (prop_validateToJSON encodeQueryUnavailableInCurrentEra responseRefs)

-- | Parse a given network query, generate an arbitrary value, encode it and validate
-- the encoding against a JSON-schema.
validateNetworkQuery
    :: Int
    -> Text
    -> Maybe Json.Value
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> SpecWith ()
validateNetworkQuery n subMethod params parser = do
  let category = "Network"
  let propName = "Query" <> category <> titleize subMethod
  let requestRef = "ogmios.json#/properties/" <> propName
  let responseRef = requestRef <> "Response"
  let method = "query" <> category <> "/" <> subMethod
  parallel $ specify (toString propName) $ do
    queryRefs <- unsafeReadSchemaRef (SchemaRef requestRef)
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON
        (\_params -> Json.object $
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= method
            ] ++ maybe [] (\x -> ["params" .= x]) _params
        )
        queryRefs
        params
    case Json.parseEither parser (fromMaybe Json.emptyObject params)  of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right queryInEra -> do
            responseRefs <- unsafeReadSchemaRef (SchemaRef responseRef)

            let encodeQueryResponse
                    :: forall result. ()
                    => (result -> Either Json.Encoding Json.Encoding)
                    -> result
                    -> Json.Encoding
                encodeQueryResponse encodeResult
                    = _encodeQueryLedgerStateResponse Rpc.defaultOptions encodeAcquireExpired
                    . Rpc.Response (Just $ toString method) Nothing
                    . either QueryEraMismatch QueryResponse
                    . encodeResult

            case queryInEra (SomeShelleyEra ShelleyBasedEraBabbage) of
                Just (SomeStandardQuery _ encodeResult genResult) -> do
                    generateTestVectors (n, toString propName)
                        (reasonablySized $ genResult Proxy)
                        (encodeQueryResponse encodeResult)
                    runQuickCheck $ withMaxSuccess n $ forAllBlind
                        (reasonablySized $ genResult Proxy)
                        (prop_validateToJSON
                            (encodingToValue . encodeQueryResponse encodeResult)
                            responseRefs
                        )
                Just (SomeCompoundQuery _ _ _ encodeResult genResult) -> do
                    generateTestVectors (n, toString propName)
                        (reasonablySized $ genResult Proxy)
                        (encodeQueryResponse encodeResult)
                    runQuickCheck $ withMaxSuccess n $ forAllBlind
                        (reasonablySized $ genResult Proxy)
                        (prop_validateToJSON
                            (encodingToValue . encodeQueryResponse encodeResult)
                            responseRefs
                        )
                Just (SomeAdHocQuery _ encodeResult genResult) -> do
                    generateTestVectors (n, toString propName)
                        (reasonablySized $ genResult Proxy)
                        (encodeQueryResponse (Right . encodeResult))
                    runQuickCheck $ withMaxSuccess n $ forAllBlind
                        (reasonablySized $ genResult Proxy)
                        (prop_validateToJSON
                            (encodingToValue . encodeQueryResponse (Right . encodeResult))
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

decodeFileThrow :: Json.FromJSON a => FilePath -> IO a
decodeFileThrow filepath = do
    json <- Json.decodeFileStrict ($(getProjectRoot) <> "/test/golden/" <> filepath)
    maybe (fail $ "Unable to decode JSON file: " <> filepath) pure json

titleize :: Text -> Text
titleize txt =
    T.map Char.toUpper (T.take 1 txt) <> T.drop 1 txt
