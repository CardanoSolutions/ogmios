-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.JsonSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
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
    )
import Data.Aeson.QQ.Simple
    ( aesonQQ
    )
import Data.Maybe
    ( fromJust
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO (..)
    , translateUtxo
    )
import Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , decodeUtxo
    , decodeWith
    , encodeAcquireFailure
    , encodeBlock
    , encodeExUnits
    , encodePoint
    , encodeScriptFailure
    , encodeSubmitTxError
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
    , parseGetBlockHeight
    , parseGetChainTip
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetEraStart
    , parseGetFilteredDelegationsAndRewards
    , parseGetGenesisConfig
    , parseGetInterpreter
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetPoolIds
    , parseGetPoolParameters
    , parseGetPoolsRanking
    , parseGetProposedPParamsUpdates
    , parseGetRewardInfoPools
    , parseGetRewardProvenance
    , parseGetStakeDistribution
    , parseGetSystemStart
    , parseGetUTxO
    , parseGetUTxOByAddress
    , parseGetUTxOByTxIn
    )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersect
    , FindIntersectResponse (..)
    , RequestNext
    , RequestNextResponse (..)
    , _decodeFindIntersect
    , _decodeRequestNext
    , _encodeFindIntersect
    , _encodeFindIntersectResponse
    , _encodeRequestNext
    , _encodeRequestNextResponse
    )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire
    , AcquireResponse (..)
    , QueryResponse (..)
    , Release
    , ReleaseResponse (..)
    , _decodeAcquire
    , _decodeRelease
    , _encodeAcquire
    , _encodeAcquireResponse
    , _encodeQueryResponse
    , _encodeRelease
    , _encodeReleaseResponse
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AwaitAcquire
    , AwaitAcquireResponse (..)
    , HasTx
    , HasTxResponse (..)
    , MempoolSizeAndCapacity
    , NextTx
    , NextTxFields (..)
    , NextTxResponse (..)
    , ReleaseMempool
    , ReleaseMempoolResponse (..)
    , SizeAndCapacity
    , SizeAndCapacityResponse (..)
    , _decodeAwaitAcquire
    , _decodeHasTx
    , _decodeNextTx
    , _decodeReleaseMempool
    , _decodeSizeAndCapacity
    , _encodeAwaitAcquire
    , _encodeAwaitAcquireResponse
    , _encodeHasTx
    , _encodeHasTxResponse
    , _encodeNextTx
    , _encodeNextTxResponse
    , _encodeReleaseMempool
    , _encodeReleaseMempoolResponse
    , _encodeSizeAndCapacity
    , _encodeSizeAndCapacityResponse
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTxResponse
    , SubmitTxResponse (..)
    , _encodeEvaluateTxResponse
    , _encodeSubmitTxResponse
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
    , genCompactGenesisResult
    , genData
    , genDelegationAndRewardsResult
    , genEpochResult
    , genEvaluateTxResponse
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
    , genPoolsRankingResult
    , genProposedPParamsResult
    , genRewardInfoPoolsResult
    , genRewardProvenanceResult
    , genSubmitResult
    , genSystemStart
    , genTip
    , genTx
    , genTxId
    , genUTxOResult
    , genUtxoAlonzo
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

import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
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
    specify (toString $ getSchemaRef ref) $ forAllBlind gen
        (prop_validateToJSON (encodingToValue . encode) refs)

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
    specify (toString $ getSchemaRef ref) $ forAllBlind gen $ \a ->
        let leftSide = decodeWith decode (jsonToByteString (encode a)) in
        conjoin
        [ prop_validateToJSON (encodingToValue . encode) refs a
        , leftSide == Just a
            & counterexample (decodeUtf8 $ Json.encodePretty $ inefficientEncodingToValue $ encode a)
            & counterexample ("Got:  " <> show leftSide)
            & counterexample ("Want: " <> show (Just a))
        ]

goldenToJSON
    :: FilePath
    -> SchemaRef
    -> SpecWith ()
goldenToJSON golden ref = parallel $ do
    refs <- runIO $ unsafeReadSchemaRef ref
    a <- runIO $ decodeFileThrow golden
    specify ("Golden: " <> golden <> " ~ " <> toString (getSchemaRef ref)) $ withMaxSuccess 1 $
        prop_validateToJSON id refs a

spec :: Spec
spec = do
    context "JSON roundtrips" $ do
        prop "encodeUtxo / decodeUtxo (Alonzo)" $ forAllShrinkBlind genUtxoAlonzo shrinkUtxo $ \utxo ->
            let encoded = inefficientEncodingToValue (Alonzo.encodeUtxo utxo) in
            case Json.parse decodeUtxo encoded of
                Json.Error e ->
                    property False
                        & counterexample e
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)
                Json.Success utxo' ->
                    UTxOInAlonzoEra utxo === utxo'
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)

        prop "encodeUtxo / decodeUtxo (Babbage)" $ forAllShrinkBlind genUtxoBabbage shrinkUtxo $ \utxo ->
            let encoded = inefficientEncodingToValue (Babbage.encodeUtxo utxo) in
            case Json.parse decodeUtxo encoded of
                Json.Error e ->
                    property False
                        & counterexample e
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)
                Json.Success (UTxOInAlonzoEra utxo') ->
                    translateUtxo utxo' === utxo
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)
                Json.Success (UTxOInBabbageEra utxo') ->
                    utxo' === utxo
                        & counterexample (decodeUtf8 $ Json.encodePretty encoded)

        specify "Golden: Utxo_1.json" $ do
            json <- decodeFileThrow "Utxo_1.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e -> do
                    show e `shouldContain` "couldn't decode plutus script"
                    show e `shouldContain` "Please drop 'd8184c820249'"
                Json.Success UTxOInAlonzoEra{} ->
                    fail "successfully decoded an invalid payload (as Alonzo Utxo)?"
                Json.Success UTxOInBabbageEra{} ->
                    fail "successfully decoded an invalid payload( as Babbage Utxo)?"

        specify "Golden: Utxo_2.json" $ do
            json <- decodeFileThrow "Utxo_2.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e ->
                    fail (show e)
                Json.Success UTxOInAlonzoEra{} ->
                    fail "wrongly decoded Babbage UTxO as Alonzo's"
                Json.Success UTxOInBabbageEra{} ->
                    pure ()

        specify "Golden: Utxo_3.json" $ do
            json <- decodeFileThrow "Utxo_3.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e ->
                    fail (show e)
                Json.Success UTxOInAlonzoEra{} ->
                    fail "wrongly decoded Babbage UTxO as Alonzo's"
                Json.Success UTxOInBabbageEra{} ->
                    pure ()

        specify "Golden: Utxo_4.json" $ do
            json <- decodeFileThrow "Utxo_4.json"
            case Json.parse (decodeUtxo @StandardCrypto) json of
                Json.Error e -> do
                    show e `shouldContain` "couldn't decode plutus script"
                    show e `shouldContain` "Please drop '820249'"
                Json.Success UTxOInAlonzoEra{} ->
                    fail "successfully decoded an invalid payload (as Alonzo Utxo)?"
                Json.Success UTxOInBabbageEra{} ->
                    fail "successfully decoded an invalid payload( as Babbage Utxo)?"

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
        let matrix = [ ( mkSlotLength 1, Json.integer 1 )
                     , ( mkSlotLength 0.1, Json.double 0.1 )
                     ]
        forM_ matrix $ \(slotLength, json) ->
            specify (show slotLength <> " → " <> show json) $ do
                encodeSlotLength slotLength `shouldBe` json

    context "validate chain-sync request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request (FindIntersect Block)))
            (_encodeFindIntersect encodePoint, _decodeFindIntersect)
            (10, "ChainSync/Request/FindIntersect")
            "ogmios.wsp.json#/properties/FindIntersect"

        validateToJSON
            (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            (_encodeFindIntersectResponse encodePoint encodeTip)
            (10, "ChainSync/Response/FindIntersect")
            "ogmios.wsp.json#/properties/FindIntersectResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request RequestNext))
            (_encodeRequestNext, _decodeRequestNext)
            (3, "ChainSync/Request/RequestNext")
            "ogmios.wsp.json#/properties/RequestNext"

        validateToJSON
            (arbitrary @(Wsp.Response (RequestNextResponse Block)))
            (_encodeRequestNextResponse (encodeBlock FullSerialization) encodePoint encodeTip)
            (200, "ChainSync/Response/RequestNext")
            "ogmios.wsp.json#/properties/RequestNextResponse"

    context "validate tx-submission request/response against JSON-schema" $ do
        prop "deserialise signed transactions" prop_parseSubmitTx

        validateToJSON
            (arbitrary @(Wsp.Response (SubmitTxResponse Block)))
            (_encodeSubmitTxResponse (Proxy @Block) encodeTxId encodeSubmitTxError)
            (200, "TxSubmission/Response/SubmitTx")
            "ogmios.wsp.json#/properties/SubmitTxResponse"

        validateToJSON
            (arbitrary @(Wsp.Response (EvaluateTxResponse Block)))
            (_encodeEvaluateTxResponse (Proxy @Block) stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError)
            (100, "TxSubmission/Response/EvaluateTx")
            "ogmios.wsp.json#/properties/EvaluateTxResponse"

        goldenToJSON
            "SubmitTxResponse_1.json"
            "ogmios.wsp.json#/properties/SubmitTxResponse"

        goldenToJSON
            "EvaluateTxRequest_1.json"
            "ogmios.wsp.json#/properties/EvaluateTx"

    context "validate tx monitor request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request AwaitAcquire))
            (_encodeAwaitAcquire, _decodeAwaitAcquire)
            (10, "TxMonitor/Request/AwaitAcquire")
            "ogmios.wsp.json#/properties/AwaitAcquire"

        validateToJSON
            (arbitrary @(Wsp.Response AwaitAcquireResponse))
            _encodeAwaitAcquireResponse
            (10, "TxMonitor/Response/AwaitAcquire")
            "ogmios.wsp.json#/properties/AwaitAcquireResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request NextTx))
            (_encodeNextTx, _decodeNextTx)
            (10, "TxMonitor/Request/NextTx")
            "ogmios.wsp.json#/properties/NextTx"

        validateToJSON
            (arbitrary @(Wsp.Response (NextTxResponse Block)))
            (_encodeNextTxResponse encodeTxId (encodeTx FullSerialization))
            (10, "TxMonitor/Response/NextTx")
            "ogmios.wsp.json#/properties/NextTxResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request (HasTx Block)))
            (_encodeHasTx encodeTxId, _decodeHasTx)
            (10, "TxMonitor/Request/HasTx")
            "ogmios.wsp.json#/properties/HasTx"

        validateToJSON
            (arbitrary @(Wsp.Response HasTxResponse))
            _encodeHasTxResponse
            (10, "TxMonitor/Response/HasTx")
            "ogmios.wsp.json#/properties/HasTxResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request SizeAndCapacity))
            (_encodeSizeAndCapacity, _decodeSizeAndCapacity)
            (10, "TxMonitor/Request/SizeAndCapacity")
            "ogmios.wsp.json#/properties/SizeAndCapacity"

        validateToJSON
            (arbitrary @(Wsp.Response SizeAndCapacityResponse))
            _encodeSizeAndCapacityResponse
            (10, "TxMonitor/Response/SizeAndCapacity")
            "ogmios.wsp.json#/properties/SizeAndCapacityResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request ReleaseMempool))
            (_encodeReleaseMempool, _decodeReleaseMempool)
            (10, "TxMonitor/Request/ReleaseMempool")
            "ogmios.wsp.json#/properties/ReleaseMempool"

        validateToJSON
            (arbitrary @(Wsp.Response ReleaseMempoolResponse))
            _encodeReleaseMempoolResponse
            (10, "TxMonitor/Response/ReleaseMempool")
            "ogmios.wsp.json#/properties/ReleaseMempoolResponse"

    context "validate acquire request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request (Acquire Block)))
            (_encodeAcquire encodePoint, _decodeAcquire)
            (10, "StateQuery/Request/Acquire")
            "ogmios.wsp.json#/properties/Acquire"

        validateToJSON
            (arbitrary @(Wsp.Response (AcquireResponse Block)))
            (_encodeAcquireResponse encodePoint encodeAcquireFailure)
            (10, "StateQuery/Response/Acquire")
            "ogmios.wsp.json#/properties/AcquireResponse"

    context "validate local state queries against JSON-schema" $ do
        validateQuery
            [aesonQQ|"eraStart"|]
            ( parseGetEraStart genBoundResult
            ) (10, "StateQuery/Response/Query[eraStart]")
            "ogmios.wsp.json#/properties/QueryResponse[eraStart]"

        validateQuery
            [aesonQQ|"eraSummaries"|]
            ( parseGetInterpreter genInterpreterResult
            ) (10, "StateQuery/Response/Query[eraSummaries]")
            "ogmios.wsp.json#/properties/QueryResponse[eraSummaries]"

        validateQuery
            [aesonQQ|"ledgerTip"|]
            ( parseGetLedgerTip genPointResultTPraos genPointResultPraos
            ) (10, "StateQuery/Response/Query[ledgerTip]")
            "ogmios.wsp.json#/properties/QueryResponse[ledgerTip]"

        validateQuery
            [aesonQQ|"currentEpoch"|]
            ( parseGetEpochNo genEpochResult
            ) (3, "StateQuery/Response/Query[currentEpoch]")
            "ogmios.wsp.json#/properties/QueryResponse[currentEpoch]"

        validateQuery
            [aesonQQ|{ "nonMyopicMemberRewards": [14, 42] }|]
            ( parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult
            ) (10, "StateQuery/Response/Query[nonMyopicMemberRewards]")
            "ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|
            { "nonMyopicMemberRewards":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                , "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                , "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                ]
            }|]
            ( parseGetNonMyopicMemberRewards genNonMyopicMemberRewardsResult
            ) (0, "StateQuery/Response/Query[nonMyopicMemberRewards]")
            "ogmios.wsp.json#/properties/QueryResponse[nonMyopicMemberRewards]"

        validateQuery
            [aesonQQ|
            { "delegationsAndRewards":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                , "script1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftykaau8x7"
                , "stake_vkh1dss9g887v3rdmadpq3n44d5ph3ma4aha2rtxfdsnnftyklueu8u"
                , "stake179kzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjcek9u8h"
                , "stake_test17pkzq4qulejydh045yzxwk4ksx780khkl4gdve9kzwd9vjc7u07r2"
                ]
            }|]
            ( parseGetFilteredDelegationsAndRewards genDelegationAndRewardsResult
            ) (10, "StateQuery/Response/Query[delegationsAndRewards]")
            "ogmios.wsp.json#/properties/QueryResponse[delegationsAndRewards]"

        validateQuery
            [aesonQQ|"currentProtocolParameters"|]
            ( parseGetCurrentPParams genPParamsResult
            ) (100, "StateQuery/Response/Query[currentProtocolParameters]")
            "ogmios.wsp.json#/properties/QueryResponse[currentProtocolParameters]"

        validateQuery
            [aesonQQ|"proposedProtocolParameters"|]
            ( parseGetProposedPParamsUpdates genProposedPParamsResult
            ) (100, "StateQuery/Response/Query[proposedProtocolParameters]")
            "ogmios.wsp.json#/properties/QueryResponse[proposedProtocolParameters]"

        validateQuery
            [aesonQQ|"stakeDistribution"|]
            ( parseGetStakeDistribution genPoolDistrResult
            ) (10, "StateQuery/Response/Query[stakeDistribution]")
            "ogmios.wsp.json#/properties/QueryResponse[stakeDistribution]"

        validateQuery
            [aesonQQ|"utxo"|]
            ( parseGetUTxO genUTxOResult
            ) (100, "StateQuery/Response/Query[utxo]")
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo": []
            }|]
            ( parseGetUTxOByAddress genUTxOResult
            ) (0, "StateQuery/Response/Query[utxo]")
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo":
                [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
                , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
                , "82d818582183581cb0574c7a1564697578b840fd7ec9d8963fa1398415f9f2f87737a83da0001ae9e3e303"
                ]
            }|]
            ( parseGetUTxOByAddress genUTxOResult
            ) (0, "StateQuery/Response/Query[utxo]")
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo":
                [ { "txId": "141933320b6e5d4522d7d3bf052dd2a26cc7eb58b66ae357f95f83715c8add5b"
                  , "index": 14
                  }
                ]
            }|]
            ( parseGetUTxOByTxIn genUTxOResult
            ) (0, "StateQuery/Response/Query[utxo]")
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|"genesisConfig"|]
            ( parseGetGenesisConfig genCompactGenesisResult
            ) (100, "StateQuery/Response/Query[genesisConfig]")
            "ogmios.wsp.json#/properties/QueryResponse[genesisConfig]"

        validateQuery
            [aesonQQ|"rewardsProvenance"|]
            ( parseGetRewardProvenance genRewardProvenanceResult
            ) (100, "StateQuery/Response/Query[rewardsProvenance]")
            "ogmios.wsp.json#/properties/QueryResponse[rewardsProvenance]"

        validateQuery
            [aesonQQ|"rewardsProvenance'"|]
            ( parseGetRewardInfoPools genRewardInfoPoolsResult
            ) (100, "StateQuery/Response/Query[rewardsProvenance']")
            "ogmios.wsp.json#/properties/QueryResponse[rewardsProvenance']"

        validateQuery
            [aesonQQ|"poolIds"|]
            ( parseGetPoolIds genPoolIdsResult
            ) (10, "StateQuery/Response/Query[poolIds]")
            "ogmios.wsp.json#/properties/QueryResponse[poolIds]"

        validateQuery
            [aesonQQ|
            { "poolParameters":
                [ "pool1m80g9t64048p0t6yg9sps6672mgnse8ug2euudccmhkygfnf6tg"
                , "d9de82af557d4e17af444160186b5e56d13864fc42b3ce3718ddec44"
                ]
            }|]
            ( parseGetPoolParameters genPoolParametersResult
            ) (50, "StateQuery/Response/Query[poolParameters]")
            "ogmios.wsp.json#/properties/QueryResponse[poolParameters]"

        validateQuery
            [aesonQQ|"poolsRanking"|]
            ( parseGetPoolsRanking genPoolsRankingResult
            ) (10, "StateQuery/Response/Query[poolsRanking]")
            "ogmios.wsp.json#/properties/QueryResponse[poolsRanking]"

        validateQuery
            [aesonQQ|"chainTip"|]
            ( parseGetChainTip (const genPoint)
            ) (10, "StateQuery/Response/Query[chainTip]")
            "ogmios.wsp.json#/properties/QueryResponse[chainTip]"

        validateQuery
            [aesonQQ|"systemStart"|]
            ( parseGetSystemStart (const genSystemStart)
            ) (10, "StateQuery/Response/Query[systemStart]")
            "ogmios.wsp.json#/properties/QueryResponse[systemStart]"

        validateQuery
            [aesonQQ|"blockHeight"|]
            ( parseGetBlockHeight (const $ genWithOrigin genBlockNo)
            ) (10, "StateQuery/Response/Query[blockHeight]")
            "ogmios.wsp.json#/properties/QueryResponse[blockHeight]"

        goldenToJSON
            "QueryResponse-utxo_1.json"
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

    context "validate release request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request Release))
            (_encodeRelease, _decodeRelease)
            (3, "StateQuery/Request/Release")
            "ogmios.wsp.json#/properties/Release"

        validateToJSON
            (arbitrary @(Wsp.Response ReleaseResponse))
            _encodeReleaseResponse
            (3, "StateQuery/Response/Release")
            "ogmios.wsp.json#/properties/ReleaseResponse"

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = oneof
        [ Wsp.Response Nothing <$> arbitrary
        , Wsp.Response (Just $ toJSON @String "st") <$> arbitrary
        ]

instance Arbitrary a => Arbitrary (Wsp.Request a) where
    arbitrary = oneof
        [ Wsp.Request Nothing <$> arbitrary
        , Wsp.Request (Just $ toJSON @String "st") <$> arbitrary
        ]

instance Arbitrary (FindIntersect Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (RequestNextResponse Block) where
    shrink = genericShrink
    arbitrary = frequency
        [ ( 10, RollForward  <$> reasonablySized arbitrary <*> arbitrary )
        , (  1, RollBackward <$> arbitrary <*> arbitrary )
        ]

instance Arbitrary RequestNext where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (SubmitTxResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (HardForkApplyTxErr (CardanoEras StandardCrypto)) where
    arbitrary = genHardForkApplyTxErr

instance Arbitrary (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto))) where
    arbitrary = reasonablySized genSubmitResult

instance Arbitrary (EvaluateTxResponse Block) where
    arbitrary = reasonablySized genEvaluateTxResponse

instance Arbitrary (Acquire Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (AcquireResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary Release where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary ReleaseResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary AcquireFailure where
    arbitrary = genAcquireFailure

instance Arbitrary AwaitAcquire where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary AwaitAcquireResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTx where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary NextTxFields where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (NextTxResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (HasTx Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary HasTxResponse where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary SizeAndCapacity where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary SizeAndCapacityResponse where
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

        -- base64, not wrapped, Mary era
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

        -- base64, with bootstrap witnesses, Mary era
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

unsafeDataFromBytes :: ByteString -> Ledger.Data era
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
validateQuery
    :: Json.Value
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> (Int, String)
    -> SchemaRef
    -> SpecWith ()
validateQuery json parser (n, vectorFilepath) resultRef =
  parallel $ specify (toString $ getSchemaRef resultRef) $ do
    queryRefs <- unsafeReadSchemaRef queryRef
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON id queryRefs json
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
            forM_ eras $ \(era, SomeQuery{genResult,encodeResult}) -> do
                let encodeQueryResponse
                        = _encodeQueryResponse encodeAcquireFailure
                        . Wsp.Response Nothing
                        . QueryResponse
                        . encodeResult FullSerialization

                case era of
                    SomeShelleyEra ShelleyBasedEraBabbage -> do
                        generateTestVectors (n, vectorFilepath)
                            (genResult Proxy)
                            encodeQueryResponse
                    _ ->
                        pure ()

                resultRefs <- unsafeReadSchemaRef resultRef

                -- NOTE: Queries are mostly identical between eras, since we run
                -- the test for each era, we can reduce the number of expected
                -- max success. In the end, the property run 1 time per era!
                runQuickCheck $ withMaxSuccess 20 $ forAllBlind
                    (genResult Proxy)
                    (prop_validateToJSON (encodingToValue . encodeQueryResponse) resultRefs)

                let encodeQueryUnavailableInCurrentEra
                        = encodingToValue
                        . _encodeQueryResponse encodeAcquireFailure
                        . Wsp.Response Nothing

                runQuickCheck $ withMaxSuccess 1 $ forAllBlind
                    (pure QueryUnavailableInCurrentEra)
                    (prop_validateToJSON encodeQueryUnavailableInCurrentEra resultRefs)
  where
    queryRef :: SchemaRef
    queryRef = "ogmios.wsp.json#/properties/Query/properties/args/properties/query"

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
