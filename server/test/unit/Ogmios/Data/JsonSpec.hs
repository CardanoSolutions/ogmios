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

import Cardano.Network.Protocol.NodeToClient
    ( Block )
import Data.Aeson
    ( parseJSON, toJSON )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import Data.Maybe
    ( fromJust )
import Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , decodeWith
    , encodeAcquireFailure
    , encodeBlock
    , encodeHardForkApplyTxErr
    , encodePoint
    , encodeTip
    , jsonToByteString
    )
import Ogmios.Data.Json.Query
    ( QueryInEra
    , ShelleyBasedEra (..)
    , SomeQuery (..)
    , SomeShelleyEra (..)
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetEraStart
    , parseGetFilteredDelegationsAndRewards
    , parseGetGenesisConfig
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    , parseGetUTxOByAddress
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
import Ogmios.Data.Protocol.TxSubmission
    ( SubmitTxResponse, _encodeSubmitTxResponse )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras, GenTx, HardForkApplyTxErr (..) )
import Ouroboros.Consensus.Shelley.Protocol
    ( StandardCrypto )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )
import Test.Generators
    ( genAcquireFailure
    , genBlock
    , genBoundResult
    , genCompactGenesisResult
    , genDelegationAndRewardsResult
    , genEpochResult
    , genHardForkApplyTxErr
    , genNonMyopicMemberRewardsResult
    , genPParamsResult
    , genPoint
    , genPointResult
    , genPoolDistrResult
    , genProposedPParamsResult
    , genTip
    , genUTxOResult
    , reasonablySized
    )
import Test.Hspec
    ( Spec, SpecWith, context, expectationFailure, parallel, runIO, specify )
import Test.Hspec.Json.Schema
    ( SchemaRef (..), prop_validateToJSON, unsafeReadSchemaRef )
import Test.Hspec.QuickCheck
    ( prop )
import Test.Path.Util
    ( getProjectRoot )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , Gen
    , Property
    , Result (..)
    , conjoin
    , elements
    , forAllBlind
    , genericShrink
    , oneof
    , quickCheckWithResult
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Test.QuickCheck as QC

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
validateToJSON gen encode ref = parallel $ do
    refs <- runIO $ unsafeReadSchemaRef ref
    specify (toString $ getSchemaRef ref) $ forAllBlind gen
        (prop_validateToJSON (jsonifierToAeson . encode) refs)

-- | Similar to 'validateToJSON', but also check that the produce value can be
-- decoded back to the expected form.
validateFromJSON
    :: (Eq a, Show a)
    => Gen a
    -> (a -> Json, Json.Value -> Json.Parser a)
    -> SchemaRef
    -> SpecWith ()
validateFromJSON gen (encode, decode) ref = parallel $ do
    refs <- runIO $ unsafeReadSchemaRef ref
    specify (toString $ getSchemaRef ref) $ forAllBlind gen $ \a -> conjoin
        [ prop_validateToJSON (jsonifierToAeson . encode) refs a
        , decodeWith decode (jsonToByteString (encode a)) === Just a
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
    context "validate chain-sync request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request (FindIntersect Block)))
            (_encodeFindIntersect encodePoint, _decodeFindIntersect)
            "ogmios.wsp.json#/properties/FindIntersect"

        validateToJSON
            (arbitrary @(Wsp.Response (FindIntersectResponse Block)))
            (_encodeFindIntersectResponse encodePoint encodeTip)
            "ogmios.wsp.json#/properties/FindIntersectResponse"

        validateFromJSON
            (arbitrary @(Wsp.Request RequestNext))
            (_encodeRequestNext, _decodeRequestNext)
            "ogmios.wsp.json#/properties/RequestNext"

        validateToJSON
            (arbitrary @(Wsp.Response (RequestNextResponse Block)))
            (_encodeRequestNextResponse (encodeBlock FullSerialization) encodePoint encodeTip)
            "ogmios.wsp.json#/properties/RequestNextResponse"

    context "validate tx submission reqquest/response against JSON-schema" $ do
        prop "deserialise signed transactions" prop_parseSubmitTx

        validateToJSON
            (arbitrary @(Wsp.Response (SubmitTxResponse Block)))
            (_encodeSubmitTxResponse (Proxy @Block) encodeHardForkApplyTxErr)
            "ogmios.wsp.json#/properties/SubmitTxResponse"

        goldenToJSON
            "SubmitTxResponse_1.json"
            "ogmios.wsp.json#/properties/SubmitTxResponse"

    context "validate acquire request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request (Acquire Block)))
            (_encodeAcquire encodePoint, _decodeAcquire)
            "ogmios.wsp.json#/properties/Acquire"

        validateToJSON
            (arbitrary @(Wsp.Response (AcquireResponse Block)))
            (_encodeAcquireResponse encodePoint encodeAcquireFailure)
            "ogmios.wsp.json#/properties/AcquireResponse"

    context "validate local state queries against JSON-schema" $ do
        validateQuery
            [aesonQQ|"eraStart"|]
            ( parseGetEraStart genBoundResult
            ) "ogmios.wsp.json#/properties/QueryResponse[eraStart]"

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
            [aesonQQ|
            { "delegationsAndRewards":
                [ "6c20541cfe6446ddf5a104675ab681bc77daf6fd50d664b6139a564b"
                ]
            }|]
            ( parseGetFilteredDelegationsAndRewards genDelegationAndRewardsResult
            ) "ogmios.wsp.json#/properties/QueryResponse[delegationsAndRewards]"

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

        validateQuery
            [aesonQQ|
            { "utxo": []
            }|]
            ( parseGetUTxOByAddress genUTxOResult
            ) "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|
            { "utxo":
                [ "addr1vxsvu329sr8z92usevrr6scp4vxxn0j8e20avag662uesgq385tfd"
                , "Ae2tdPwUPEZEQHoZTVq3KQhtjP32JzoEE5onUS45bFmsBSXYCXSXEQEzb4v"
                , "82d818582183581cb0574c7a1564697578b840fd7ec9d8963fa1398415f9f2f87737a83da0001ae9e3e303"
                ]
            }|]
            ( parseGetUTxOByAddress genUTxOResult
            ) "ogmios.wsp.json#/properties/QueryResponse[utxo]"

        validateQuery
            [aesonQQ|"genesisConfig"|]
            ( parseGetGenesisConfig genCompactGenesisResult
            ) "ogmios.wsp.json#/properties/QueryResponse[genesisConfig]"

        goldenToJSON
            "QueryResponse-utxo_1.json"
            "ogmios.wsp.json#/properties/QueryResponse[utxo]"

    context "validate release request/response against JSON-schema" $ do
        validateFromJSON
            (arbitrary @(Wsp.Request Release))
            (_encodeRelease, _decodeRelease)
            "ogmios.wsp.json#/properties/Release"

        validateToJSON
            (arbitrary @(Wsp.Response ReleaseResponse))
            _encodeReleaseResponse
            "ogmios.wsp.json#/properties/ReleaseResponse"

instance Arbitrary a => Arbitrary (Wsp.Response a) where
    arbitrary = oneof
        [ Wsp.Response Nothing <$> arbitrary
        , Wsp.Response (Just $ toJSON @Int 14) <$> arbitrary
        ]

instance Arbitrary a => Arbitrary (Wsp.Request a) where
    arbitrary = oneof
        [ Wsp.Request Nothing <$> arbitrary
        , Wsp.Request (Just $ toJSON @String "patate") <$> arbitrary
        ]

instance Arbitrary (FindIntersect Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (FindIntersectResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (RequestNextResponse Block) where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary RequestNext where
    shrink = genericShrink
    arbitrary = reasonablySized genericArbitrary

instance Arbitrary (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto))) where
    arbitrary = genHardForkApplyTxErr

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

instance Arbitrary (Point Block) where
    arbitrary = genPoint

instance Arbitrary (Tip Block) where
    arbitrary = genTip

instance Arbitrary Block where
    arbitrary = genBlock True

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
    -> (Json.Value -> Json.Parser (QueryInEra Gen Block))
    -> SchemaRef
    -> SpecWith ()
validateQuery json parser resultRef = parallel $ specify (toString $ getSchemaRef resultRef) $ do
    queryRefs <- unsafeReadSchemaRef queryRef
    runQuickCheck $ withMaxSuccess 1 $ prop_validateToJSON id queryRefs json
    case Json.parseEither parser json of
        Left e ->
            expectationFailure $ "failed to parse JSON: " <> show e
        Right queryInEra -> do
            let eras = catMaybes $ queryInEra <$>
                    [ SomeShelleyEra ShelleyBasedEraShelley
                    , SomeShelleyEra ShelleyBasedEraAllegra
                    , SomeShelleyEra ShelleyBasedEraMary
                    , SomeShelleyEra ShelleyBasedEraAlonzo
                    ]
            forM_ eras $ \SomeQuery{genResult,encodeResult} -> do
                let encodeQueryResponse
                        = jsonifierToAeson
                        . _encodeQueryResponse encodeAcquireFailure
                        . Wsp.Response Nothing
                        . QueryResponse
                        . encodeResult

                resultRefs <- unsafeReadSchemaRef resultRef

                -- NOTE: Queries are mostly identical between eras, since we run
                -- the test for each era, we can reduce the number of expected
                -- max success. In the end, the property run 4 times!
                runQuickCheck $ withMaxSuccess 25 $ forAllBlind
                    (genResult Proxy)
                    (prop_validateToJSON encodeQueryResponse resultRefs)

                let encodeQueryUnavailableInCurrentEra
                        = jsonifierToAeson
                        . _encodeQueryResponse encodeAcquireFailure
                        . Wsp.Response Nothing

                runQuickCheck $ withMaxSuccess 1 $ forAllBlind
                    (pure QueryUnavailableInCurrentEra)
                    (prop_validateToJSON encodeQueryUnavailableInCurrentEra resultRefs)
  where
    queryRef :: SchemaRef
    queryRef = "ogmios.wsp.json#/properties/Query/properties/args/properties/query"

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
