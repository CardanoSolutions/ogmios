--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , jsonToByteString
    , FromJSON
    , ToJSON
    , decodeWith
    , inefficientEncodingToValue

      -- * Encoders
    , encodeAcquireFailure
    , encodeBlock
    , encodeHardForkApplyTxErr
    , encodeSubmitTxPayload
    , encodePoint
    , encodeTip
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( FromCBOR (..), ToCBOR (..) )
import Cardano.Crypto.Hashing
    ( decodeHash, hashToBytes )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient, encodeTraceClient )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..), WithOrigin (..) )
import Data.ByteString.Base16
    ( decodeBase16 )
import Data.ByteString.Base64
    ( decodeBase64 )
import Ogmios.Data.Json.Query
    ( QueryInEra, encodeEraMismatch, encodeOneEraHash, encodePoint )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock (..) )
import Ouroboros.Consensus.Byron.Ledger.Mempool
    ( encodeByronGenTx )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, wrapCBORinCBOR )
import Ouroboros.Network.Point
    ( Block (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Shelley.Spec.Ledger.API
    ( ApplyTxError (..), PraosCrypto )

import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Query as Query
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Orphans
--

-- Only used for logging
instance (Crypto crypto, PraosCrypto crypto) => ToJSON
  ( TraceClient
      (GenTx (CardanoBlock crypto))
      (HardForkApplyTxErr (CardanoEras crypto))
  ) where
    toJSON = encodeTraceClient
        (inefficientEncodingToValue . encodeSubmitTxPayload)
        (inefficientEncodingToValue . encodeHardForkApplyTxErr)

-- Only used for logging & health
instance ToJSON (Tip (CardanoBlock crypto)) where
    toJSON = inefficientEncodingToValue . encodeTip
    toEncoding = encodeTip

--
-- Encoders
--

encodeAcquireFailure
    :: AcquireFailure
    -> Json
encodeAcquireFailure = \case
    AcquireFailurePointTooOld ->
        encodeText "pointTooOld"
    AcquireFailurePointNotOnChain ->
        encodeText "pointNotOnChain"

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> CardanoBlock crypto
    -> Json
encodeBlock mode = \case
    BlockByron blk -> encodeObject
        [ ( "byron"
          , Byron.encodeABlockOrBoundary mode (byronBlockRaw blk)
          )
        ]
    BlockShelley blk -> encodeObject
        [ ( "shelley"
          , Shelley.encodeBlock mode blk
          )
        ]
    BlockAllegra blk -> encodeObject
        [ ( "allegra"
          , Allegra.encodeBlock mode blk
          )
        ]
    BlockMary blk -> encodeObject
        [ ( "mary"
          , Mary.encodeBlock mode blk
          )
        ]
    BlockAlonzo blk -> encodeObject
        [ ( "alonzo"
          , Alonzo.encodeBlock mode blk
          )
        ]

encodeHardForkApplyTxErr
    :: Crypto crypto
    => HardForkApplyTxErr (CardanoEras crypto)
    -> Json
encodeHardForkApplyTxErr = \case
    ApplyTxErrByron e ->
        Byron.encodeApplyMempoolPayloadErr e
    ApplyTxErrShelley (ApplyTxError xs) ->
        encodeList Shelley.encodeLedgerFailure xs
    ApplyTxErrAllegra (ApplyTxError xs) ->
        encodeList Allegra.encodeLedgerFailure xs
    ApplyTxErrMary (ApplyTxError xs) ->
        encodeList Mary.encodeLedgerFailure xs
    ApplyTxErrAlonzo (ApplyTxError xs) ->
        encodeList Alonzo.encodeLedgerFailure xs
    ApplyTxErrWrongEra e ->
        encodeEraMismatch e

encodeSubmitTxPayload
    :: PraosCrypto crypto
    => GenTx (CardanoBlock crypto)
    -> Json
encodeSubmitTxPayload = \case
    GenTxByron tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ encodeByronGenTx tx
    GenTxShelley tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAllegra tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxMary tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAlonzo tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx

encodeTip
    :: Tip (CardanoBlock crypto)
    -> Json
encodeTip = \case
    TipGenesis -> encodeText "origin"
    Tip slot hash blockNo -> encodeObject
        [ ( "slot"
          , encodeSlotNo slot
          )
        , ( "hash"
          , encodeOneEraHash hash
          )
        , ( "blockNo"
          , encodeBlockNo blockNo
          )
        ]

--
-- Parsers
--

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const mempty) (pure . OneEraHash . toShort . hashToBytes) . decodeHash

instance PraosCrypto crypto => FromJSON (GenTx (CardanoBlock crypto))
  where
    parseJSON = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
        bytes <- parseBase16 utf8 <|> parseBase64 utf8
        deserialiseCBOR (fromStrict bytes) <|> deserialiseCBOR (wrap bytes)
      where
        tryCBOR gen = second (gen . snd) . Cbor.deserialiseFromBytes fromCBOR
        deserialiseCBOR cbor =
            either (fail . show) pure $
                tryCBOR GenTxAlonzo cbor
                <> tryCBOR GenTxMary cbor
                <> tryCBOR GenTxAllegra cbor
                <> tryCBOR GenTxShelley cbor

        -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
        -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
        -- `24` and serialized as CBOR bytes `58xx`.
        wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

        parseBase16 :: ByteString -> Json.Parser ByteString
        parseBase16 = either (fail . toString) pure . decodeBase16

        parseBase64 :: ByteString -> Json.Parser ByteString
        parseBase64 = either (fail . toString) pure . decodeBase64

instance Crypto crypto => FromJSON (Point (CardanoBlock crypto)) where
    parseJSON json = parseOrigin json <|> parsePoint json
      where
        parseOrigin = Json.withText "Point" $ \case
            txt | txt == "origin" -> pure genesisPoint
            _ -> empty

        parsePoint = Json.withObject "Point" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= decodeOneEraHash
            pure $ Point $ At $ Block (SlotNo slot) hash

instance Crypto crypto => FromJSON (QueryInEra Proxy (CardanoBlock crypto)) where
    parseJSON = choice "query"
        [ Query.parseGetCurrentPParams (const id)
        , Query.parseGetEpochNo id
        , Query.parseGetEraStart id
        , Query.parseGetFilteredDelegationsAndRewards id
        , Query.parseGetGenesisConfig (const id)
        , Query.parseGetLedgerTip (const id)
        , Query.parseGetNonMyopicMemberRewards id
        , Query.parseGetPoolIds id
        , Query.parseGetPoolParameters id
        , Query.parseGetPoolsRanking id
        , Query.parseGetProposedPParamsUpdates (const id)
        , Query.parseGetRewardProvenance id
        , Query.parseGetStakeDistribution id
        , Query.parseGetUTxO (const id)
        , Query.parseGetUTxOByAddress (const id)
        , Query.parseGetUTxOByTxIn (const id)
        ]

instance Crypto crypto => FromJSON (Tip (CardanoBlock crypto)) where
    parseJSON json = parseOrigin json <|> parseTip json
      where
        parseOrigin = Json.withText "Tip" $ \case
            txt | txt == "origin" -> pure TipGenesis
            _ -> empty

        parseTip = Json.withObject "Tip" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= decodeOneEraHash
            blockNo <- obj .: "blockNo"
            pure $ Tip (SlotNo slot) hash (BlockNo blockNo)
