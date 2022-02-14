--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , ViaEncoding (..)
    , jsonToByteString
    , FromJSON
    , ToJSON
    , decodeWith
    , inefficientEncodingToValue

      -- * Encoders
    , encodeAcquireFailure
    , encodeBlock
    , Alonzo.encodeExUnits
    , encodePoint
    , Alonzo.encodeScriptFailure
    , encodeSerializedTx
    , encodeSubmitTxError
    , encodeTip
    , encodeTxId
    , Shelley.encodeTxIn
    , Alonzo.stringifyRdmrPtr
    , Alonzo.encodeUtxo

      -- * Decoders
    , decodeOneEraHash
    , decodePoint
    , decodeSerializedTx
    , decodeTip
    , decodeTxId
    , decodeUtxo
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( DecoderError, FromCBOR (..), ToCBOR (..), decodeFull )
import Cardano.Crypto.Hash
    ( hashFromBytes )
import Cardano.Crypto.Hashing
    ( decodeHash, hashToBytes )
import Cardano.Ledger.Alonzo.TxBody
    ( TxOut )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Shelley.API
    ( ApplyTxError (..), PraosCrypto )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..) )
import Cardano.Ledger.TxIn
    ( TxIn )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx, GenTxId, SerializedTx, SubmitTxError )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..), WithOrigin (..) )
import Formatting.Buildable
    ( build )
import Ogmios.Data.Json.Query
    ( decodeTxIn
    , decodeTxOut
    , encodeEraMismatch
    , encodeOneEraHash
    , encodePoint
    )
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
    , TxId (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( AlonzoEra, MaryEra )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( TxId (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, wrapCBORinCBOR )
import Ouroboros.Network.Point
    ( Block (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )

import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

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

encodeSubmitTxError
    :: Crypto crypto
    => SubmitTxError (CardanoBlock crypto)
    -> Json
encodeSubmitTxError = \case
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
        encodeList encodeEraMismatch [ e ]

encodeSerializedTx
    :: PraosCrypto crypto
    => SerializedTx (CardanoBlock crypto)
    -> Json
encodeSerializedTx = \case
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

encodeTxId
    :: Crypto crypto
    => GenTxId (CardanoBlock crypto)
    -> Json
encodeTxId = \case
    GenTxIdAlonzo (ShelleyTxId x) ->
        Shelley.encodeTxId x
    GenTxIdMary (ShelleyTxId x) ->
        Shelley.encodeTxId x
    GenTxIdAllegra (ShelleyTxId x) ->
        Shelley.encodeTxId x
    GenTxIdShelley (ShelleyTxId x) ->
        Shelley.encodeTxId x
    GenTxIdByron _ ->
        error "encodeTxId: unsupported Byron transaction."

--
-- Decoders
--

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const mempty) (pure . OneEraHash . toShort . hashToBytes) . decodeHash

decodePoint
    :: Json.Value
    -> Json.Parser (Point (CardanoBlock crypto))
decodePoint json =
    parseOrigin json <|> parsePoint json
  where
    parseOrigin = Json.withText "Point" $ \case
        txt | txt == "origin" -> pure genesisPoint
        _ -> empty

    parsePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        pure $ Point $ At $ Block (SlotNo slot) hash

decodeSerializedTx
    :: forall crypto. PraosCrypto crypto
    => Json.Value
    -> Json.Parser (SerializedTx (CardanoBlock crypto))
decodeSerializedTx = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8 <|> decodeBase64 utf8 <|> invalidEncodingError
    -- NOTE: Avoiding 'asum' here because it generates poor errors on failures
    deserialiseCBOR GenTxMary (wrap bytes)
        <|> deserialiseCBOR GenTxMary (fromStrict bytes)
        <|> deserialiseCBOR GenTxAlonzo (wrap bytes)
        <|> deserialiseCBOR GenTxAlonzo (fromStrict bytes)
  where
    invalidEncodingError :: Json.Parser a
    invalidEncodingError =
        fail "failed to decode payload from base64 or base16."

    -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
    -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
    -- `24` and serialized as CBOR bytes `58xx`.
    wrap :: ByteString -> LByteString
    wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

    deserialiseCBOR
        :: forall era.
            ( Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ ShelleyBlock (MaryEra crypto))
            , FromCBOR (GenTx era)
            )
        => (GenTx era -> GenTx (CardanoBlock crypto))
        -> LByteString
        -> Json.Parser (GenTx (CardanoBlock crypto))
    deserialiseCBOR mk =
        either (fail . prettyDecoderError) (pure . mk)
        .
        decodeFull
      where
        _compilerWarning = keepRedundantConstraint
            (Proxy @(Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ ShelleyBlock (MaryEra crypto))
            ))

        prettyDecoderError :: DecoderError -> String
        prettyDecoderError =
            toString
                . TL.replace
                    (toLazy $ label (Proxy @(GenTx era)))
                    "serialised transaction"
                . TL.replace
                    "\n"
                    " "
                . TL.toLazyText
                . build

decodeUtxo
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (UTxO (AlonzoEra crypto))
decodeUtxo v = do
    xs <- Json.parseJSONList v >>= traverse decodeUtxoEntry
    pure $ UTxO (Map.fromList xs)
  where
    decodeUtxoEntry :: Json.Value -> Json.Parser (TxIn crypto, TxOut (AlonzoEra crypto))
    decodeUtxoEntry =
        Json.parseJSONList >=> \case
            [i, o] ->
                (,) <$> decodeTxIn i <*> decodeTxOut o
            _ ->
                fail
                    "Failed to decode utxo entry. Expected an array of length \
                    \2 as [output-reference, output]"

decodeTip
    :: Json.Value
    -> Json.Parser (Tip (CardanoBlock crypto))
decodeTip json =
    parseOrigin json <|> parseTip json
  where
    parseOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty

    parseTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeTxId
    :: forall crypto. PraosCrypto crypto
    => Json.Value
    -> Json.Parser (GenTxId (CardanoBlock crypto))
decodeTxId = Json.withText "TxId" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8
    case hashFromBytes bytes of
        Nothing ->
            fail "couldn't interpret bytes as blake2b-256 digest."
        Just h ->
            pure $ GenTxIdAlonzo $ ShelleyTxId $ Ledger.TxId (Ledger.unsafeMakeSafeHash h)
