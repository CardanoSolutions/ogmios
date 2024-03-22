--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Ogmios.Data.Json
    ( Json
    , ViaEncoding (..)
    , jsonToByteString
    , FromJSON
    , ToJSON
    , decodeWith
    , inefficientEncodingToValue
    , encodeObject

      -- * Encoders
    , encodeAcquireExpired
    , encodeAcquireFailure
    , encodeBlock
    , encodeDeserialisationFailure
    , Alonzo.encodeExUnits
    , encodePoint
    , encodeScriptFailure
    , encodeSerializedTransaction
    , encodeEvaluationError
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTx
    , encodeGenTxId
    , Shelley.encodeTxId
    , Shelley.encodeTxIn

      -- * Decoders
    , MultiEraDecoder (..)
    , decodeOneEraHash
    , decodePoint
    , decodeScript
    , decodeSerializedTransaction
    , decodeTip
    , decodeTxId
    , decodeUtxo
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Binary
    ( ToCBOR (..)
    )
import Cardano.Ledger.Shelley.API
    ( ApplyTxError (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    , GenTxId
    , SerializedTransaction
    , SubmitTransactionError
    )
import Ogmios.Data.Json.Ledger.PredicateFailure
    ( encodePredicateFailure
    )
import Ogmios.Data.Json.Ledger.ScriptFailure
    ( encodeEvaluationError
    , encodeScriptFailure
    )
import Ogmios.Data.Json.Query
    ( decodeOneEraHash
    , decodePoint
    , decodeScript
    , decodeSerializedTransaction
    , decodeTip
    , decodeTxId
    , decodeUtxo
    , encodeEraMismatch
    , encodeOneEraHash
    , encodePoint
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( pickPredicateFailure
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock (..)
    )
import Ouroboros.Consensus.Byron.Ledger.Mempool
    ( encodeByronGenTx
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , TxId (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( PraosCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( GenTx (..)
    , TxId (..)
    )
import Ouroboros.Network.Block
    ( Tip (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..)
    )

import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Codec.CBOR.Read as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.Text as T

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Ogmios.Data.Ledger.PredicateFailure.Allegra as Allegra
import qualified Ogmios.Data.Ledger.PredicateFailure.Alonzo as Alonzo
import qualified Ogmios.Data.Ledger.PredicateFailure.Babbage as Babbage
import qualified Ogmios.Data.Ledger.PredicateFailure.Conway as Conway
import qualified Ogmios.Data.Ledger.PredicateFailure.Mary as Mary
import qualified Ogmios.Data.Ledger.PredicateFailure.Shelley as Shelley


--
-- Encoders
--

encodeAcquireFailure
    :: AcquireFailure
    -> Json
encodeAcquireFailure = \case
    AcquireFailurePointTooOld ->
        encodeText "Target point is too old."
    AcquireFailurePointNotOnChain ->
        encodeText "Target point doesn't longer exist."

encodeAcquireExpired
    :: AcquireFailure
    -> Json
encodeAcquireExpired = \case
    AcquireFailurePointTooOld ->
        encodeText "Acquired point is now too old."
    AcquireFailurePointNotOnChain ->
        encodeText "Acquired point no longer exist."

encodeBlock
    :: forall crypto. (Era (ByronEra crypto))
    => (MetadataFormat, IncludeCbor)
    -> CardanoBlock crypto
    -> Json
encodeBlock opts = \case
    BlockByron blk ->
        Byron.encodeABlockOrBoundary @crypto (snd opts) (byronBlockRaw blk)
    BlockShelley blk ->
        Shelley.encodeBlock opts blk
    BlockAllegra blk ->
        Allegra.encodeBlock opts blk
    BlockMary blk ->
        Mary.encodeBlock opts blk
    BlockAlonzo blk ->
        Alonzo.encodeBlock opts blk
    BlockBabbage blk ->
        Babbage.encodeBlock opts blk
    BlockConway blk ->
        Conway.encodeBlock opts blk

encodeSubmitTransactionError
    :: Crypto crypto
    => (Rpc.FaultCode -> String -> Maybe Json -> Json)
    -> SubmitTransactionError (CardanoBlock crypto)
    -> Json
encodeSubmitTransactionError reject = \case
    ApplyTxErrWrongEra e ->
        reject (Rpc.FaultCustom 3005)
            "Failed to submit the transaction in the current era. This may happen when trying to \
            \submit a transaction near an era boundary (i.e. at the moment of a hard-fork). \
            \Retrying should help."
            (pure $ encodeEraMismatch e)
    ApplyTxErrConway (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Conway.encodeLedgerFailure <$> xs)
    ApplyTxErrBabbage (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Babbage.encodeLedgerFailure <$> xs)
    ApplyTxErrAlonzo (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Alonzo.encodeLedgerFailure <$> xs)
    ApplyTxErrMary (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Mary.encodeLedgerFailure <$> xs)
    ApplyTxErrAllegra (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Allegra.encodeLedgerFailure <$> xs)
    ApplyTxErrShelley (ApplyTxError xs) ->
        (encodePredicateFailure reject . pickPredicateFailure)
            (Shelley.encodeLedgerFailure <$> xs)
    ApplyTxErrByron{} ->
        error "encodeSubmitTransactionError: unsupported Byron transaction."

encodeSerializedTransaction
    :: (PraosCrypto crypto, TPraos.PraosCrypto crypto)
    => SerializedTransaction (CardanoBlock crypto)
    -> Json
encodeSerializedTransaction =
    encodeByteStringBase16 . Cbor.toStrictByteString . \case
        GenTxByron tx ->
            encodeByronGenTx tx
        GenTxShelley tx ->
            toCBOR tx
        GenTxAllegra tx ->
            toCBOR tx
        GenTxMary tx ->
            toCBOR tx
        GenTxAlonzo tx ->
            toCBOR tx
        GenTxBabbage tx ->
            toCBOR tx
        GenTxConway tx ->
            toCBOR tx

encodeTip
    :: Tip (CardanoBlock crypto)
    -> Json
encodeTip = \case
    TipGenesis ->
        encodeText "origin"
    Tip slot hash blockNo ->
        "slot" .=
            encodeSlotNo slot <>
        "id" .=
            encodeOneEraHash hash <>
        "height" .=
            encodeBlockNo blockNo
        & encodeObject

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => (MetadataFormat, IncludeCbor)
    -> GenTx (CardanoBlock crypto)
    -> Json
encodeTx opts = \case
    GenTxConway (ShelleyTx _ x) ->
        Conway.encodeTx opts x
    GenTxBabbage (ShelleyTx _ x) ->
        Babbage.encodeTx opts x
    GenTxAlonzo (ShelleyTx _ x) ->
        Alonzo.encodeTx opts x
    GenTxMary (ShelleyTx _ x) ->
        Mary.encodeTx opts x
    GenTxAllegra (ShelleyTx _ x) ->
        Allegra.encodeTx opts x
    GenTxShelley (ShelleyTx _ x) ->
        Shelley.encodeTx opts x
    GenTxByron _ ->
        error "encodeTx: unsupported Byron transaction."

encodeGenTxId
    :: Crypto crypto
    => GenTxId (CardanoBlock crypto)
    -> Json
encodeGenTxId = encodeObject . \case
    GenTxIdConway (ShelleyTxId x) ->
        Shelley.encodeTxId x
    GenTxIdBabbage (ShelleyTxId x) ->
        Shelley.encodeTxId x
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

encodeDeserialisationFailure
    :: (Rpc.FaultCode -> String -> Maybe Json -> Json)
    -> [(SomeShelleyEra, Binary.DecoderError, Word)]
    -> Json
encodeDeserialisationFailure reject errs =
    reject Rpc.FaultInvalidParams
        "Invalid transaction; It looks like the given transaction wasn't \
        \well-formed. Note that I try to decode the transaction in every \
        \possible era and it was malformed in ALL eras. \
        \Yet, I can't pinpoint the exact issue for I do not know in which \
        \era / format you intended the transaction to be. The 'data' field, \
        \therefore, contains errors for each era."
        (pure $ encodeObject $ mconcat
            [ encodeDecoderErrorInEra size e era | (SomeShelleyEra era, e, size) <- errs ]
        )
  where
    encodeDecoderErrorInEra
        :: forall era. ()
        => Word
        -> Binary.DecoderError
        -> ShelleyBasedEra era
        -> Json.Series
    encodeDecoderErrorInEra size e era =
        let
            k = case era of
                ShelleyBasedEraShelley -> "shelley"
                ShelleyBasedEraAllegra -> "allegra"
                ShelleyBasedEraMary    -> "mary"
                ShelleyBasedEraAlonzo  -> "alonzo"
                ShelleyBasedEraBabbage -> "babbage"
                ShelleyBasedEraConway  -> "conway"
         in
            k .= encodeDecoderError size e

    encodeDecoderError size = encodeText . reduceNoise . \case
        Binary.DecoderErrorCanonicityViolation lbl ->
            "couldn't decode due to internal constraint violations on '" <> lbl <> "': \
            \ found CBOR that isn't canonical when I expected it to be."
        Binary.DecoderErrorCustom lbl hint ->
            "couldn't decode due to internal constraint violations on '" <> lbl <> "': " <> hint
        Binary.DecoderErrorDeserialiseFailure lbl (Cbor.DeserialiseFailure offset hint) | offset >= fromIntegral size ->
            "invalid or incomplete value of type '" <> lbl <> "': " <> toText hint
        Binary.DecoderErrorDeserialiseFailure lbl (Cbor.DeserialiseFailure offset hint) ->
            "invalid CBOR found at offset [" <> show offset <> "] while decoding a value of type '" <> lbl <> "': "
            <> toText hint
        Binary.DecoderErrorEmptyList{} ->
            "couldn't decode due to internal constraint violations on a non-empty list: \
            \must not be empty"
        Binary.DecoderErrorLeftover lbl bytes ->
            "unexpected " <> show (BS.length bytes) <> " bytes found left after \
            \successfully deserialising a/an '" <> lbl <> "'"
        Binary.DecoderErrorSizeMismatch lbl expected actual | expected >= actual ->
            show (expected - actual) <> " missing element(s) in a \
            \data-structure of type '" <> lbl <> "'"
        Binary.DecoderErrorSizeMismatch lbl expected actual ->
            show (actual - expected) <> " extra element(s) in a \
            \data-structure of type '" <> lbl <> "'"
        Binary.DecoderErrorUnknownTag lbl tag ->
            "unknown binary tag (" <> show tag <> ") when decoding a value of type '" <> lbl <> "'\
            \; which is probably because I am trying to decode something else than what \
            \I encountered."
        Binary.DecoderErrorVoid ->
            "impossible: attempted to decode void. Please open an issue."
     where
        reduceNoise
          = T.replace "\n" " "
          . T.replace "Error: " ""
          . T.replace "Record" "Object / Array"
          . T.replace "Record RecD" "Object / Array"
          . T.replace " (ShelleyEra StandardCrypto)" ""
          . T.replace " (AllegraEra StandardCrypto)" ""
          . T.replace " (MaryEra StandardCrypto)" ""
          . T.replace " (AlonzoEra StandardCrypto)" ""
          . T.replace " (BabbageEra StandardCrypto)" ""
          . T.replace " (ConwayEra StandardCrypto)" ""
