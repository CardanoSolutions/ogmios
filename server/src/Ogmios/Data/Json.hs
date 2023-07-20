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

      -- * Encoders
    , encodeAcquireFailure
    , encodeAcquireExpired
    , encodeBlock
    , Alonzo.encodeExUnits
    , encodePoint
    , Alonzo.encodeScriptFailure
    , encodeSerializedTransaction
    , encodeSubmitTransactionError
    , encodeTip
    , encodeTx
    , encodeTxId
    , Shelley.encodeTxIn
    , Alonzo.stringifyRdmrPtr
    , Alonzo.encodeTranslationError

      -- * Decoders
    , decodeOneEraHash
    , decodePoint
    , decodeSerializedTransaction
    , decodeTip
    , decodeTxId
    , decodeUtxo
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( ToCBOR (..)
    )
import Cardano.Ledger.Crypto
    ( Crypto
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
import Ogmios.Data.Json.Query
    ( decodeOneEraHash
    , decodePoint
    , decodeSerializedTransaction
    , decodeTip
    , decodeTxId
    , decodeUtxo
    , encodeEraMismatch
    , encodeOneEraHash
    , encodePoint
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

import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Codec.CBOR.Write as Cbor

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
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
    :: Crypto crypto
    => CardanoBlock crypto
    -> Json
encodeBlock = \case
    BlockByron blk ->
        Byron.encodeABlockOrBoundary (byronBlockRaw blk)
    BlockShelley blk ->
        Shelley.encodeBlock blk
    BlockAllegra blk ->
        Allegra.encodeBlock blk
    BlockMary blk ->
        Mary.encodeBlock blk
    BlockAlonzo blk ->
        Alonzo.encodeBlock blk
    BlockBabbage blk ->
        Babbage.encodeBlock blk

encodeSubmitTransactionError
    :: Crypto crypto
    => SubmitTransactionError (CardanoBlock crypto)
    -> Json
encodeSubmitTransactionError = \case
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
    ApplyTxErrBabbage (ApplyTxError xs) ->
        encodeList Babbage.encodeLedgerFailure xs
    ApplyTxErrWrongEra e ->
        encodeList encodeEraMismatch [ e ]

encodeSerializedTransaction
    :: (PraosCrypto crypto, TPraos.PraosCrypto crypto)
    => SerializedTransaction (CardanoBlock crypto)
    -> Json
encodeSerializedTransaction = \case
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
    GenTxBabbage tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx

encodeTip
    :: Tip (CardanoBlock crypto)
    -> Json
encodeTip = \case
    TipGenesis ->
        encodeText "origin"
    Tip slot hash blockNo ->
        "slot" .=
            encodeSlotNo slot <>
        "hash" .=
            encodeOneEraHash hash <>
        "blockNo" .=
            encodeBlockNo blockNo
        & encodeObject

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => GenTx (CardanoBlock crypto)
    -> Json
encodeTx = \case
    GenTxBabbage (ShelleyTx _ x) ->
        Babbage.encodeTx x
    GenTxAlonzo (ShelleyTx _ x) ->
        Alonzo.encodeTx x
    GenTxMary (ShelleyTx _ x) ->
        Mary.encodeTx x
    GenTxAllegra (ShelleyTx _ x) ->
        Allegra.encodeTx x
    GenTxShelley (ShelleyTx _ x) ->
        Shelley.encodeTx x
    GenTxByron _ ->
        error "encodeTx: unsupported Byron transaction."

encodeTxId
    :: Crypto crypto
    => GenTxId (CardanoBlock crypto)
    -> Json
encodeTxId = \case
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
