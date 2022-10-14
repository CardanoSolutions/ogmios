--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

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
    , encodeTx
    , encodeTxId
    , Shelley.encodeTxIn
    , Alonzo.stringifyRdmrPtr
    , Alonzo.encodeTranslationError

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
    , SerializedTx
    , SubmitTxError
    )
import Ogmios.Data.Json.Query
    ( decodeOneEraHash
    , decodePoint
    , decodeSerializedTx
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
    BlockBabbage blk -> encodeObject
        [ ( "babbage"
          , Babbage.encodeBlock mode blk
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
    ApplyTxErrBabbage (ApplyTxError xs) ->
        encodeList Babbage.encodeLedgerFailure xs
    ApplyTxErrWrongEra e ->
        encodeList encodeEraMismatch [ e ]

encodeSerializedTx
    :: (PraosCrypto crypto, TPraos.PraosCrypto crypto)
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
    GenTxBabbage tx ->
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

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => SerializationMode
    -> GenTx (CardanoBlock crypto)
    -> Json
encodeTx mode = \case
    GenTxBabbage (ShelleyTx _ x) ->
        Babbage.encodeTx mode x
    GenTxAlonzo (ShelleyTx _ x) ->
        Alonzo.encodeTx mode x
    GenTxMary (ShelleyTx _ x) ->
        Mary.encodeTx mode x
    GenTxAllegra (ShelleyTx _ x) ->
        Allegra.encodeTx mode x
    GenTxShelley (ShelleyTx _ x) ->
        Shelley.encodeTx mode x
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
