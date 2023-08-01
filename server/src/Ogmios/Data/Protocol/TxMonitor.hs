--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-# OPTIONS_GHC -fno-warn-unused-foralls #-}

module Ogmios.Data.Protocol.TxMonitor
    ( -- * Codecs
      TxMonitorCodecs (..)
    , mkTxMonitorCodecs

      -- * Messages
    , TxMonitorMessage (..)

      -- ** Acquire / AcquireMempool
    , AcquireMempool (..)
    , _encodeAcquireMempool
    , _decodeAcquireMempool
    , AcquireMempoolResponse (..)
    , _encodeAcquireMempoolResponse

      -- ** NextTransaction
    , NextTransaction (..)
    , NextTransactionFields (..)
    , _encodeNextTransaction
    , _decodeNextTransaction
    , NextTransactionResponse (..)
    , _encodeNextTransactionResponse

      -- ** HasTransaction
    , HasTransaction (..)
    , _encodeHasTransaction
    , _decodeHasTransaction
    , HasTransactionResponse (..)
    , _encodeHasTransactionResponse


      -- ** SizeOfMempool
    , SizeOfMempool (..)
    , _encodeSizeOfMempool
    , _decodeSizeOfMempool
    , SizeOfMempoolResponse (..)
    , _encodeSizeOfMempoolResponse

      -- ** ReleaseMempools
    , ReleaseMempool (..)
    , _encodeReleaseMempool
    , _decodeReleaseMempool
    , ReleaseMempoolResponse (..)
    , _encodeReleaseMempoolResponse

     -- * Re-exports
    , GenTx
    , GenTxId
    , MempoolSizeAndCapacity (..)
    , SlotNo (..)
    ) where

import Ogmios.Data.Json.Prelude hiding
    ( id
    )

import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    , GenTxId
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
    ( MempoolSizeAndCapacity (..)
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json

--
-- Codecs
--

data TxMonitorCodecs block = TxMonitorCodecs
    { decodeAcquireMempool
        :: ByteString
        -> Maybe (Rpc.Request AcquireMempool)
    , encodeAcquireMempoolResponse
        :: Rpc.Response AcquireMempoolResponse
        -> Json
    , decodeNextTransaction
        :: ByteString
        -> Maybe (Rpc.Request NextTransaction)
    , encodeNextTransactionResponse
        :: Rpc.Response (NextTransactionResponse block)
        -> Json
    , decodeHasTransaction
        :: ByteString
        -> Maybe (Rpc.Request (HasTransaction block))
    , encodeHasTransactionResponse
        :: Rpc.Response HasTransactionResponse
        -> Json
    , decodeSizeOfMempool
        :: ByteString
        -> Maybe (Rpc.Request SizeOfMempool)
    , encodeSizeOfMempoolResponse
        :: Rpc.Response SizeOfMempoolResponse
        -> Json
    , decodeReleaseMempool
        :: ByteString
        -> Maybe (Rpc.Request ReleaseMempool)
    , encodeReleaseMempoolResponse
        :: Rpc.Response ReleaseMempoolResponse
        -> Json
    }

mkTxMonitorCodecs
    :: (FromJSON (GenTxId block))
    => (GenTxId block -> Json)
    -> (GenTx block -> Json)
    -> TxMonitorCodecs block
mkTxMonitorCodecs encodeTxId encodeTx =
    TxMonitorCodecs
        { decodeAcquireMempool = decodeWith _decodeAcquireMempool
        , encodeAcquireMempoolResponse = _encodeAcquireMempoolResponse
        , decodeNextTransaction = decodeWith _decodeNextTransaction
        , encodeNextTransactionResponse = _encodeNextTransactionResponse encodeTxId encodeTx
        , decodeHasTransaction = decodeWith _decodeHasTransaction
        , encodeHasTransactionResponse = _encodeHasTransactionResponse
        , decodeSizeOfMempool = decodeWith _decodeSizeOfMempool
        , encodeSizeOfMempoolResponse = _encodeSizeOfMempoolResponse
        , decodeReleaseMempool = decodeWith _decodeReleaseMempool
        , encodeReleaseMempoolResponse = _encodeReleaseMempoolResponse
        }

--
-- Messages
--

data TxMonitorMessage block
    = MsgAcquireMempool
        AcquireMempool
        (Rpc.ToResponse AcquireMempoolResponse)
    | MsgNextTransaction
        NextTransaction
        (Rpc.ToResponse (NextTransactionResponse block))
    | MsgHasTransaction
        (HasTransaction block)
        (Rpc.ToResponse HasTransactionResponse)
    | MsgSizeOfMempool
        SizeOfMempool
        (Rpc.ToResponse SizeOfMempoolResponse)
    | MsgReleaseMempool
        ReleaseMempool
        (Rpc.ToResponse ReleaseMempoolResponse)

--
-- AcquireMempool
--

data AcquireMempool
    = AcquireMempool
    deriving (Generic, Show, Eq)

_encodeAcquireMempool
    :: Rpc.Request AcquireMempool
    -> Json
_encodeAcquireMempool =
    Rpc.mkRequestNoParams

_decodeAcquireMempool
    :: Json.Value
    -> Json.Parser (Rpc.Request AcquireMempool)
_decodeAcquireMempool =
    Rpc.genericFromJSON Rpc.defaultOptions

data AcquireMempoolResponse
    = AcquireMempoolResponse { slot :: SlotNo }
    deriving (Generic, Show, Eq)

_encodeAcquireMempoolResponse
    :: Rpc.Response AcquireMempoolResponse
    -> Json
_encodeAcquireMempoolResponse =
    Rpc.mkResponse $ \resolve _reject -> \case
        AcquireMempoolResponse{slot} ->
            resolve $ encodeObject
                ( "acquired" .=
                    encodeText "mempool" <>
                  "slot" .=
                    encodeSlotNo slot
                )

--
-- NextTransaction
--

data NextTransaction
    = NextTransaction { fields :: Maybe NextTransactionFields }
    deriving (Generic, Show, Eq)

_encodeNextTransaction
    :: Rpc.Request NextTransaction
    -> Json
_encodeNextTransaction =
    Rpc.mkRequest $ encodeObject . \case
        NextTransaction{fields} ->
            case fields of
                Nothing ->
                    mempty
                Just NextTransactionAllFields ->
                    "fields" .= encodeText "all"

_decodeNextTransaction
    :: Json.Value
    -> Json.Parser (Rpc.Request NextTransaction)
_decodeNextTransaction =
    Rpc.genericFromJSON Rpc.defaultOptions
        { Rpc.onMissingField = \case
            "fields" ->
                pure Json.Null
            k ->
                Rpc.onMissingField Rpc.defaultOptions k
        }

data NextTransactionFields
    = NextTransactionAllFields
    deriving (Generic, Show, Eq)

instance FromJSON NextTransactionFields where
    parseJSON = Json.withText "NextTransactionFields" $ \x -> do
        when (x /= "all") $ do
            fail "Invalid argument to 'fields'. Expected 'all'."
        pure NextTransactionAllFields

instance ToJSON NextTransactionFields where
    toJSON = \case
        NextTransactionAllFields ->
            Json.String "all"
    toEncoding = \case
        NextTransactionAllFields ->
            Json.text "all"

data NextTransactionResponse block
    = NextTransactionResponseId
        { nextId :: Maybe (GenTxId block)
        }
    | NextTransactionResponseTx
        { nextTx :: Maybe (GenTx block)
        }

    | NextTransactionMustAcquireFirst
    deriving (Generic)
deriving instance
    ( Show (GenTxId block)
    , Show (GenTx block)
    ) => Show (NextTransactionResponse block)
deriving instance
    ( Eq (GenTxId block)
    , Eq (GenTx block)
    ) => Eq (NextTransactionResponse block)

_encodeNextTransactionResponse
    :: forall block. ()
    => (GenTxId block -> Json)
    -> (GenTx block -> Json)
    -> Rpc.Response (NextTransactionResponse block)
    -> Json
_encodeNextTransactionResponse encodeTxId encodeTx =
    Rpc.mkResponse $ \resolve reject -> \case
        NextTransactionResponseId{nextId} ->
            resolve $ encodeObject
                ( "transaction" .= encodeMaybe encodeTxId nextId
                )
        NextTransactionResponseTx{nextTx} ->
            resolve $ encodeObject
                ( "transaction" .= encodeMaybe encodeTx nextTx
                )
        NextTransactionMustAcquireFirst ->
            reject (Rpc.FaultCustom 4000)
                "You must acquire a mempool snapshot prior to accessing it."
                Nothing

--
-- HasTransaction
--
data HasTransaction block
    = HasTransaction { id :: GenTxId block }
    deriving (Generic)
deriving instance Show (GenTxId block) => Show (HasTransaction block)
deriving instance   Eq (GenTxId block) =>   Eq (HasTransaction block)

_encodeHasTransaction
    :: forall block. ()
    => (GenTxId block -> Json)
    -> Rpc.Request (HasTransaction block)
    -> Json
_encodeHasTransaction encodeTxId =
    Rpc.mkRequest $ \case
        HasTransaction{id} ->
            encodeTxId id

_decodeHasTransaction
    :: forall block. (FromJSON (GenTxId block))
    => Json.Value
    -> Json.Parser (Rpc.Request (HasTransaction block))
_decodeHasTransaction =
    Rpc.genericFromJSON Rpc.defaultOptions

data HasTransactionResponse
    = HasTransactionResponse { has :: Bool }
    | HasTransactionMustAcquireFirst
    deriving (Generic, Show, Eq)

_encodeHasTransactionResponse
    :: forall block. ()
    => Rpc.Response HasTransactionResponse
    -> Json
_encodeHasTransactionResponse =
    Rpc.mkResponse $ \resolve reject -> \case
        HasTransactionResponse{has} ->
            resolve (encodeBool has)
        HasTransactionMustAcquireFirst ->
            reject (Rpc.FaultCustom 4000)
                "You must acquire a mempool snapshot prior to accessing it."
                Nothing

--
-- SizeOfMempool
--

data SizeOfMempool
    = SizeOfMempool
    deriving (Generic, Show, Eq)

_encodeSizeOfMempool
    :: Rpc.Request SizeOfMempool
    -> Json
_encodeSizeOfMempool =
    Rpc.mkRequestNoParams

_decodeSizeOfMempool
    :: Json.Value
    -> Json.Parser (Rpc.Request SizeOfMempool)
_decodeSizeOfMempool =
    Rpc.genericFromJSON Rpc.defaultOptions

data SizeOfMempoolResponse
    = SizeOfMempoolResponse { mempool :: MempoolSizeAndCapacity }
    | SizeOfMempoolMustAcquireFirst
    deriving (Generic, Show)

_encodeSizeOfMempoolResponse
    :: Rpc.Response SizeOfMempoolResponse
    -> Json
_encodeSizeOfMempoolResponse =
    Rpc.mkResponse $ \resolve reject -> \case
        SizeOfMempoolResponse{mempool} ->
            resolve $ encodeObject
                ( "maxCapacity" .= encodeObject
                    ( "bytes" .= encodeWord32 (capacityInBytes mempool) ) <>
                  "currentSize" .= encodeObject
                    ( "bytes" .= encodeWord32 (sizeInBytes mempool)) <>
                  "transactions" .= encodeObject
                    ( "count" .= encodeWord32 (numberOfTxs mempool))
                )
        SizeOfMempoolMustAcquireFirst ->
            reject (Rpc.FaultCustom 4000)
                "You must acquire a mempool snapshot prior to accessing it."
                Nothing

--
-- ReleaseMempool
--

data ReleaseMempool
    = ReleaseMempool
    deriving (Generic, Show, Eq)

_encodeReleaseMempool
    :: Rpc.Request ReleaseMempool
    -> Json
_encodeReleaseMempool =
    Rpc.mkRequestNoParams

_decodeReleaseMempool
    :: Json.Value
    -> Json.Parser (Rpc.Request ReleaseMempool)
_decodeReleaseMempool =
    Rpc.genericFromJSON Rpc.defaultOptions

data ReleaseMempoolResponse
    = Released
    | ReleaseMempoolMustAcquireFirst
    deriving (Generic, Show)

_encodeReleaseMempoolResponse
    :: Rpc.Response ReleaseMempoolResponse
    -> Json
_encodeReleaseMempoolResponse =
    Rpc.mkResponse $ \resolve reject -> \case
        Released ->
            resolve $ encodeObject
                ( "released" .= encodeText "mempool"
                )
        ReleaseMempoolMustAcquireFirst ->
            reject (Rpc.FaultCustom 4000)
                "You must acquire a mempool snapshot prior to accessing it."
                Nothing
