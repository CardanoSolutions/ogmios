--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
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

      -- ** Acquire / AwaitAcquire
    , AwaitAcquire (..)
    , _encodeAwaitAcquire
    , _decodeAwaitAcquire
    , AwaitAcquireResponse (..)
    , _encodeAwaitAcquireResponse

      -- ** NextTx
    , NextTx (..)
    , NextTxFields (..)
    , _encodeNextTx
    , _decodeNextTx
    , NextTxResponse (..)
    , _encodeNextTxResponse

      -- ** HasTx
    , HasTx (..)
    , _encodeHasTx
    , _decodeHasTx
    , HasTxResponse (..)
    , _encodeHasTxResponse


      -- ** SizeAndCapacity
    , SizeAndCapacity (..)
    , _encodeSizeAndCapacity
    , _decodeSizeAndCapacity
    , SizeAndCapacityResponse (..)
    , _encodeSizeAndCapacityResponse

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
    { decodeAwaitAcquire
        :: ByteString
        -> Maybe (Rpc.Request AwaitAcquire)
    , encodeAwaitAcquireResponse
        :: Rpc.Response AwaitAcquireResponse
        -> Json
    , decodeNextTx
        :: ByteString
        -> Maybe (Rpc.Request NextTx)
    , encodeNextTxResponse
        :: Rpc.Response (NextTxResponse block)
        -> Json
    , decodeHasTx
        :: ByteString
        -> Maybe (Rpc.Request (HasTx block))
    , encodeHasTxResponse
        :: Rpc.Response HasTxResponse
        -> Json
    , decodeSizeAndCapacity
        :: ByteString
        -> Maybe (Rpc.Request SizeAndCapacity)
    , encodeSizeAndCapacityResponse
        :: Rpc.Response SizeAndCapacityResponse
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
        { decodeAwaitAcquire = decodeWith _decodeAwaitAcquire
        , encodeAwaitAcquireResponse = _encodeAwaitAcquireResponse
        , decodeNextTx = decodeWith _decodeNextTx
        , encodeNextTxResponse = _encodeNextTxResponse encodeTxId encodeTx
        , decodeHasTx = decodeWith _decodeHasTx
        , encodeHasTxResponse = _encodeHasTxResponse
        , decodeSizeAndCapacity = decodeWith _decodeSizeAndCapacity
        , encodeSizeAndCapacityResponse = _encodeSizeAndCapacityResponse
        , decodeReleaseMempool = decodeWith _decodeReleaseMempool
        , encodeReleaseMempoolResponse = _encodeReleaseMempoolResponse
        }

--
-- Messages
--

data TxMonitorMessage block
    = MsgAwaitAcquire
        AwaitAcquire
        (Rpc.ToResponse AwaitAcquireResponse)
        Rpc.ToFault
    | MsgNextTx
        NextTx
        (Rpc.ToResponse (NextTxResponse block))
        Rpc.ToFault
    | MsgHasTx
        (HasTx block)
        (Rpc.ToResponse HasTxResponse)
        Rpc.ToFault
    | MsgSizeAndCapacity
        SizeAndCapacity
        (Rpc.ToResponse SizeAndCapacityResponse)
        Rpc.ToFault
    | MsgReleaseMempool
        ReleaseMempool
        (Rpc.ToResponse ReleaseMempoolResponse)
        Rpc.ToFault

--
-- AwaitAcquire
--

data AwaitAcquire
    = AwaitAcquire
    deriving (Generic, Show, Eq)

_encodeAwaitAcquire
    :: Rpc.Request AwaitAcquire
    -> Json
_encodeAwaitAcquire =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        AwaitAcquire ->
            mempty

_decodeAwaitAcquire
    :: Json.Value
    -> Json.Parser (Rpc.Request AwaitAcquire)
_decodeAwaitAcquire =
    Rpc.genericFromJSON Rpc.defaultOptions

data AwaitAcquireResponse
    = AwaitAcquired { slot :: SlotNo }
    deriving (Generic, Show, Eq)

_encodeAwaitAcquireResponse
    :: Rpc.Response AwaitAcquireResponse
    -> Json
_encodeAwaitAcquireResponse =
    Rpc.mkResponse $ encodeObject . \case
        AwaitAcquired{slot} ->
            "AwaitAcquired" .= encodeObject
                ( "slot" .= encodeSlotNo slot
                )

--
-- NextTx
--

data NextTx
    = NextTx { fields :: Maybe NextTxFields }
    deriving (Generic, Show, Eq)

_encodeNextTx
    :: Rpc.Request NextTx
    -> Json
_encodeNextTx =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        NextTx{fields} ->
            case fields of
                Nothing ->
                    mempty
                Just NextTxAllFields ->
                    "fields" .= encodeText "all"

_decodeNextTx
    :: Json.Value
    -> Json.Parser (Rpc.Request NextTx)
_decodeNextTx =
    Rpc.genericFromJSON Rpc.defaultOptions
        { Rpc.onMissingField = \case
            "fields" ->
                pure Json.Null
            k ->
                Rpc.onMissingField Rpc.defaultOptions k
        }

data NextTxFields
    = NextTxAllFields
    deriving (Generic, Show, Eq)

instance FromJSON NextTxFields where
    parseJSON = Json.withText "NextTxFields" $ \x -> do
        when (x /= "all") $ do
            fail "Invalid argument to 'fields'. Expected 'all'."
        pure NextTxAllFields

instance ToJSON NextTxFields where
    toJSON = \case
        NextTxAllFields ->
            Json.String "all"
    toEncoding = \case
        NextTxAllFields ->
            Json.text "all"

data NextTxResponse block
    = NextTxResponseId
        { nextId :: Maybe (GenTxId block)
        }
    | NextTxResponseTx
        { nextTx :: Maybe (GenTx block)
        }
    deriving (Generic)
deriving instance
    ( Show (GenTxId block)
    , Show (GenTx block)
    ) => Show (NextTxResponse block)
deriving instance
    ( Eq (GenTxId block)
    , Eq (GenTx block)
    ) => Eq (NextTxResponse block)

_encodeNextTxResponse
    :: forall block. ()
    => (GenTxId block -> Json)
    -> (GenTx block -> Json)
    -> Rpc.Response (NextTxResponse block)
    -> Json
_encodeNextTxResponse encodeTxId encodeTx =
    Rpc.mkResponse $ \case
        NextTxResponseId{nextId} ->
            encodeMaybe encodeTxId nextId
        NextTxResponseTx{nextTx} ->
            encodeMaybe encodeTx nextTx

--
-- HasTx
--
data HasTx block
    = HasTx { id :: GenTxId block }
    deriving (Generic)
deriving instance Show (GenTxId block) => Show (HasTx block)
deriving instance   Eq (GenTxId block) =>   Eq (HasTx block)

_encodeHasTx
    :: forall block. ()
    => (GenTxId block -> Json)
    -> Rpc.Request (HasTx block)
    -> Json
_encodeHasTx encodeTxId =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        HasTx{id} ->
            "id" .= encodeTxId id

_decodeHasTx
    :: forall block. (FromJSON (GenTxId block))
    => Json.Value
    -> Json.Parser (Rpc.Request (HasTx block))
_decodeHasTx =
    Rpc.genericFromJSON Rpc.defaultOptions

data HasTxResponse
    = HasTxResponse { has :: Bool }
    deriving (Generic, Show, Eq)

_encodeHasTxResponse
    :: forall block. ()
    => Rpc.Response HasTxResponse
    -> Json
_encodeHasTxResponse =
    Rpc.mkResponse $ \case
        HasTxResponse{has} ->
            encodeBool has

--
-- SizeAndCapacity
--

data SizeAndCapacity
    = SizeAndCapacity
    deriving (Generic, Show, Eq)

_encodeSizeAndCapacity
    :: Rpc.Request SizeAndCapacity
    -> Json
_encodeSizeAndCapacity =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        SizeAndCapacity ->
            mempty

_decodeSizeAndCapacity
    :: Json.Value
    -> Json.Parser (Rpc.Request SizeAndCapacity)
_decodeSizeAndCapacity =
    Rpc.genericFromJSON Rpc.defaultOptions

data SizeAndCapacityResponse
    = SizeAndCapacityResponse { sizes :: MempoolSizeAndCapacity }
    deriving (Generic, Show)

_encodeSizeAndCapacityResponse
    :: Rpc.Response SizeAndCapacityResponse
    -> Json
_encodeSizeAndCapacityResponse =
    Rpc.mkResponse $ encodeObject . \case
        SizeAndCapacityResponse{sizes} ->
            "capacity" .=
                encodeWord32 (capacityInBytes sizes) <>
            "currentSize" .=
                encodeWord32 (sizeInBytes sizes) <>
            "numberOfTxs" .=
                encodeWord32 (numberOfTxs sizes)

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
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        ReleaseMempool ->
            mempty

_decodeReleaseMempool
    :: Json.Value
    -> Json.Parser (Rpc.Request ReleaseMempool)
_decodeReleaseMempool =
    Rpc.genericFromJSON Rpc.defaultOptions

data ReleaseMempoolResponse
    = Released
    deriving (Generic, Show)

_encodeReleaseMempoolResponse
    :: Rpc.Response ReleaseMempoolResponse
    -> Json
_encodeReleaseMempoolResponse =
    Rpc.mkResponse $ \case
        Released ->
            encodeText "Released"
