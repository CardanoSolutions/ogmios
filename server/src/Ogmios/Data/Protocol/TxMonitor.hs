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

import Ogmios.Data.Protocol
    ()

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

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Aeson.Types as Json

--
-- Codecs
--

data TxMonitorCodecs block = TxMonitorCodecs
    { decodeAwaitAcquire
        :: ByteString
        -> Maybe (Wsp.Request AwaitAcquire)
    , encodeAwaitAcquireResponse
        :: Wsp.Response AwaitAcquireResponse
        -> Json
    , decodeNextTx
        :: ByteString
        -> Maybe (Wsp.Request NextTx)
    , encodeNextTxResponse
        :: Wsp.Response (NextTxResponse block)
        -> Json
    , decodeHasTx
        :: ByteString
        -> Maybe (Wsp.Request (HasTx block))
    , encodeHasTxResponse
        :: Wsp.Response HasTxResponse
        -> Json
    , decodeSizeAndCapacity
        :: ByteString
        -> Maybe (Wsp.Request SizeAndCapacity)
    , encodeSizeAndCapacityResponse
        :: Wsp.Response SizeAndCapacityResponse
        -> Json
    , decodeReleaseMempool
        :: ByteString
        -> Maybe (Wsp.Request ReleaseMempool)
    , encodeReleaseMempoolResponse
        :: Wsp.Response ReleaseMempoolResponse
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
        (Wsp.ToResponse AwaitAcquireResponse)
        Wsp.ToFault
    | MsgNextTx
        NextTx
        (Wsp.ToResponse (NextTxResponse block))
        Wsp.ToFault
    | MsgHasTx
        (HasTx block)
        (Wsp.ToResponse HasTxResponse)
        Wsp.ToFault
    | MsgSizeAndCapacity
        SizeAndCapacity
        (Wsp.ToResponse SizeAndCapacityResponse)
        Wsp.ToFault
    | MsgReleaseMempool
        ReleaseMempool
        (Wsp.ToResponse ReleaseMempoolResponse)
        Wsp.ToFault

--
-- AwaitAcquire
--

data AwaitAcquire
    = AwaitAcquire
    deriving (Generic, Show, Eq)

_encodeAwaitAcquire
    :: Wsp.Request AwaitAcquire
    -> Json
_encodeAwaitAcquire =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        AwaitAcquire -> encodeObject []

_decodeAwaitAcquire
    :: Json.Value
    -> Json.Parser (Wsp.Request AwaitAcquire)
_decodeAwaitAcquire =
    Wsp.genericFromJSON Wsp.defaultOptions

data AwaitAcquireResponse
    = AwaitAcquired { slot :: SlotNo }
    deriving (Generic, Show, Eq)

_encodeAwaitAcquireResponse
    :: Wsp.Response AwaitAcquireResponse
    -> Json
_encodeAwaitAcquireResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        AwaitAcquired{slot} -> encodeObject
            [ ( "AwaitAcquired"
              , encodeObject [ ( "slot", encodeSlotNo slot ) ]
              )
            ]
      where
        proxy = Proxy @(Wsp.Request AwaitAcquire)

--
-- NextTx
--

data NextTx
    = NextTx { fields :: Maybe NextTxFields }
    deriving (Generic, Show, Eq)

_encodeNextTx
    :: Wsp.Request NextTx
    -> Json
_encodeNextTx =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        NextTx{fields} ->
            case fields of
                Nothing ->
                    encodeObject []
                Just NextTxAllFields ->
                    encodeObject [ ( "fields", encodeText "all" ) ]

_decodeNextTx
    :: Json.Value
    -> Json.Parser (Wsp.Request NextTx)
_decodeNextTx =
    Wsp.genericFromJSON Wsp.defaultOptions
        { Wsp.onMissingField = \case
            "fields" -> pure Json.Null
            k -> Wsp.onMissingField  Wsp.defaultOptions k
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
        NextTxAllFields -> Json.String "all"
    toEncoding = \case
        NextTxAllFields -> Json.text "all"

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
    -> Wsp.Response (NextTxResponse block)
    -> Json
_encodeNextTxResponse encodeTxId encodeTx =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        NextTxResponseId{nextId} -> encodeMaybe encodeTxId nextId
        NextTxResponseTx{nextTx} -> encodeMaybe encodeTx nextTx
  where
    proxy = Proxy @(Wsp.Request NextTx)

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
    -> Wsp.Request (HasTx block)
    -> Json
_encodeHasTx encodeTxId =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        HasTx{id} -> encodeObject [ ( "id", encodeTxId id ) ]

_decodeHasTx
    :: forall block. (FromJSON (GenTxId block))
    => Json.Value
    -> Json.Parser (Wsp.Request (HasTx block))
_decodeHasTx =
    Wsp.genericFromJSON Wsp.defaultOptions

data HasTxResponse
    = HasTxResponse { has :: Bool }
    deriving (Generic, Show, Eq)

_encodeHasTxResponse
    :: forall block. ()
    => Wsp.Response HasTxResponse
    -> Json
_encodeHasTxResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        HasTxResponse{has} -> encodeBool has
  where
    proxy = Proxy @(Wsp.Request (HasTx block))

--
-- SizeAndCapacity
--

data SizeAndCapacity
    = SizeAndCapacity
    deriving (Generic, Show, Eq)

_encodeSizeAndCapacity
    :: Wsp.Request SizeAndCapacity
    -> Json
_encodeSizeAndCapacity =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        SizeAndCapacity -> encodeObject []

_decodeSizeAndCapacity
    :: Json.Value
    -> Json.Parser (Wsp.Request SizeAndCapacity)
_decodeSizeAndCapacity =
    Wsp.genericFromJSON Wsp.defaultOptions

data SizeAndCapacityResponse
    = SizeAndCapacityResponse { sizes :: MempoolSizeAndCapacity }
    deriving (Generic, Show)

_encodeSizeAndCapacityResponse
    :: Wsp.Response SizeAndCapacityResponse
    -> Json
_encodeSizeAndCapacityResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        SizeAndCapacityResponse{sizes} -> encodeObject
            [ ( "capacity", encodeWord32 (capacityInBytes sizes) )
            , ( "currentSize", encodeWord32 (sizeInBytes sizes) )
            , ( "numberOfTxs", encodeWord32 (numberOfTxs sizes) )
            ]
  where
    proxy = Proxy @(Wsp.Request SizeAndCapacity)

--
-- ReleaseMempool
--

data ReleaseMempool
    = ReleaseMempool
    deriving (Generic, Show, Eq)

_encodeReleaseMempool
    :: Wsp.Request ReleaseMempool
    -> Json
_encodeReleaseMempool =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        ReleaseMempool -> encodeObject []

_decodeReleaseMempool
    :: Json.Value
    -> Json.Parser (Wsp.Request ReleaseMempool)
_decodeReleaseMempool =
    Wsp.genericFromJSON Wsp.defaultOptions

data ReleaseMempoolResponse
    = Released
    deriving (Generic, Show)

_encodeReleaseMempoolResponse
    :: Wsp.Response ReleaseMempoolResponse
    -> Json
_encodeReleaseMempoolResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        Released -> encodeText "Released"
  where
    proxy = Proxy @(Wsp.Request ReleaseMempool)
