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


      -- ** GetSizes
    , GetSizes (..)
    , _encodeGetSizes
    , _decodeGetSizes
    , GetSizesResponse (..)
    , _encodeGetSizesResponse

      -- ** Releases
    , Release (..)
    , _encodeRelease
    , _decodeRelease
    , ReleaseResponse (..)
    , _encodeReleaseResponse

     -- * Re-exports
    , GenTx
    , GenTxId
    , MempoolSizeAndCapacity (..)
    , SlotNo (..)
    ) where

import Ogmios.Data.Json.Prelude hiding
    ( id )

import Ogmios.Data.Protocol
    ()

import Cardano.Network.Protocol.NodeToClient
    ( GenTx, GenTxId )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
    ( MempoolSizeAndCapacity (..) )

import qualified Codec.Json.Wsp as Wsp
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
    , decodeGetSizes
        :: ByteString
        -> Maybe (Wsp.Request GetSizes)
    , encodeGetSizesResponse
        :: Wsp.Response GetSizesResponse
        -> Json
    , decodeRelease
        :: ByteString
        -> Maybe (Wsp.Request Release)
    , encodeReleaseResponse
        :: Wsp.Response ReleaseResponse
        -> Json
    }

mkTxMonitorCodecs
    :: (FromJSON (GenTxId block))
    => (GenTx block -> Json)
    -> TxMonitorCodecs block
mkTxMonitorCodecs encodeTx =
    TxMonitorCodecs
        { decodeAwaitAcquire = decodeWith _decodeAwaitAcquire
        , encodeAwaitAcquireResponse = _encodeAwaitAcquireResponse
        , decodeNextTx = decodeWith _decodeNextTx
        , encodeNextTxResponse = _encodeNextTxResponse encodeTx
        , decodeHasTx = decodeWith _decodeHasTx
        , encodeHasTxResponse = _encodeHasTxResponse
        , decodeGetSizes = decodeWith _decodeGetSizes
        , encodeGetSizesResponse = _encodeGetSizesResponse
        , decodeRelease = decodeWith _decodeRelease
        , encodeReleaseResponse = _encodeReleaseResponse
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
    | MsgGetSizes
        GetSizes
        (Wsp.ToResponse GetSizesResponse)
        Wsp.ToFault
    | MsgRelease
        Release
        (Wsp.ToResponse ReleaseResponse)
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
    = NextTx
    deriving (Generic, Show, Eq)

_encodeNextTx
    :: Wsp.Request NextTx
    -> Json
_encodeNextTx =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        NextTx -> encodeObject
            [ ( "NextTx", encodeObject [] )
            ]

_decodeNextTx
    :: Json.Value
    -> Json.Parser (Wsp.Request NextTx)
_decodeNextTx =
    Wsp.genericFromJSON Wsp.defaultOptions

data NextTxResponse block
    = NextTxResponse { next :: Maybe (GenTx block) }
    deriving (Generic)
deriving instance Show (GenTx block) => Show (NextTxResponse block)
deriving instance   Eq (GenTx block) =>   Eq (NextTxResponse block)

_encodeNextTxResponse
    :: forall block. ()
    => (GenTx block -> Json)
    -> Wsp.Response (NextTxResponse block)
    -> Json
_encodeNextTxResponse encodeTx =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        NextTxResponse{next} -> encodeMaybe encodeTx next
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
        HasTx{id} -> encodeObject
            [ ( "HasTx"
              , encodeObject [ ( "id", encodeTxId id ) ]
              )
            ]

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
-- GetSizes
--

data GetSizes
    = GetSizes
    deriving (Generic, Show, Eq)

_encodeGetSizes
    :: Wsp.Request GetSizes
    -> Json
_encodeGetSizes =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        GetSizes -> encodeObject []

_decodeGetSizes
    :: Json.Value
    -> Json.Parser (Wsp.Request GetSizes)
_decodeGetSizes =
    Wsp.genericFromJSON Wsp.defaultOptions

data GetSizesResponse
    = GetSizesResponse { sizes :: MempoolSizeAndCapacity }
    deriving (Generic, Show)

_encodeGetSizesResponse
    :: Wsp.Response GetSizesResponse
    -> Json
_encodeGetSizesResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        GetSizesResponse{sizes} -> encodeObject
            [ ( "capacity", encodeWord32 (capacityInBytes sizes) )
            , ( "currentSize", encodeWord32 (sizeInBytes sizes) )
            , ( "numberOfTxs", encodeWord32 (numberOfTxs sizes) )
            ]
  where
    proxy = Proxy @(Wsp.Request GetSizes)

--
-- Release
--

data Release
    = Release
    deriving (Generic, Show, Eq)

_encodeRelease
    :: Wsp.Request Release
    -> Json
_encodeRelease =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        Release -> encodeObject []

_decodeRelease
    :: Json.Value
    -> Json.Parser (Wsp.Request Release)
_decodeRelease =
    Wsp.genericFromJSON Wsp.defaultOptions

data ReleaseResponse
    = Released
    deriving (Generic, Show)

_encodeReleaseResponse
    :: Wsp.Response ReleaseResponse
    -> Json
_encodeReleaseResponse =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        Released -> encodeText "Released"
  where
    proxy = Proxy @(Wsp.Request Release)
