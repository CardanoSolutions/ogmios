--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Protocol.ChainSync
    ( -- * Codecs
      ChainSyncCodecs (..)
    , mkChainSyncCodecs

      -- * Messages
    , ChainSyncMessage (..)

      -- ** FindIntersect
    , FindIntersect (..)
    , _encodeFindIntersect
    , _decodeFindIntersect
    , FindIntersectResponse (..)
    , _encodeFindIntersectResponse

      -- ** RequestNext
    , RequestNext (..)
    , _encodeRequestNext
    , _decodeRequestNext
    , RequestNextResponse (..)
    , _encodeRequestNextResponse
    ) where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Protocol
    ()

import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson.Types as Json

--
-- Codecs
--

data ChainSyncCodecs block = ChainSyncCodecs
    { decodeFindIntersect
        :: ByteString
        -> Maybe (Wsp.Request (FindIntersect block))
    , encodeFindIntersectResponse
        :: Wsp.Response (FindIntersectResponse block)
        -> Json
    , decodeRequestNext
        :: ByteString
        -> Maybe (Wsp.Request RequestNext)
    , encodeRequestNextResponse
        :: Wsp.Response (RequestNextResponse block)
        -> Json
    }

mkChainSyncCodecs
    :: (FromJSON (Point block))
    => (block -> Json)
    -> (Point block -> Json)
    -> (Tip block -> Json)
    -> ChainSyncCodecs block
mkChainSyncCodecs encodeBlock encodePoint encodeTip =
    ChainSyncCodecs
        { decodeFindIntersect =
            decodeWith _decodeFindIntersect
        , encodeFindIntersectResponse =
            _encodeFindIntersectResponse encodePoint encodeTip
        , decodeRequestNext =
            decodeWith _decodeRequestNext
        , encodeRequestNextResponse =
            _encodeRequestNextResponse encodeBlock encodePoint encodeTip
        }

--
-- ChainSyncMessage
--

data ChainSyncMessage block
    = MsgFindIntersect
        (FindIntersect block)
        (Wsp.ToResponse (FindIntersectResponse block))
        Wsp.ToFault
    | MsgRequestNext
        RequestNext
        (Wsp.ToResponse (RequestNextResponse block))
        Wsp.ToFault

--
-- FindIntersect
--

data FindIntersect block
    = FindIntersect { points :: [Point block] }
    deriving (Generic, Show, Eq)

_encodeFindIntersect
    :: forall block. ()
    => (Point block -> Json)
    -> Wsp.Request (FindIntersect block)
    -> Json
_encodeFindIntersect encodePoint =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        FindIntersect{points} -> encodeObject
            [ ( "points", encodeList encodePoint points )
            ]

_decodeFindIntersect
    :: FromJSON (Point block)
    => Json.Value
    -> Json.Parser (Wsp.Request (FindIntersect block))
_decodeFindIntersect =
    Wsp.genericFromJSON Wsp.defaultOptions

data FindIntersectResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

_encodeFindIntersectResponse
    :: forall block. ()
    => (Point block -> Json)
    -> (Tip block -> Json)
    -> Wsp.Response (FindIntersectResponse block)
    -> Json
_encodeFindIntersectResponse encodePoint encodeTip =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        IntersectionFound{point,tip} -> encodeObject
            [ ("IntersectionFound", encodeObject
                [ ("point", encodePoint point)
                , ("tip", encodeTip tip)
                ]
              )
            ]
        IntersectionNotFound{tip} -> encodeObject
            [ ("IntersectionNotFound", encodeObject
                [ ("tip", encodeTip tip)
                ]
              )
            ]
  where
    proxy = Proxy @(Wsp.Request (FindIntersect block))

--
-- Requestnext
--

data RequestNext
    = RequestNext
    deriving (Generic, Show, Eq)

_encodeRequestNext
    :: Wsp.Request RequestNext
    -> Json
_encodeRequestNext =
    Wsp.mkRequest Wsp.defaultOptions $ \case
        RequestNext -> encodeObject []

_decodeRequestNext
    :: Json.Value
    -> Json.Parser (Wsp.Request RequestNext)
_decodeRequestNext =
    Wsp.genericFromJSON Wsp.defaultOptions

data RequestNextResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

_encodeRequestNextResponse
    :: (block -> Json)
    -> (Point block -> Json)
    -> (Tip block -> Json)
    -> Wsp.Response (RequestNextResponse block)
    -> Json
_encodeRequestNextResponse encodeBlock encodePoint encodeTip =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        RollForward{block,tip} -> encodeObject
            [ ("RollForward", encodeObject
                [ ("block", encodeBlock block)
                , ("tip", encodeTip tip)
                ]
              )
            ]
        RollBackward{point,tip} -> encodeObject
            [ ("RollBackward", encodeObject
                [ ("point", encodePoint point)
                , ("tip", encodeTip tip)
                ]
              )
            ]
  where
    proxy = Proxy @(Wsp.Request RequestNext)
