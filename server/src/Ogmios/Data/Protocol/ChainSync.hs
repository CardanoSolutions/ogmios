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

import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Types as Json

--
-- Codecs
--

data ChainSyncCodecs block = ChainSyncCodecs
    { decodeFindIntersect
        :: ByteString
        -> Maybe (Rpc.Request (FindIntersect block))
    , encodeFindIntersectResponse
        :: Rpc.Response (FindIntersectResponse block)
        -> Json
    , decodeRequestNext
        :: ByteString
        -> Maybe (Rpc.Request RequestNext)
    , encodeRequestNextResponse
        :: Rpc.Response (RequestNextResponse block)
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
        (Rpc.ToResponse (FindIntersectResponse block))
        Rpc.ToFault
    | MsgRequestNext
        RequestNext
        (Rpc.ToResponse (RequestNextResponse block))
        Rpc.ToFault

--
-- FindIntersect
--

data FindIntersect block
    = FindIntersect { points :: [Point block] }
    deriving (Generic, Show, Eq)

_encodeFindIntersect
    :: forall block. ()
    => (Point block -> Json)
    -> Rpc.Request (FindIntersect block)
    -> Json
_encodeFindIntersect encodePoint =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        FindIntersect{points} ->
            "points" .=
                encodeList encodePoint points

_decodeFindIntersect
    :: FromJSON (Point block)
    => Json.Value
    -> Json.Parser (Rpc.Request (FindIntersect block))
_decodeFindIntersect =
    Rpc.genericFromJSON Rpc.defaultOptions

data FindIntersectResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

_encodeFindIntersectResponse
    :: forall block. ()
    => (Point block -> Json)
    -> (Tip block -> Json)
    -> Rpc.Response (FindIntersectResponse block)
    -> Json
_encodeFindIntersectResponse encodePoint encodeTip =
    Rpc.mkResponse $ encodeObject . \case
        IntersectionFound{point,tip} ->
            "IntersectionFound" .= encodeObject
                ( "point" .= encodePoint point <>
                  "tip" .= encodeTip tip
                )
        IntersectionNotFound{tip} ->
            "IntersectionNotFound" .= encodeObject
                ( "tip" .= encodeTip tip
                )

--
-- Requestnext
--

data RequestNext
    = RequestNext
    deriving (Generic, Show, Eq)

_encodeRequestNext
    :: Rpc.Request RequestNext
    -> Json
_encodeRequestNext =
    Rpc.mkRequest Rpc.defaultOptions $ \case
        RequestNext ->
            encodeObject mempty

_decodeRequestNext
    :: Json.Value
    -> Json.Parser (Rpc.Request RequestNext)
_decodeRequestNext =
    Rpc.genericFromJSON Rpc.defaultOptions

data RequestNextResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

_encodeRequestNextResponse
    :: (block -> Json)
    -> (Point block -> Json)
    -> (Tip block -> Json)
    -> Rpc.Response (RequestNextResponse block)
    -> Json
_encodeRequestNextResponse encodeBlock encodePoint encodeTip =
    Rpc.mkResponse $ encodeObject . \case
        RollForward{block,tip} ->
            "RollForward" .= encodeObject
                ( "block" .= encodeBlock block <>
                  "tip" .= encodeTip tip
                )
        RollBackward{point,tip} ->
            "RollBackward" .= encodeObject
                ( "point" .= encodePoint point <>
                  "tip" .= encodeTip tip
                )
