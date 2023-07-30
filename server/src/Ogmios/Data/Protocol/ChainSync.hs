--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}

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

      -- ** FindIntersection
    , FindIntersection (..)
    , _encodeFindIntersection
    , _decodeFindIntersection
    , FindIntersectionResponse (..)
    , _encodeFindIntersectionResponse

      -- ** NextBlock
    , NextBlock (..)
    , _encodeNextBlock
    , _decodeNextBlock
    , NextBlockResponse (..)
    , _encodeNextBlockResponse
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
    { decodeFindIntersection
        :: ByteString
        -> Maybe (Rpc.Request (FindIntersection block))
    , encodeFindIntersectionResponse
        :: Rpc.Response (FindIntersectionResponse block)
        -> Json
    , decodeNextBlock
        :: ByteString
        -> Maybe (Rpc.Request NextBlock)
    , encodeNextBlockResponse
        :: Rpc.Response (NextBlockResponse block)
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
        { decodeFindIntersection =
            decodeWith _decodeFindIntersection
        , encodeFindIntersectionResponse =
            _encodeFindIntersectionResponse encodePoint encodeTip
        , decodeNextBlock =
            decodeWith _decodeNextBlock
        , encodeNextBlockResponse =
            _encodeNextBlockResponse encodeBlock encodePoint encodeTip
        }

--
-- ChainSyncMessage
--

data ChainSyncMessage block
    = MsgFindIntersection
        (FindIntersection block)
        (Rpc.ToResponse (FindIntersectionResponse block))
        Rpc.ToFault
    | MsgNextBlock
        NextBlock
        (Rpc.ToResponse (NextBlockResponse block))
        Rpc.ToFault

--
-- FindIntersection
--

data FindIntersection block
    = FindIntersection { points :: [Point block] }
    deriving (Generic, Show, Eq)

_encodeFindIntersection
    :: forall block. ()
    => (Point block -> Json)
    -> Rpc.Request (FindIntersection block)
    -> Json
_encodeFindIntersection encodePoint =
    Rpc.mkRequest $ encodeObject . \case
        FindIntersection{points} ->
            "points" .=
                encodeList encodePoint points

_decodeFindIntersection
    :: FromJSON (Point block)
    => Json.Value
    -> Json.Parser (Rpc.Request (FindIntersection block))
_decodeFindIntersection =
    Rpc.genericFromJSON Rpc.defaultOptions

data FindIntersectionResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

_encodeFindIntersectionResponse
    :: forall block. ()
    => (Point block -> Json)
    -> (Tip block -> Json)
    -> Rpc.Response (FindIntersectionResponse block)
    -> Json
_encodeFindIntersectionResponse encodePoint encodeTip =
    Rpc.mkResponse $ \resolve reject -> \case
        IntersectionFound{point,tip} ->
            resolve $ encodeObject
                ( "intersection" .= encodePoint point <>
                  "tip" .= encodeTip tip
                )
        IntersectionNotFound{tip} ->
            reject (Rpc.FaultCustom 1000)
                "No intersection found."
                (Just $ encodeObject
                    ( "tip" .= encodeTip tip
                    )
                )

--
-- Requestnext
--

data NextBlock
    = NextBlock
    deriving (Generic, Show, Eq)

_encodeNextBlock
    :: Rpc.Request NextBlock
    -> Json
_encodeNextBlock =
    Rpc.mkRequestNoParams

_decodeNextBlock
    :: Json.Value
    -> Json.Parser (Rpc.Request NextBlock)
_decodeNextBlock =
    Rpc.genericFromJSON Rpc.defaultOptions

data NextBlockResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

_encodeNextBlockResponse
    :: (block -> Json)
    -> (Point block -> Json)
    -> (Tip block -> Json)
    -> Rpc.Response (NextBlockResponse block)
    -> Json
_encodeNextBlockResponse encodeBlock encodePoint encodeTip =
    Rpc.ok $ encodeObject . \case
        RollForward{block,tip} ->
            ( "direction" .= encodeText "forward" <>
              "block" .= encodeBlock block <>
              "tip" .= encodeTip tip
            )
        RollBackward{point,tip} ->
            ( "direction" .= encodeText "backward" <>
              "point" .= encodePoint point <>
              "tip" .= encodeTip tip
            )
