--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- | Clients that wish to synchronise blocks from the Cardano chain can use the
-- Local Chain Sync protocol.
--
-- The protocol is stateful, which means that each connection between clients and
-- Ogmios has a state: a  cursor locating a point on the chain. Typically, a
-- client will  start by looking for an intersection between its own local chain
-- and the one from the node / Ogmios.
--
-- Then, it'll simply request the next action to take: either rolling forward
-- and adding new blocks, or rolling backward.
--
-- @
--     ┌───────────┐
--     │ Intersect │◀══════════════════════════════╗
--     └─────┬─────┘         FindIntersect         ║
--           │                                     ║
--           │                                ┌──────────┐
--           │ Intersect.{Found,NotFound}     │          │
--           └───────────────────────────────▶│          │
--                                            │   Idle   │
--        ╔═══════════════════════════════════│          │
--        ║            RequestNext            │          │⇦ START
--        ║                                   └──────────┘
--        ▼                                        ▲
--     ┌──────┐       Roll.{Backward,Forward}      │
--     │ Next ├────────────────────────────────────┘
--     └──────┘
-- @
module Ogmios.App.Protocol.ChainSync
    ( mkChainSyncClient
    , MaxInFlight
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TQueue
    , readTQueue
    , tryReadTQueue
    )
import Ogmios.Data.Json
    ( Json
    )
import Ogmios.Data.Protocol.ChainSync
    ( ChainSyncCodecs (..)
    , ChainSyncMessage (..)
    , FindIntersect (..)
    , FindIntersectResponse (..)
    , RequestNext (..)
    , RequestNextResponse (..)
    )

import Data.Sequence
    ( Seq (..)
    , (|>)
    )
import Network.TypedProtocol.Pipelined
    ( Nat (..)
    , natToInt
    )
import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Sequence as Seq

type MaxInFlight = Int

mkChainSyncClient
    :: forall m block.
        ( MonadSTM m
        )
    => MaxInFlight
        -- ^ Max number of requests allowed to be in-flight / pipelined
    -> ChainSyncCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (ChainSyncMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkChainSyncClient maxInFlight ChainSyncCodecs{..} queue yield =
    ChainSyncClientPipelined $ clientStIdle Zero Seq.Empty
  where
    await :: m (ChainSyncMessage block)
    await = atomically (readTQueue queue)

    tryAwait :: m (Maybe (ChainSyncMessage block))
    tryAwait = atomically (tryReadTQueue queue)

    clientStIdle
        :: forall n. ()
        => Nat n
        -> Seq (Wsp.ToResponse (RequestNextResponse block))
        -> m (ClientPipelinedStIdle n block (Point block) (Tip block) m ())
    clientStIdle Zero buffer = await <&> \case
        MsgRequestNext RequestNext toResponse _ ->
            let buffer' = buffer |> toResponse
                collect = CollectResponse
                    (Just $ clientStIdle (Succ Zero) buffer')
                    (clientStNext Zero buffer')
            in SendMsgRequestNextPipelined collect

        MsgFindIntersect FindIntersect{points} toResponse _ ->
            SendMsgFindIntersect points (clientStIntersect toResponse)

    clientStIdle n@(Succ prev) buffer = tryAwait >>= \case
        -- If there's no immediate incoming message, we take this opportunity to
        -- wait and collect one response.
        Nothing ->
            pure $ CollectResponse Nothing (clientStNext prev buffer)

        -- Yet, if we have already received a new message from the client, we
        -- prioritize it and pipeline it right away unless there are already too
        -- many requests in flights.
        Just (MsgRequestNext RequestNext toResponse _) -> do
            let buffer' = buffer |> toResponse
            let collect = CollectResponse
                    (guard (natToInt n < maxInFlight) $> clientStIdle (Succ n) buffer')
                    (clientStNext n buffer')
            pure $ SendMsgRequestNextPipelined collect

        Just (MsgFindIntersect _FindIntersect _toResponse toFault) -> do
            let fault = "'FindIntersect' requests cannot be interleaved with 'RequestNext'."
            yield $ Wsp.mkFault $ toFault Wsp.FaultClient fault
            clientStIdle n buffer

    clientStNext
        :: Nat n
        -> Seq (Wsp.ToResponse (RequestNextResponse block))
        -> ClientStNext n block (Point block) (Tip block) m ()
    clientStNext _ Empty =
        error "invariant violation: empty buffer on clientStNext"
    clientStNext n (toResponse :<| buffer) =
        ClientStNext
            { recvMsgRollForward = \block tip -> do
                yield $ encodeRequestNextResponse $ toResponse $ RollForward block tip
                clientStIdle n buffer
            , recvMsgRollBackward = \point tip -> do
                yield $ encodeRequestNextResponse $ toResponse $ RollBackward point tip
                clientStIdle n buffer
            }

    clientStIntersect
        :: Wsp.ToResponse (FindIntersectResponse block)
        -> ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    clientStIntersect toResponse = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> do
            yield $ encodeFindIntersectResponse $ toResponse $ IntersectionFound point tip
            clientStIdle Zero Seq.empty
        , recvMsgIntersectNotFound = \tip -> do
            yield $ encodeFindIntersectResponse $ toResponse $ IntersectionNotFound tip
            clientStIdle Zero Seq.empty
        }
