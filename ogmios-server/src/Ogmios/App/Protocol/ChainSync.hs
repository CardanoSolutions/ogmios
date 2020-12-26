--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

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
--
module Ogmios.App.Protocol.ChainSync
    ( mkChainSyncClient
    , mkHealthCheckClient
    ) where

import Prelude

import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue, tryReadTQueue )
import Ogmios.Data.Protocol
    ( onUnmatchedMessage )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersect (..)
    , FindIntersectResponse (..)
    , RequestNext (..)
    , RequestNextResponse (..)
    , parserVoid
    )

import Control.Monad
    ( guard )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.Sequence
    ( Seq (..), (|>) )
import Network.TypedProtocol.Pipelined
    ( N (..), Nat (..), natToInt )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, getTipPoint )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Sequence as Seq

-- | Because:
--
-- - Each request may contain data that must be mirrored in the response
-- - Requests are pipelined and may be collected _later_
--
-- We can't simply 'yield' response out of any context. Instead, to yield a response,
-- we must remember how to construct the adequate response from when we got the
-- request.
type PendingResponse block =
    RequestNextResponse block -> Wsp.Response (RequestNextResponse block)

mkChainSyncClient
    :: forall m block.
        ( FromJSON (Point block)
        , ToJSON block
        , ToJSON (Point block)
        , ToJSON (Tip block)
        , MonadSTM m
        )
    => TQueue m ByteString
    -> (Json.Encoding -> m ())
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkChainSyncClient pipe yield =
    ChainSyncClientPipelined $ clientStIdle Zero Seq.Empty
  where
    await :: m ByteString
    await = atomically (readTQueue pipe)

    tryAwait :: m (Maybe ByteString)
    tryAwait = atomically (tryReadTQueue pipe)

    clientStIdle
        :: forall n. ()
        => Nat n
        -> Seq (PendingResponse block)
        -> m (ClientPipelinedStIdle n block (Point block) (Tip block) m ())
    clientStIdle Zero buffer = await >>= Wsp.handle
        (\bytes -> do
            yield $ onUnmatchedMessage (parserVoid @block) bytes
            clientStIdle Zero buffer
        )
        [ Wsp.Handler $ \FindIntersect{points} ->
            pure . SendMsgFindIntersect points . clientStIntersect

        , Wsp.Handler $ \RequestNext toResponse -> do
            let buffer' = buffer |> toResponse
            let collect = CollectResponse
                    (Just $ clientStIdle (Succ Zero) buffer')
                    (clientStNext Zero buffer')
            pure $ SendMsgRequestNextPipelined collect
        ]
    clientStIdle n@(Succ prev) buffer = tryAwait >>= \case
        -- If there's no immediate incoming message, we take this opportunity to
        -- wait and collect one response.
        Nothing | Seq.null buffer ->
            clientStIdle n buffer

        Nothing ->
            pure $ CollectResponse Nothing (clientStNext prev buffer)

        -- Yet, if we have already received a new message from the client, we
        -- prioritize it and pipeline it right away unless there are already too
        -- many requests in flights.
        Just msg -> Wsp.handle
            (\bytes -> do
                yield $ onUnmatchedMessage (parserVoid @block) bytes
                clientStIdle n buffer
            )
            [ Wsp.Handler $ \RequestNext toResponse -> do
                let buffer' = buffer |> toResponse
                let collect = CollectResponse
                        (guard (natToInt n < 1000) $> clientStIdle (Succ n) buffer')
                        (clientStNext n buffer')
                pure $ SendMsgRequestNextPipelined collect
            ] msg

    clientStNext
        :: Nat n
        -> Seq (PendingResponse block)
        -> ClientStNext n block (Point block) (Tip block) m ()
    clientStNext _ Empty =
        error "invariant violation: empty buffer on clientStNext"
    clientStNext n (toResponse :<| buffer) =
        ClientStNext
            { recvMsgRollForward = \block tip -> do
                yield $ Json.toEncoding $ toResponse $ RollForward block tip
                clientStIdle n buffer
            , recvMsgRollBackward = \point tip -> do
                yield $ Json.toEncoding $ toResponse $ RollBackward point tip
                clientStIdle n buffer
            }

    clientStIntersect
        :: (FindIntersectResponse block -> Wsp.Response (FindIntersectResponse block))
        -> ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    clientStIntersect toResponse = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> do
            yield $ Json.toEncoding $ toResponse $ IntersectionFound point tip
            clientStIdle Zero Seq.empty
        , recvMsgIntersectNotFound = \tip -> do
            yield $ Json.toEncoding $ toResponse $ IntersectionNotFound tip
            clientStIdle Zero Seq.empty
        }

-- | Simple client that follows the chain by jumping directly to the tip and
-- notify a consumer for every tip change.
mkHealthCheckClient
    :: forall m block.
        ( Monad m
        )
    => (Tip block -> m ())
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkHealthCheckClient notify =
    ChainSyncClientPipelined stInit
  where
    stInit
        :: m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ())
    stInit = pure $
        SendMsgFindIntersect [genesisPoint] $ stIntersect $ \tip -> pure $
            SendMsgFindIntersect [getTipPoint tip] $ stIntersect (const stIdle)

    stIntersect
        :: (Tip block -> m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ()))
        -> ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    stIntersect stFound = ClientPipelinedStIntersect
        { recvMsgIntersectNotFound = const stInit
        , recvMsgIntersectFound = const stFound
        }

    stIdle
        :: m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ())
    stIdle = pure $
        SendMsgRequestNext stNext (pure stNext)

    stNext
        :: ClientStNext Z block (Point block) (Tip block) m ()
    stNext = ClientStNext
        { recvMsgRollForward  = const check
        , recvMsgRollBackward = const check
        }
      where
        check tip = notify tip *> stIdle
