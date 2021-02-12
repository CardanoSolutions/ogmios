--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

-- | The state query protocol is likely the most versatile of the three Ouroboros
-- mini-protocols. As a matter of fact, it allows for querying various types of
-- information directly from the ledger. In essence, it is like a very simpler
-- request/response pattern where the types of questions one can ask are
-- specified by the protocols. Those questions include: information about the
-- chain tip, information about stake pools but also the balance of a particular
-- address.
--
-- In order to run a question by the ledger, one must first acquire a particular
-- position on the chain, so that the node can reliably answer a few questions
-- on a chosen, frozen state while continuing maintaining more recent version of
-- the ledger on the side. It is important to note that:
--
-- 1. The node cannot acquire any arbitrary state. One can only rewind up
--    to a certain point.
--
-- 2. Should a client keep a state acquired for too long, it is likely to become
--    unreachable at some point, forcing clients to re-acquire.
--
-- @
--                     ┌───────────────┐
--             ┌──────▶│     Idle      │⇦ START
--             │       └───┬───────────┘
--             │           │       ▲
--             │   Acquire │       │ Failure
--             │           ▼       │
--             │       ┌───────────┴───┐
--     Release │       │   Acquiring   │◀─────────────────┐
--             │       └───┬───────────┘                  │
--             │           │       ▲                      │ Result
--             │  Acquired │       │ ReAcquire            │
--             │           ▼       │                      │
--             │       ┌───────────┴───┐         ┌────────┴───────┐
--             └───────┤   Acquired    │────────▶│    Querying    │
--                     └───────────────┘         └────────────────┘
-- @
module Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient
    ) where

import Relude hiding
    ( atomically )

import Ogmios.Control.Exception
    ( MonadThrow )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Json.Query
    ( SomeQuery (..) )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire (..)
    , AcquireResponse (..)
    , Query (..)
    , QueryResponse (..)
    , Release (..)
    , ReleaseResponse (..)
    , StateQueryCodecs (..)
    , StateQueryMessage (..)
    )

import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

mkStateQueryClient
    :: forall m block.
        ( MonadThrow m
        , MonadSTM m
        )
    => StateQueryCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (StateQueryMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalStateQueryClient block (Point block) (Ledger.Query block) m ()
mkStateQueryClient StateQueryCodecs{..} queue yield =
    LocalStateQueryClient clientStIdle
  where
    await :: m (StateQueryMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (LSQ.ClientStIdle block (Point block) (Ledger.Query block) m ())
    clientStIdle = await >>= \case
        MsgAcquire (Acquire pt) toResponse ->
            pure $ LSQ.SendMsgAcquire (Just pt) (clientStAcquiring pt toResponse)
        MsgRelease Release toResponse -> do
            yield $ encodeReleaseResponse (toResponse Released)
            clientStIdle
        MsgQuery query toResponse -> do
            pure $ LSQ.SendMsgAcquire Nothing (clientStAcquiringTip query toResponse)

    clientStAcquiring
        :: Point block
        -> Wsp.ToResponse (AcquireResponse block)
        -> LSQ.ClientStAcquiring block (Point block) (Ledger.Query block) m ()
    clientStAcquiring pt toResponse =
        LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                yield $ encodeAcquireResponse $ toResponse $ AcquireSuccess pt
                clientStAcquired
            , LSQ.recvMsgFailure = \failure -> do
                yield $ encodeAcquireResponse $ toResponse $ AcquireFailure failure
                clientStIdle
            }

    clientStAcquiringTip
        :: Query block
        -> Wsp.ToResponse (QueryResponse block)
        -> LSQ.ClientStAcquiring block (Point block) (Ledger.Query block) m ()
    clientStAcquiringTip (Query (SomeQuery query encodeResult _)) toResponse =
        LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                pure $ LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                    { LSQ.recvMsgResult = \result -> do
                        let response = QueryResponse $ encodeResult result
                        yield $ encodeQueryResponse $ toResponse response
                        pure $ LSQ.SendMsgRelease clientStIdle
                    }
            , LSQ.recvMsgFailure = \failure -> do
                let response = QueryAcquireFailure failure
                yield $ encodeQueryResponse $ toResponse response
                clientStIdle
            }

    clientStAcquired
        :: m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
    clientStAcquired = await >>= \case
        MsgAcquire (Acquire pt) toResponse ->
            pure $ LSQ.SendMsgReAcquire (Just pt) (clientStAcquiring pt toResponse)
        MsgRelease Release toResponse -> do
            yield $ encodeReleaseResponse (toResponse Released)
            pure $ LSQ.SendMsgRelease clientStIdle
        MsgQuery (Query (SomeQuery query encodeResult _)) toResponse ->
            pure $ LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \result -> do
                    let response = QueryResponse $ encodeResult result
                    yield $ encodeQueryResponse $ toResponse response
                    clientStAcquired
                }
