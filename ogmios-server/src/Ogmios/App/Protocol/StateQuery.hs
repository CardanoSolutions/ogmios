--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

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
--
module Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient
    ) where

import Prelude

import Ogmios.Control.Exception
    ( MonadThrow )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue )
import Ogmios.Data.Json
    ( SomeQuery (..) )
import Ogmios.Data.Protocol
    ( onUnmatchedMessage )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire (..)
    , AcquireResponse (..)
    , Query (..)
    , QueryResponse (..)
    , Release (..)
    , parserVoid
    )

import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.ByteString
    ( ByteString )
import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure )

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

mkStateQueryClient
    :: forall m block point.
        ( MonadThrow m
        , MonadSTM m
        , ToJSON point
        , ToJSON AcquireFailure
        , FromJSON point
        , FromJSON (SomeQuery Maybe block)
        , point ~ Point block
        )
    => TQueue m ByteString
    -> (Json.Encoding -> m ())
    -> LocalStateQueryClient block (Point block) (Ledger.Query block) m ()
mkStateQueryClient pipe yield =
    LocalStateQueryClient clientStIdle
  where
    await :: m ByteString
    await = atomically (readTQueue pipe)

    clientStIdle
        :: m (LSQ.ClientStIdle block (Point block) (Ledger.Query block) m ())
    clientStIdle = await >>= Wsp.handle
        (\bytes -> do
            yield $ onUnmatchedMessage (parserVoid @block) bytes
            clientStIdle
        )
        [ Wsp.Handler $ \(Acquire pt) toResponse ->
            pure $ LSQ.SendMsgAcquire pt (clientStAcquiring pt toResponse)
        , Wsp.Handler $ \Release _toResponse ->
            clientStIdle
        ]

    clientStAcquiring
        :: point
        -> (AcquireResponse point -> Wsp.Response (AcquireResponse point))
        -> LSQ.ClientStAcquiring block (Point block) (Ledger.Query block) m ()
    clientStAcquiring pt toResponse = LSQ.ClientStAcquiring
        { LSQ.recvMsgAcquired = do
            yield $ Json.toEncoding $ toResponse $ AcquireSuccess pt
            clientStAcquired
        , LSQ.recvMsgFailure = \failure -> do
            yield $ Json.toEncoding $ toResponse $ AcquireFailed failure
            clientStIdle
        }

    clientStAcquired
        :: m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
    clientStAcquired = await >>= Wsp.handle
        (\bytes -> do
            yield $ onUnmatchedMessage (parserVoid @block) bytes
            clientStAcquired
        )
        [ Wsp.Handler $ \(Acquire pt) toResponse ->
            pure $ LSQ.SendMsgReAcquire pt (clientStAcquiring pt toResponse)
        , Wsp.Handler $ \Release _toResponse ->
            pure $ LSQ.SendMsgRelease clientStIdle
        , Wsp.Handler $ \(Query (SomeQuery query encodeResult _)) toResponse ->
            pure $ LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \result -> do
                    let response = toResponse $ QueryResponse $ encodeResult result
                    yield $ Json.toEncoding response
                    clientStAcquired
                }
        ]
