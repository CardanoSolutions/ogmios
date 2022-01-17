--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- This is used by local clients (like wallets, explorers and CLI tools) to
-- monitor the transactions passing through the mempool of a local node.
--
-- The protocol is stateful such that the server keeps track of the transactions
-- already sent to the client.
--
-- @
--                    START
--                      ⇓
--                    ┌───────────────┐
--            ┌──────▶│     Idle      │⇒ DONE
--            │       └───┬───────────┘
--            │           │
--            │   Acquire │
--            │           ▼
--            │       ┌───────────────┐
--    Release │       │   Acquiring   │
--            │       └───┬───────────┘
--            │           │       ▲
--            │  Acquired │       │ AwaitAcquire
--            │           ▼       │
--            │       ┌───────────┴───┐
--            └───────┤   Acquired    │
--                    └───┬───────────┘
--                        │       ▲
--  HasTx|NextTx|GetSizes │       │ Reply (HasTx|NextTx|GetSizes)
--                        ▼       │
--                    ┌───────────┴───┐
--                    │      Busy     │
--                    └───────────────┘
-- @
--
-- Note that Ogmios enables a slightly modified version of that protocol where
-- `HasTx`, `NextTx` and `GetSizes` can be sent right away. This effectively
-- performs an implicit `Acquire` conviniently. From there, the protocol behaves
-- identically.
module Ogmios.App.Protocol.TxMonitor
    ( mkTxMonitorClient
    ) where

import Ogmios.Prelude hiding
    ( id )

import Ogmios.Control.MonadSTM
    ( MonadSTM (..) )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Protocol.TxMonitor
    ( AwaitAcquire (..)
    , AwaitAcquireResponse (..)
    , GenTx
    , GenTxId
    , GetSizes (..)
    , GetSizesResponse (..)
    , HasTx (..)
    , HasTxResponse (..)
    , NextTx (..)
    , NextTxResponse (..)
    , Release (..)
    , ReleaseResponse (..)
    , SlotNo (..)
    , TxMonitorCodecs (..)
    , TxMonitorMessage (..)
    )

import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( ClientStAcquired (..), ClientStIdle (..), LocalTxMonitorClient (..) )

import qualified Codec.Json.Wsp as Wsp

mkTxMonitorClient
    :: forall m block.
        ( MonadSTM m
        )
    => TxMonitorCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (TxMonitorMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
mkTxMonitorClient TxMonitorCodecs{..} queue yield =
    LocalTxMonitorClient clientStIdle
  where
    await :: m (TxMonitorMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (ClientStIdle (GenTxId block) (GenTx block) SlotNo m ())
    clientStIdle = await >>= \case
        MsgAwaitAcquire AwaitAcquire toResponse _ ->
            pure $ SendMsgAcquire $ \slot -> do
                yield $ encodeAwaitAcquireResponse $ toResponse $ AwaitAcquired slot
                clientStAcquired
        MsgNextTx NextTx toResponse _ ->
            pure $ SendMsgAcquire $ \_slot -> do
                pure $ sendMsgNextTx NextTx toResponse
        MsgHasTx HasTx{id} toResponse _ ->
            pure $ SendMsgAcquire $ \_slot -> do
                pure $ sendMsgHasTx HasTx{id} toResponse
        MsgGetSizes GetSizes toResponse _ -> do
            pure $ SendMsgAcquire $ \_slot -> do
                pure $ sendMsgGetSizes GetSizes toResponse
        MsgRelease Release _ toFault -> do
            let fault = "'Release' must be call after acquiring some state."
            yield $ Wsp.mkFault $ toFault Wsp.FaultClient fault
            clientStIdle

    clientStAcquired
        :: m (ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
    clientStAcquired = await <&> \case
        MsgAwaitAcquire AwaitAcquire toResponse _ ->
            SendMsgAwaitAcquire $ \slot -> do
                yield $ encodeAwaitAcquireResponse $ toResponse $ AwaitAcquired slot
                clientStAcquired
        MsgNextTx NextTx toResponse _ ->
            sendMsgNextTx NextTx toResponse
        MsgHasTx HasTx{id} toResponse _ ->
            sendMsgHasTx HasTx{id} toResponse
        MsgGetSizes GetSizes toResponse _ ->
            sendMsgGetSizes GetSizes toResponse
        MsgRelease Release toResponse _ ->
            SendMsgRelease $ do
                yield $ encodeReleaseResponse $ toResponse Released
                clientStIdle

    sendMsgNextTx
        :: NextTx
        -> (Wsp.ToResponse (NextTxResponse block))
        -> ClientStAcquired (GenTxId block) (GenTx block) SlotNo  m ()
    sendMsgNextTx NextTx toResponse =
        SendMsgNextTx $ \next -> do
            yield $ encodeNextTxResponse $ toResponse $ NextTxResponse{next}
            clientStAcquired

    sendMsgHasTx
        :: HasTx block
        -> (Wsp.ToResponse HasTxResponse)
        -> ClientStAcquired (GenTxId block) (GenTx block) SlotNo  m ()
    sendMsgHasTx HasTx{id} toResponse =
        SendMsgHasTx id $ \has -> do
            yield $ encodeHasTxResponse $ toResponse $ HasTxResponse{has}
            clientStAcquired

    sendMsgGetSizes
        :: GetSizes
        -> (Wsp.ToResponse GetSizesResponse)
        -> ClientStAcquired (GenTxId block) (GenTx block) SlotNo  m ()
    sendMsgGetSizes GetSizes toResponse =
        SendMsgGetSizes $ \sizes -> do
            yield $ encodeGetSizesResponse $ toResponse $ GetSizesResponse{sizes}
            clientStAcquired
