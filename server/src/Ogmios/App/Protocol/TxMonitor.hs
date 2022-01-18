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
-- `HasTx`, `NextTx` and `SizeAndCapacity` can be sent right away. This effectively
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
    , HasTx (..)
    , HasTxResponse (..)
    , NextTx (..)
    , NextTxResponse (..)
    , ReleaseMempool (..)
    , ReleaseMempoolResponse (..)
    , SizeAndCapacity (..)
    , SizeAndCapacityResponse (..)
    , SlotNo (..)
    , TxMonitorCodecs (..)
    , TxMonitorMessage (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..) )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( ClientStAcquired (..), ClientStIdle (..), LocalTxMonitorClient (..) )

import qualified Codec.Json.Wsp as Wsp

mkTxMonitorClient
    :: forall m block.
        ( MonadSTM m
        , HasTxId (GenTx block)
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
        MsgSizeAndCapacity SizeAndCapacity toResponse _ -> do
            pure $ SendMsgAcquire $ \_slot -> do
                pure $ sendMsgSizeAndCapacity SizeAndCapacity toResponse
        MsgReleaseMempool ReleaseMempool _ toFault -> do
            let fault = "'ReleaseMempool' must be call after acquiring some state."
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
        MsgSizeAndCapacity SizeAndCapacity toResponse _ ->
            sendMsgSizeAndCapacity SizeAndCapacity toResponse
        MsgReleaseMempool ReleaseMempool toResponse _ ->
            SendMsgRelease $ do
                yield $ encodeReleaseMempoolResponse $ toResponse Released
                clientStIdle

    sendMsgNextTx
        :: NextTx
        -> (Wsp.ToResponse (NextTxResponse block))
        -> ClientStAcquired (GenTxId block) (GenTx block) SlotNo  m ()
    sendMsgNextTx NextTx toResponse =
        SendMsgNextTx $ \(fmap txId -> next) -> do
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

    sendMsgSizeAndCapacity
        :: SizeAndCapacity
        -> (Wsp.ToResponse SizeAndCapacityResponse)
        -> ClientStAcquired (GenTxId block) (GenTx block) SlotNo  m ()
    sendMsgSizeAndCapacity SizeAndCapacity toResponse =
        SendMsgGetSizes $ \sizes -> do
            yield $ encodeSizeAndCapacityResponse $ toResponse $ SizeAndCapacityResponse{sizes}
            clientStAcquired
