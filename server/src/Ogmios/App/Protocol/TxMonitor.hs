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
module Ogmios.App.Protocol.TxMonitor
    ( mkTxMonitorClient
    ) where

import Ogmios.Prelude hiding
    ( id
    )

import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Data.Json
    ( Json
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AwaitAcquire (..)
    , AwaitAcquireResponse (..)
    , GenTx
    , GenTxId
    , HasTx (..)
    , HasTxResponse (..)
    , NextTx (..)
    , NextTxFields (..)
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
    ( HasTxId (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( ClientStAcquired (..)
    , ClientStIdle (..)
    , LocalTxMonitorClient (..)
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Prelude

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

    mustAcquireFirst :: forall a. (Show a) => a -> Wsp.ToFault -> m ()
    mustAcquireFirst query toFault = do
        let constructor = toString $ Prelude.head $ words $ show query
        let fault = "'" <> constructor <> "' must be called after at least one 'AwaitAcquire'."
        yield $ Wsp.mkFault $ toFault Wsp.FaultClient fault

    clientStIdle
        :: m (ClientStIdle (GenTxId block) (GenTx block) SlotNo m ())
    clientStIdle = await >>= \case
        MsgAwaitAcquire AwaitAcquire toResponse _ ->
            pure $ SendMsgAcquire $ \slot -> do
                yield $ encodeAwaitAcquireResponse $ toResponse $ AwaitAcquired slot
                clientStAcquired
        MsgNextTx q@NextTx{} _ toFault -> do
            mustAcquireFirst q toFault
            clientStIdle
        MsgHasTx q@HasTx{} _ toFault -> do
            mustAcquireFirst q toFault
            clientStIdle
        MsgSizeAndCapacity q@SizeAndCapacity _ toFault -> do
            mustAcquireFirst q toFault
            clientStIdle
        MsgReleaseMempool q@ReleaseMempool _ toFault -> do
            mustAcquireFirst q toFault
            clientStIdle

    clientStAcquired
        :: m (ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
    clientStAcquired = await <&> \case
        MsgAwaitAcquire AwaitAcquire toResponse _ ->
            SendMsgAwaitAcquire $ \slot -> do
                yield $ encodeAwaitAcquireResponse $ toResponse $ AwaitAcquired slot
                clientStAcquired
        MsgNextTx NextTx{fields} toResponse _ ->
            SendMsgNextTx $ \mTx -> do
                let response = case fields of
                        Nothing ->
                            NextTxResponseId (txId <$> mTx)
                        Just NextTxAllFields ->
                            NextTxResponseTx mTx
                yield $ encodeNextTxResponse $ toResponse response
                clientStAcquired
        MsgHasTx HasTx{id} toResponse _ ->
            SendMsgHasTx id $ \has -> do
                yield $ encodeHasTxResponse $ toResponse $ HasTxResponse{has}
                clientStAcquired
        MsgSizeAndCapacity SizeAndCapacity toResponse _ ->
            SendMsgGetSizes $ \sizes -> do
                yield $ encodeSizeAndCapacityResponse $ toResponse $ SizeAndCapacityResponse{sizes}
                clientStAcquired
        MsgReleaseMempool ReleaseMempool toResponse _ ->
            SendMsgRelease $ do
                yield $ encodeReleaseMempoolResponse $ toResponse Released
                clientStIdle
