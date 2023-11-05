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

import Ogmios.App.Protocol
    ( defaultWithInternalError
    )
import Ogmios.Control.Exception
    ( MonadCatch
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Data.Json
    ( Json
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AcquireMempool (..)
    , AcquireMempoolResponse (..)
    , GenTx
    , GenTxId
    , HasTransaction (..)
    , HasTransactionResponse (..)
    , NextTransaction (..)
    , NextTransactionFields (..)
    , NextTransactionResponse (..)
    , ReleaseMempool (..)
    , ReleaseMempoolResponse (..)
    , SizeOfMempool (..)
    , SizeOfMempoolResponse (..)
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

mkTxMonitorClient
    :: forall m block.
        ( MonadSTM m
        , MonadCatch m
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
        MsgAcquireMempool AcquireMempool toResponse ->
            defaultWithInternalError clientStIdle yield toResponse $ do
                pure $ SendMsgAcquire $ \slot -> do
                    yield $ encodeAcquireMempoolResponse $ toResponse $ AcquireMempoolResponse slot
                    clientStAcquired
        MsgNextTransaction NextTransaction{} toResponse -> do
            yield $ encodeNextTransactionResponse $ toResponse NextTransactionMustAcquireFirst
            clientStIdle
        MsgHasTransaction HasTransaction{} toResponse -> do
            yield $ encodeHasTransactionResponse $ toResponse HasTransactionMustAcquireFirst
            clientStIdle
        MsgSizeOfMempool SizeOfMempool toResponse -> do
            yield $ encodeSizeOfMempoolResponse $ toResponse SizeOfMempoolMustAcquireFirst
            clientStIdle
        MsgReleaseMempool ReleaseMempool toResponse -> do
            yield $ encodeReleaseMempoolResponse $ toResponse ReleaseMempoolMustAcquireFirst
            clientStIdle

    clientStAcquired
        :: m (ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
    clientStAcquired = await >>= \case
        MsgAcquireMempool AcquireMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgAwaitAcquire $ \slot -> do
                    yield $ encodeAcquireMempoolResponse $ toResponse $ AcquireMempoolResponse slot
                    clientStAcquired
        MsgNextTransaction NextTransaction{fields} toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgNextTx $ \mTx -> do
                    let response = case fields of
                            Nothing ->
                                NextTransactionResponseId (txId <$> mTx)
                            Just NextTransactionAllFields ->
                                NextTransactionResponseTx mTx
                    yield $ encodeNextTransactionResponse $ toResponse response
                    clientStAcquired
        MsgHasTransaction HasTransaction{id} toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgHasTx id $ \has -> do
                    yield $ encodeHasTransactionResponse $ toResponse $ HasTransactionResponse{has}
                    clientStAcquired
        MsgSizeOfMempool SizeOfMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgGetSizes $ \mempool -> do
                    yield $ encodeSizeOfMempoolResponse $ toResponse $ SizeOfMempoolResponse{mempool}
                    clientStAcquired
        MsgReleaseMempool ReleaseMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgRelease $ do
                    yield $ encodeReleaseMempoolResponse $ toResponse Released
                    clientStIdle
