--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

-- | Transaction submission is pretty simple & works by submitting an already
-- serialized and signed transaction as one single message.
--
-- In case of success, Ogmios / the node returns an empty response. Otherwise,
-- it returns an error with some details about what went wrong. Clients must
-- thereby know how to construct valid transactions.
--
-- @
--      ┌──────────┐
--      │   Busy   │◀══════════════════════════════╗
--      └────┬─────┘            SubmitTx           ║
--           │                                     ║
--           │                                ┌──────────┐
--           │                                │          │
--           │                                │          │
--           │          SubmitTxResponse      │   Idle   │
--           └───────────────────────────────▶│          │
--                                            │          │⇦ START
--                                            └──────────┘
-- @
module Ogmios.App.Protocol.TxSubmission
    ( mkTxSubmissionClient
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Protocol.TxSubmission
    ( SubmitTx (..), TxSubmissionCodecs (..), TxSubmissionMessage (..) )

import Cardano.Network.Protocol.NodeToClient
    ( SerializedTx, SubmitTxError )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )

mkTxSubmissionClient
    :: forall m block.
        ( MonadSTM m
        )
    => TxSubmissionCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (TxSubmissionMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalTxSubmissionClient (SerializedTx block) (SubmitTxError block) m ()
mkTxSubmissionClient TxSubmissionCodecs{..} queue yield =
    LocalTxSubmissionClient clientStIdle
  where
    await :: m (TxSubmissionMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (LocalTxClientStIdle (SerializedTx block) (SubmitTxError block) m ())
    clientStIdle = await >>= \case
        MsgSubmitTx SubmitTx{bytes} toResponse _ -> do
            pure $ SendMsgSubmitTx bytes $ \result -> do
                yield $ encodeSubmitTxResponse $ toResponse result
                clientStIdle
