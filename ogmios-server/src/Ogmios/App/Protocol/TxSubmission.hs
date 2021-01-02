--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

-- | Transaction submission is pretty simple & works by submitting an already
-- serialized and signed transaction as one single message.
--
-- In case of success, Ogmios / the node returns an empty response. Otherwise,
-- it returns an error with some details about what went wrong. Clients must
-- thereby know how to construct valid transactions.
--
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
--
module Ogmios.App.Protocol.TxSubmission
    ( mkTxSubmissionClient
    ) where

import Relude hiding
    ( atomically )

import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue )
import Ogmios.Data.Protocol
    ( onUnmatchedMessage )
import Ogmios.Data.Protocol.TxSubmission
    ( SubmitTx (..), SubmitTxResponse (..), parserVoid )

import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json

--
-- Client
--

mkTxSubmissionClient
    :: forall m tx err.
        ( FromJSON tx
        , ToJSON err
        , MonadSTM m
        )
    => TQueue m ByteString
    -> (Json.Encoding -> m ())
    -> LocalTxSubmissionClient tx err m ()
mkTxSubmissionClient pipe yield =
     LocalTxSubmissionClient clientStIdle
  where
    await :: m ByteString
    await = atomically (readTQueue pipe)

    clientStIdle
        :: m (LocalTxClientStIdle tx err m ())
    clientStIdle = await >>= Wsp.handle
        (\bytes -> do
            yield $ onUnmatchedMessage (parserVoid @tx) bytes
            clientStIdle
        )
        [ Wsp.Handler $ \SubmitTx{bytes} toResponse ->
            pure $ SendMsgSubmitTx bytes $ \e -> do
                yield $ Json.toEncoding $ toResponse $ SubmitTxResponse e
                clientStIdle
        ]
