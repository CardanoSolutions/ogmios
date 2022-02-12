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
-- Ogmios offers a slightly modified version of that protocol and allows to
-- only evaluate a transaction redeemers' execution units.
-- @
--   ┌──────────┐
--   │   Busy   │◀═══════════════════════════════════════╗
--   └────┬─────┘        SubmitTx / EvaluateTx           ║
--        │                                              ║
--        │                                         ┌──────────┐
--        │                                         │          │
--        │                                         │          │
--        │  SubmitTxResponse / EvaluateTxResponse  │   Idle   │
--        └────────────────────────────────────────▶│          │
--                                                  │          │⇦ START
--                                                  └──────────┘
-- @
module Ogmios.App.Protocol.TxSubmission
    ( ExecutionUnitsEvaluator(..)
    , mkTxSubmissionClient
    , mkExecutionUnitsEvaluator
    ) where

import Ogmios.Prelude

import Ogmios.Control.Exception
    ( MonadThrow )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TQueue
    , TVar
    , newEmptyTMVar
    , putTMVar
    , readTQueue
    , takeTMVar
    )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Protocol.TxSubmission
    ( AlonzoEra
    , EpochInfo
    , EvaluateTx (..)
    , EvaluateTxError (..)
    , EvaluateTxResponse (..)
    , PParams
    , SubmitTx (..)
    , SystemStart
    , Tx
    , TxSubmissionCodecs (..)
    , TxSubmissionMessage (..)
    , UTxO
    , evaluateExecutionUnits
    , incompatibleEra
    )

import Cardano.Network.Protocol.NodeToClient
    ( Crypto, SerializedTx, SubmitTxError )
import Cardano.Slotting.EpochInfo.API
    ( EpochInfo )
import Cardano.Slotting.Time
    ( SystemStart )
import Data.Array
    ( Array )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras, GenTx (..) )
import Ouroboros.Consensus.HardFork.Combinator
    ( HardForkBlock, eraIndexToInt )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, getTipPoint )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )

import qualified Ouroboros.Consensus.HardFork.Combinator as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

-- | A thin abstraction for evaluating transaction units.
data ExecutionUnitsEvaluator m block = ExecutionUnitsEvaluator
    { evaluateExecutionUnitsM
        :: SerializedTx block
        -> m (EvaluateTxResponse block)
    }

mkTxSubmissionClient
    :: forall m block.
        ( MonadSTM m
        )
    => TxSubmissionCodecs block
        -- ^ For encoding Haskell types to JSON
    -> ExecutionUnitsEvaluator m block
        -- ^ An interface for evaluating transaction execution units
    -> TQueue m (TxSubmissionMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalTxSubmissionClient (SerializedTx block) (SubmitTxError block) m ()
mkTxSubmissionClient TxSubmissionCodecs{..} ExecutionUnitsEvaluator{..} queue yield = do
    LocalTxSubmissionClient clientStIdle
  where
    await :: m (TxSubmissionMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (LocalTxClientStIdle (SerializedTx block) (SubmitTxError block) m ())
    clientStIdle = await >>= \case
        MsgSubmitTx SubmitTx{submit = tx} toResponse _ -> do
            pure $ SendMsgSubmitTx tx $ \result -> do
                yield $ encodeSubmitTxResponse $ toResponse result
                clientStIdle
        MsgEvaluateTx EvaluateTx{evaluate = tx} toResponse _ -> do
            result <- evaluateExecutionUnitsM tx
            yield $ encodeEvaluateTxResponse $ toResponse result
            clientStIdle

mkExecutionUnitsEvaluator
    :: forall m block crypto tx.
        ( MonadSTM m
        , MonadThrow m
        , crypto ~ Crypto block
        , block ~ HardForkBlock (CardanoEras crypto)
        )
    => m ( ExecutionUnitsEvaluator m block
         , LocalStateQueryClient block (Point block) (Ledger.Query block) m ()
         )
mkExecutionUnitsEvaluator = do
    evaluateExecutionUnitsRequest  <- atomically newEmptyTMVar
    evaluateExecutionUnitsResponse <- atomically newEmptyTMVar
    return
        ( ExecutionUnitsEvaluator
            { evaluateExecutionUnitsM = \case
                GenTxByron{} ->
                    return (incompatibleEra "Byron")
                GenTxShelley{} ->
                    return (incompatibleEra "Shelley")
                GenTxAllegra{} ->
                    return (incompatibleEra "Allegra")
                GenTxMary{} ->
                    return (incompatibleEra "Mary")
                GenTxAlonzo (ShelleyTx _id tx) -> do
                    atomically $ putTMVar evaluateExecutionUnitsRequest tx
                    atomically $ takeTMVar evaluateExecutionUnitsResponse
            }
        , LocalStateQueryClient $ clientStIdle
            (atomically $ takeTMVar evaluateExecutionUnitsRequest)
            (atomically . putTMVar evaluateExecutionUnitsResponse)
        )
  where
    clientStIdle
        :: m (Tx (AlonzoEra crypto))
        -> (EvaluateTxResponse block -> m ())
        -> m (LSQ.ClientStIdle block (Point block) (Ledger.Query block) m ())
    clientStIdle =
        undefined -- FIXME: implement.

    -- . evaluateExecutionUnits
    --    (  PParams (AlonzoEra crypto)
    --    -> SystemStart
    --    -> EpochInfo m
    --    -> UTxO (AlonzoEra crypto)
    --    -> Tx (AlonzoEra crypto)
    --    -> m (EvaluateTxResponse block)
    --    )
