--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
    , newExecutionUnitsEvaluator
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, newEmptyTMVar, putTMVar, readTQueue, takeTMVar )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Protocol
    ( MostRecentEra )
import Ogmios.Data.Protocol.TxSubmission
    ( AlonzoEra
    , BackwardCompatibleSubmitTx (..)
    , EpochInfo
    , EvaluateTx (..)
    , EvaluateTxError (..)
    , EvaluateTxResponse (..)
    , HasTxId
    , PParams
    , PastHorizonException
    , SerializedTx
    , SubmitTx (..)
    , SubmitTxError
    , SystemStart
    , Tx
    , TxSubmissionCodecs (..)
    , TxSubmissionMessage (..)
    , UTxO (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , mkSubmitTxResponse
    )

import Cardano.Ledger.Alonzo.Tx
    ( body )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Control.Monad.Trans.Except
    ( Except )
import GHC.Records
    ( HasField (..) )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..), CardanoEras, GenTx (..) )
import Ouroboros.Consensus.HardFork.Combinator
    ( HardForkBlock )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryHardFork (..) )
import Ouroboros.Consensus.HardFork.History
    ( interpreterToEpochInfo )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx (..) )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..) )
import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )

import qualified Data.Map.Strict as Map
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

mkTxSubmissionClient
    :: forall m block.
        ( MonadSTM m
        , HasTxId (SerializedTx block)
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
        MsgBackwardCompatibleSubmitTx BackwardCompatibleSubmitTx{bytes = tx} toResponse _ -> do
            pure $ SendMsgSubmitTx tx $ \result -> do
                yield $ encodeBackwardCompatibleSubmitTxResponse $ toResponse result
                clientStIdle
        MsgSubmitTx SubmitTx{submit = tx} toResponse _ -> do
            pure $ SendMsgSubmitTx tx $ \result -> do
                yield $ encodeSubmitTxResponse $ toResponse $ mkSubmitTxResponse tx result
                clientStIdle
        MsgEvaluateTx EvaluateTx{additionalUtxoSet, evaluate = tx} toResponse _ -> do
            result <- evaluateExecutionUnitsM additionalUtxoSet tx
            yield $ encodeEvaluateTxResponse $ toResponse result
            clientStIdle

-- | A thin abstraction for evaluating transaction units.
data ExecutionUnitsEvaluator m block = ExecutionUnitsEvaluator
    { evaluateExecutionUnitsM
        :: UTxO (MostRecentEra block)
        -> SerializedTx block
        -> m (EvaluateTxResponse block)
    }

-- | Construct an effectful 'ExecutionUnitsEvaluator'; this requires to wire a
-- local-state-query client to the node.
newExecutionUnitsEvaluator
    :: forall m block crypto.
        ( MonadSTM m
        , crypto ~ StandardCrypto
        , block ~ HardForkBlock (CardanoEras crypto)
        )
    => m ( ExecutionUnitsEvaluator m block
         , LocalStateQueryClient block (Point block) (Query block) m ()
         )
newExecutionUnitsEvaluator = do
    evaluateExecutionUnitsRequest  <- atomically newEmptyTMVar
    evaluateExecutionUnitsResponse <- atomically newEmptyTMVar
    return
        ( ExecutionUnitsEvaluator
            { evaluateExecutionUnitsM = \utxo -> \case
                GenTxByron{} ->
                    return (incompatibleEra "Byron")
                GenTxShelley{} ->
                    return (incompatibleEra "Shelley")
                GenTxAllegra{} ->
                    return (incompatibleEra "Allegra")
                GenTxMary{} ->
                    return (incompatibleEra "Mary")
                GenTxAlonzo (ShelleyTx _id tx) -> do
                    atomically $ putTMVar evaluateExecutionUnitsRequest (utxo, tx)
                    atomically $ takeTMVar evaluateExecutionUnitsResponse
            }
        , localStateQueryClient
            (atomically $ takeTMVar evaluateExecutionUnitsRequest)
            (atomically . putTMVar evaluateExecutionUnitsResponse)
        )
  where
    localStateQueryClient
        :: m (UTxO (AlonzoEra crypto), Tx (AlonzoEra crypto))
        -> (EvaluateTxResponse block -> m ())
        -> LocalStateQueryClient block (Point block) (Query block) m ()
    localStateQueryClient await reply =
        LocalStateQueryClient clientStIdle
      where
        clientStIdle
            :: m (LSQ.ClientStIdle block (Point block) (Query block) m ())
        clientStIdle = do
            await <&> LSQ.SendMsgAcquire Nothing . uncurry clientStAcquiring

        clientStAcquiring
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> LSQ.ClientStAcquiring block (Point block) (Query block) m ()
        clientStAcquiring utxo tx =
            LSQ.ClientStAcquiring
                { LSQ.recvMsgAcquired =
                    pure (clientStAcquired0 utxo tx evaluateExecutionUnits)
                , LSQ.recvMsgFailure =
                    const $ pure $ LSQ.SendMsgAcquire Nothing (clientStAcquiring utxo tx)
                }

        reAcquire
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        reAcquire utxo
            = LSQ.SendMsgReAcquire Nothing . clientStAcquiring utxo

        clientStAcquired0
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> (  PParams (AlonzoEra crypto)
               -> SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO (AlonzoEra crypto)
               -> Tx (AlonzoEra crypto)
               -> EvaluateTxResponse block
               )
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired0 utxo tx callback = do
            let query = BlockQuery (QueryIfCurrentAlonzo GetCurrentPParams)
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \case
                    Right pparams ->
                        pure $ clientStAcquired1 utxo tx (callback pparams)
                    Left{} ->
                        pure $ reAcquire utxo tx
                }

        clientStAcquired1
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> (  SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO (AlonzoEra crypto)
               -> Tx (AlonzoEra crypto)
               -> EvaluateTxResponse block
               )
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired1 utxo tx callback = do
            let query = GetSystemStart
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult =
                    pure . clientStAcquired2 utxo tx . callback
                }

        clientStAcquired2
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> (  EpochInfo (Except PastHorizonException)
               -> UTxO (AlonzoEra crypto)
               -> Tx (AlonzoEra crypto)
               -> EvaluateTxResponse block
               )
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired2 utxo tx callback = do
            let query = BlockQuery $ QueryHardFork GetInterpreter
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \(interpreterToEpochInfo -> epochInfo) ->
                    pure $ clientStAcquired3 utxo tx (callback epochInfo)
                }

        clientStAcquired3
            :: UTxO (AlonzoEra crypto)
            -> Tx (AlonzoEra crypto)
            -> (  UTxO (AlonzoEra crypto)
               -> Tx (AlonzoEra crypto)
               -> EvaluateTxResponse block
               )
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired3 (UTxO userProvidedUtxo) tx callback = do
            let inputs = getField @"inputs" (body tx)
                query  = BlockQuery $ QueryIfCurrentAlonzo $ GetUTxOByTxIn inputs
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \case
                    Right (UTxO networkUtxo) -> do
                        let intersection = Map.intersection userProvidedUtxo networkUtxo
                        if null intersection then do
                            reply (callback (UTxO $ userProvidedUtxo <> networkUtxo) tx)
                        else do
                            let failure = EvaluateTxAdditionalUtxoOverlap $ Map.keysSet intersection
                            reply (EvaluationFailure failure)
                        pure $ LSQ.SendMsgRelease clientStIdle
                    Left{} ->
                        pure $ reAcquire (UTxO userProvidedUtxo) tx
                }
