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

import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Control.Monad.Trans.Except
    ( Except )
import Data.SOP.Strict
    ( NS (..) )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import GHC.Records
    ( HasField (..) )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, newEmptyTMVar, putTMVar, readTQueue, takeTMVar )
import Ogmios.Data.EraTranslation
    ( translateTx, translateUTxO )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Protocol.TxSubmission
    ( AlonzoEra
    , BabbageEra
    , BackwardCompatibleSubmitTx (..)
    , CanEvaluateScriptsInEra
    , EpochInfo
    , EvaluateTx (..)
    , EvaluateTxError (..)
    , EvaluateTxResponse (..)
    , HasTxId
    , PParams
    , PastHorizonException
    , SerializedTx
    , SomeUTxOInAnyEra (..)
    , SubmitTx (..)
    , SubmitTxError
    , SystemStart
    , Tx
    , TxIn
    , TxSubmissionCodecs (..)
    , TxSubmissionMessage (..)
    , UTxO (..)
    , eraDiscrepancy
    , evaluateExecutionUnits
    , incompatibleEra
    , mkSubmitTxResponse
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoBlock
    , CardanoEras
    , CardanoQuery
    , CardanoQueryResult
    , GenTx (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..), HardForkBlock )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryHardFork (..) )
import Ouroboros.Consensus.HardFork.History
    ( interpreterToEpochInfo )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..) )
import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )
import Type.Reflection
    ( typeRep )

import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.HardFork.Combinator as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as Ledger
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
            result <- evaluateExecutionUnitsM (additionalUtxoSet, tx)
            yield $ encodeEvaluateTxResponse $ toResponse result
            clientStIdle

-- | A thin abstraction for evaluating transaction units.
data ExecutionUnitsEvaluator m block = ExecutionUnitsEvaluator
    { evaluateExecutionUnitsM
        :: (SomeUTxOInAnyEra block, GenTx block)
        -> m (EvaluateTxResponse block)
    }

data SomeEvaluationInAnyEra where
    SomeEvaluationInAnyEra
        :: forall era. (CanEvaluateScriptsInEra era)
        => UTxO era
        -> Tx era
        -> SomeEvaluationInAnyEra

-- | Construct an effectful 'ExecutionUnitsEvaluator'; this requires to wire a
-- local-state-query client to the node.
newExecutionUnitsEvaluator
    :: forall m block crypto.
        ( MonadSTM m
        , block ~ HardForkBlock (CardanoEras crypto)
        , crypto ~ StandardCrypto
        , CanEvaluateScriptsInEra (AlonzoEra crypto)
        , CanEvaluateScriptsInEra (BabbageEra crypto)
        )
    => m ( ExecutionUnitsEvaluator m block
         , LocalStateQueryClient block (Point block) (Query block) m ()
         )
newExecutionUnitsEvaluator = do
    evaluateExecutionUnitsRequest  <- atomically newEmptyTMVar
    evaluateExecutionUnitsResponse <- atomically newEmptyTMVar
    return
        ( ExecutionUnitsEvaluator
            { evaluateExecutionUnitsM = \case
                (_, GenTxByron{}) ->
                    return (incompatibleEra "Byron")
                (_, GenTxShelley{}) ->
                    return (incompatibleEra "Shelley")
                (_, GenTxAllegra{}) ->
                    return (incompatibleEra "Allegra")
                (_, GenTxMary{}) ->
                    return (incompatibleEra "Mary")
                (SomeUTxOInAlonzoEra utxo, GenTxAlonzo (ShelleyTx _id tx)) -> do
                    atomically $ putTMVar evaluateExecutionUnitsRequest $ SomeEvaluationInAnyEra utxo tx
                    atomically $ takeTMVar evaluateExecutionUnitsResponse
                (SomeUTxOInBabbageEra utxo, GenTxBabbage (ShelleyTx _id tx)) -> do
                    atomically $ putTMVar evaluateExecutionUnitsRequest $ SomeEvaluationInAnyEra utxo tx
                    atomically $ takeTMVar evaluateExecutionUnitsResponse
                (_, _) ->
                    return eraDiscrepancy
            }
        , localStateQueryClient
            (atomically $ takeTMVar evaluateExecutionUnitsRequest)
            (atomically . putTMVar evaluateExecutionUnitsResponse)
        )
  where
    localStateQueryClient
        :: m SomeEvaluationInAnyEra
        -> (EvaluateTxResponse block -> m ())
        -> LocalStateQueryClient block (Point block) (Query block) m ()
    localStateQueryClient await reply =
        LocalStateQueryClient clientStIdle
      where
        clientStIdle
            :: m (LSQ.ClientStIdle block (Point block) (Query block) m ())
        clientStIdle = do
            await <&> LSQ.SendMsgAcquire Nothing . clientStAcquiring

        clientStAcquiring
            :: SomeEvaluationInAnyEra
            -> LSQ.ClientStAcquiring block (Point block) (Query block) m ()
        clientStAcquiring args =
            LSQ.ClientStAcquiring
                { LSQ.recvMsgAcquired = pure $ selectEra
                    -- Default / Fallback
                    (\era -> do
                        reply (incompatibleEra era)
                        pure $ LSQ.SendMsgRelease clientStIdle
                    )
                    -- Alonzo
                    (clientStAcquired0 args evaluateExecutionUnits)
                    -- Babbage
                    (clientStAcquired0 args evaluateExecutionUnits)
                , LSQ.recvMsgFailure =
                    const $ pure $ LSQ.SendMsgAcquire Nothing (clientStAcquiring args)
                }

        reAcquire
            :: SomeEvaluationInAnyEra
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        reAcquire
            = LSQ.SendMsgReAcquire Nothing . clientStAcquiring

        clientStAcquired0
            :: forall proto era. (CanEvaluateScriptsInEra era)
            => SomeEvaluationInAnyEra
            -> (  PParams era
               -> SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTxResponse block
               )
            -> (forall result a. (CardanoQueryResult crypto result ~ a) => BlockQuery (ShelleyBlock (proto crypto) era) result -> CardanoQuery crypto a)
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired0 args callback hoistQuery = do
            let query = BlockQuery (hoistQuery GetCurrentPParams)
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \case
                    Right pparams ->
                        pure $ clientStAcquired1 args (callback pparams) hoistQuery
                    Left{} ->
                        pure $ reAcquire args
                }

        clientStAcquired1
            :: forall proto era. (CanEvaluateScriptsInEra era)
            => SomeEvaluationInAnyEra
            -> (  SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTxResponse block
               )
            -> (forall result a. (CardanoQueryResult crypto result ~ a) => BlockQuery (ShelleyBlock (proto crypto) era) result -> CardanoQuery crypto a)
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired1 args callback hoistQuery = do
            let query = GetSystemStart
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \systemStart ->
                    pure $ clientStAcquired2 args (callback systemStart) hoistQuery
                }

        clientStAcquired2
            :: forall proto era. (CanEvaluateScriptsInEra era)
            => SomeEvaluationInAnyEra
            -> (  EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTxResponse block
               )
            -> (forall result a. (CardanoQueryResult crypto result ~ a) => BlockQuery (ShelleyBlock (proto crypto) era) result -> CardanoQuery crypto a)
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired2 args callback hoistQuery = do
            let query = BlockQuery $ QueryHardFork GetInterpreter
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \(interpreterToEpochInfo -> epochInfo) ->
                    pure $ clientStAcquired3 args (callback epochInfo) hoistQuery
                }

        clientStAcquired3
            :: forall proto era. (CanEvaluateScriptsInEra era)
            => SomeEvaluationInAnyEra
            -> (  UTxO era
               -> Tx era
               -> EvaluateTxResponse block
               )
            -> (forall result a. (CardanoQueryResult crypto result ~ a) => BlockQuery (ShelleyBlock (proto crypto) era) result -> CardanoQuery crypto a)
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired3 args callback hoistQuery = do
            let query = BlockQuery $ hoistQuery $ GetUTxOByTxIn (inputsInAnyEra args)
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \case
                    Right (UTxO networkUtxo) -> do
                        let result = case translateToNetworkEra @era args of
                                Nothing ->
                                    eraDiscrepancy
                                Just (UTxO userProvidedUtxo, tx) ->
                                    let intersection = Map.intersection userProvidedUtxo networkUtxo in
                                    if null intersection
                                    then
                                        callback (UTxO $ userProvidedUtxo <> networkUtxo) tx
                                    else
                                        let failure = EvaluateTxAdditionalUtxoOverlap $ Map.keysSet intersection
                                         in EvaluationFailure failure
                        reply result $> LSQ.SendMsgRelease clientStIdle
                    Left{} ->
                        pure $ reAcquire args
                }

inputsInAnyEra
    :: SomeEvaluationInAnyEra
    -> Set (TxIn StandardCrypto)
inputsInAnyEra (SomeEvaluationInAnyEra _ tx) =
    getField @"inputs" (getField @"body" tx)

-- | 99% of the time, we only need to worry about one era: the current one; yet, near hard-forks,
-- there is a time where the application needs to be compatible with both the current era and the
-- upcoming one (or, depending on the perspective, the current era and the previous one).
--
-- A 'simple' way to handle this would be to enforce that the network era matches the era of the
-- arguments provided by the users. However this can be extremely inconvenient for users who now
-- need to change their application logic to also be hard-fork resilient. Since hard-fork generally
-- changes in a forward-compatible way, then we can instead upgrade (or translate) clients' requests
-- to the latest era when applicable.
--
-- This allows client applications to upgrade whenever they are ready, and keep using structures from
-- the previous era, even after the hard-fork has happened. We take the burden of detecting the era and
-- translating the request.
translateToNetworkEra
    :: forall eraNetwork block crypto.
        ( CanEvaluateScriptsInEra eraNetwork
        )
    => SomeEvaluationInAnyEra
    -> Maybe (UTxO eraNetwork, Tx eraNetwork)
translateToNetworkEra (SomeEvaluationInAnyEra utxoOrig txOrig) =
    translate utxoOrig txOrig
  where
    translate
        :: forall eraArgs.
            ( CanEvaluateScriptsInEra eraArgs
            )
        => UTxO eraArgs
        -> Tx eraArgs
        -> Maybe (UTxO eraNetwork, Tx eraNetwork)
    translate utxo tx =
        let
            eraNetwork = typeRep @eraNetwork
            eraAlonzo  = typeRep @(AlonzoEra StandardCrypto)
            eraArgs    = typeRep @eraArgs
            eraBabbage = typeRep @(BabbageEra StandardCrypto)

            sameEra =
                case (testEquality eraNetwork eraArgs) of
                    Just Refl ->
                        Just (utxo, tx)
                    _ ->
                        Nothing

            alonzoToBabbage =
                case (testEquality eraNetwork eraBabbage, testEquality eraArgs eraAlonzo) of
                    (Just Refl, Just Refl) -> do
                        Just (translateUTxO utxo, translateTx tx)
                    _ ->
                        Nothing
          in
            sameEra <|> alonzoToBabbage

-- | Run local-state queries in either Alonzo or Babbage, depending on where the network is at.
--
-- This is crucial to allow the server to *dynamically* switch to the right era after the hard-fork.
selectEra
    :: forall block c m.
        ( block ~ HardForkBlock (CardanoEras c)
        , Applicative m
        )

    -- Default selector, when the network era is older than Alonzo and doesn't support Phase-2 scripts.
    => ( Text -> m (LSQ.ClientStAcquired block (Point block) (Query block) m ())
       )

    -- Selector for the Alonzo era.
    -> ( (forall r a. (CardanoQueryResult c r ~ a) => BlockQuery (ShelleyBlock (TPraos c) (AlonzoEra c)) r -> BlockQuery (CardanoBlock c) a)
         -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
       )

    -- Selector for the Babbage era.
    -> ( (forall r a. (CardanoQueryResult c r ~ a) => BlockQuery (ShelleyBlock (Praos c) (BabbageEra c)) r -> BlockQuery (CardanoBlock c) a)
         -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
       )

    -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
selectEra fallback asAlonzo asBabbage =
    LSQ.SendMsgQuery (Ledger.BlockQuery $ LSQ.QueryHardFork LSQ.GetCurrentEra) $
    LSQ.ClientStQuerying
        { LSQ.recvMsgResult = \case
            EraIndex                Z{}      -> fallback "Byron"
            EraIndex             (S Z{})     -> fallback "Shelley"
            EraIndex          (S (S Z{}))    -> fallback "Allegra"
            EraIndex       (S (S (S Z{})))   -> fallback "Mary"
            EraIndex    (S (S (S (S Z{}))))  -> pure (asAlonzo QueryIfCurrentAlonzo)
            EraIndex (S (S (S (S (S Z{}))))) -> pure (asBabbage QueryIfCurrentBabbage)
        }
