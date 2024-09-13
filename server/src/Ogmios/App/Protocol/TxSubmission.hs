--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

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
    , TraceTxSubmission (..)

    -- Internal
    , SomeEvaluationInAnyEra (..)
    , newEvaluateTransactionResponse
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.TxBody
    ( AlonzoEraTxBody (..)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageEraTxBody (..)
    )
import Cardano.Ledger.BaseTypes
    ( SlotNo (..)
    )
import Cardano.Ledger.Core
    ( EraTx (..)
    , EraTxBody (..)
    , eraName
    )
import Control.Monad.Trans.Except
    ( Except
    )
import Data.Aeson
    ( genericToEncoding
    , toEncoding
    )
import Data.SOP.Strict
    ( NS (..)
    )
import Data.Type.Equality
    ( testEquality
    , (:~:) (..)
    )
import Ogmios.Control.MonadClock
    ( MonadClock (..)
    )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TQueue
    , putTMVar
    , readTQueue
    , takeTMVar
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    , MultiEraUTxO (..)
    , Upgrade (..)
    , upgradeGenTx
    )
import Ogmios.Data.Json
    ( Json
    , MultiEraDecoder (..)
    , ToJSON
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( EvaluateTransactionError (..)
    )
import Ogmios.Data.Protocol.TxSubmission
    ( CanEvaluateScriptsInEra
    , EpochInfo
    , EvaluateTransaction (..)
    , EvaluateTransactionResponse (..)
    , GenTxId
    , HasTxId
    , PParams
    , PastHorizonException
    , SerializedTransaction
    , SubmitTransaction (..)
    , SubmitTransactionError
    , SubmitTransactionResponse (..)
    , SystemStart
    , TxIn (..)
    , TxSubmissionCodecs (..)
    , TxSubmissionMessage (..)
    , UTxO (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , mergeUtxo
    , mkSubmitTransactionResponse
    , nodeTipTooOld
    , unsupportedEra
    , utxoFromMempool
    , utxoReferences
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoBlock
    , CardanoQueryResult
    , GenTx (..)
    , HardForkApplyTxErr (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( HardForkBlock
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryHardFork (..)
    )
import Ouroboros.Consensus.HardFork.History
    ( interpreterToEpochInfo
    )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ConvertRawTxId (..)
    , HasTxId (..)
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( GenTx (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock
    )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..)
    )
import Ouroboros.Network.Block
    ( Point (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( Target (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( LocalTxMonitorClient (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..)
    , LocalTxSubmissionClient (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..)
    )
import Type.Reflection
    ( typeRep
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Ouroboros.Consensus.HardFork.Combinator as HF
import qualified Ouroboros.Consensus.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Client as LMM

mkTxSubmissionClient
    :: forall m block crypto.
        ( MonadSTM m
        , MonadLog m
        , HasTxId (SerializedTransaction block)
        , Crypto crypto
        , block ~ CardanoBlock crypto
        )
    => Logger TraceTxSubmission
        -- ^ A logger for the submission client
    -> (forall a r. m a -> (Json -> m ()) -> Rpc.ToResponse r -> m a -> m a)
        -- ^ A default response handler to catch errors.
    -> TxSubmissionCodecs block
        -- ^ For encoding Haskell types to JSON
    -> ExecutionUnitsEvaluator m block
        -- ^ An interface for evaluating transaction execution units
    -> TQueue m (TxSubmissionMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalTxSubmissionClient (SerializedTransaction block) (SubmitTransactionError block) m ()
mkTxSubmissionClient tr defaultWithInternalError TxSubmissionCodecs{..} ExecutionUnitsEvaluator{..} queue yield = do
    LocalTxSubmissionClient clientStIdle
  where
    await :: m (TxSubmissionMessage block)
    await = atomically (readTQueue queue)

    isMostRecentEra era =
        T.toLower (toText era) == T.toLower (toText (eraName @(MostRecentEra block)))

    -- NOTE: On successful submission, clear our cached
    -- mempool to ensure we always use the latest available
    -- mempool snapshot during evaluation.
    clearMempoolOnSuccess = \case
        SubmitSuccess -> clearMempoolM
        _ -> pure ()

    clientStIdle
        :: m (LocalTxClientStIdle (SerializedTransaction block) (SubmitTransactionError block) m ())
    clientStIdle = await >>= \case
        MsgSubmitTransaction SubmitTransaction{transaction = request} toResponse -> do
            defaultWithInternalError clientStIdle yield toResponse $ case request of
                MultiEraDecoderSuccess transaction -> do
                    pure $ SendMsgSubmitTx transaction $ \result -> do
                        case result of
                            SubmitFail (ApplyTxErrWrongEra eraMismatch) | isMostRecentEra (ledgerEraName eraMismatch) -> do
                                case upgradeGenTx transaction of
                                    Left hint -> do
                                        SubmitTransactionFailedToUpgrade hint
                                            & toResponse
                                            & encodeSubmitTransactionResponse
                                            & yield
                                        clientStIdle
                                    Right upgradedTx ->
                                        pure $ SendMsgSubmitTx upgradedTx $ \result' -> do
                                            clearMempoolOnSuccess result'
                                            mkSubmitTransactionResponse transaction result'
                                                & toResponse
                                                & encodeSubmitTransactionResponse
                                                & yield
                                            clientStIdle
                            _ -> do
                                -- NOTE: On successful submission, clear our cached
                                -- mempool to ensure we always use the latest available
                                -- mempool snapshot during evaluation.
                                clearMempoolOnSuccess result
                                mkSubmitTransactionResponse transaction result
                                    & toResponse
                                    & encodeSubmitTransactionResponse
                                    & yield
                                clientStIdle

                MultiEraDecoderErrors errs -> do
                    SubmitTransactionDeserialisationFailure errs
                        & toResponse
                        & encodeSubmitTransactionResponse
                        & yield
                    clientStIdle

        MsgEvaluateTransaction EvaluateTransaction{additionalUtxo, transaction = request} toResponse -> do
            defaultWithInternalError clientStIdle yield toResponse $ case request of
                MultiEraDecoderSuccess transaction -> do
                    mempoolUtxo <- utxoFromMempool <$> readMempoolM
                    logWith tr $ TxSubmissionEvaluateUtxoProvidedByUser { utxoRefs = utxoReferences additionalUtxo }
                    logWith tr $ TxSubmissionEvaluateUtxoInferredFromMempool { utxoRefs = utxoReferences mempoolUtxo }
                    result <- evaluateExecutionUnitsM (mergeUtxo mempoolUtxo additionalUtxo, transaction)
                    result
                        & toResponse
                        & encodeEvaluateTransactionResponse
                        & yield
                    clientStIdle
                MultiEraDecoderErrors errs -> do
                    EvaluateTransactionDeserialisationFailure errs
                        & toResponse
                        & encodeEvaluateTransactionResponse
                        & yield
                    clientStIdle

-- | A thin abstraction for evaluating transaction units.
data ExecutionUnitsEvaluator m block = ExecutionUnitsEvaluator
    { evaluateExecutionUnitsM
        :: (MultiEraUTxO block, GenTx block)
        -> m (EvaluateTransactionResponse block)

    , readMempoolM
        :: m [GenTx block]

    , clearMempoolM
        :: m ()
    }

-- | Construct an effectful 'ExecutionUnitsEvaluator'; this requires to wire a
-- local-state-query client to the node.
newExecutionUnitsEvaluator
    :: forall m block crypto.
        ( MonadSTM m
        , MonadLog m
        , MonadClock m
        , block ~ HardForkBlock (CardanoEras crypto)
        , crypto ~ StandardCrypto
        , CanEvaluateScriptsInEra (BabbageEra crypto)
        , CanEvaluateScriptsInEra (ConwayEra crypto)
        , ConvertRawTxId (GenTx (CardanoBlock crypto))
        )
    => Logger TraceTxSubmission
    -> m ( ExecutionUnitsEvaluator m block
         , LocalStateQueryClient block (Point block) (Query block) m ()
         , LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
         )
newExecutionUnitsEvaluator tr = do
    evaluateExecutionUnitsRequest  <- newEmptyTMVarIO
    evaluateExecutionUnitsResponse <- newEmptyTMVarIO

    mempool <- newTMVarIO []
    mempoolSnapshot <- newTVarIO Nothing
    mempoolSnapshotOnLastSubmit <- newTVarIO Nothing

    let runEvaluation = either return $ \(!eval) -> do
            atomically $ putTMVar evaluateExecutionUnitsRequest eval
            atomically $ takeTMVar evaluateExecutionUnitsResponse

    return
        ( ExecutionUnitsEvaluator
            { evaluateExecutionUnitsM = runEvaluation <=< \case
                (_, GenTxByron{}) -> do
                    return $ Left (incompatibleEra "byron")

                (_, GenTxShelley{}) ->
                    return $ Left (incompatibleEra "shelley")

                (_, GenTxAllegra{}) ->
                    return $ Left (incompatibleEra "allegra")

                (_, GenTxMary{}) ->
                    return $ Left (incompatibleEra "mary")

                (_, GenTxAlonzo{}) ->
                    return $ Left (unsupportedEra "alonzo")

                (UTxOInBabbageEra utxo, GenTxBabbage (ShelleyTx _id tx)) -> do
                    logWith tr $ TxSubmissionEvaluateArguments { utxoEra = "babbage", transactionEra = "babbage" }
                    return $ Right (SomeEvaluationInAnyEra utxo tx)

                (UTxOInBabbageEra utxo, GenTxConway (ShelleyTx _id tx)) -> do
                    logWith tr $ TxSubmissionEvaluateArguments { utxoEra = "babbage", transactionEra = "conway" }
                    return $ Right (SomeEvaluationInAnyEra (upgrade utxo) tx)

                (UTxOInConwayEra utxo, GenTxBabbage (ShelleyTx _id tx)) -> do
                    logWith tr $ TxSubmissionEvaluateArguments { utxoEra = "conway", transactionEra = "babbage" }
                    return $ Right (SomeEvaluationInAnyEra utxo (upgrade tx))

                (UTxOInConwayEra utxo, GenTxConway (ShelleyTx _id tx)) -> do
                    logWith tr $ TxSubmissionEvaluateArguments { utxoEra = "conway", transactionEra = "conway" }
                    return $ Right (SomeEvaluationInAnyEra utxo tx)

            , readMempoolM = do
                logWith tr TxSubmissionLocalMempoolTryRead
                txs <- atomically $ do
                    snapshot <- readTVar mempoolSnapshot
                    readTVar mempoolSnapshotOnLastSubmit >>= \case
                        Just snapshot' | snapshot == Just snapshot' -> retry
                        _ -> readTMVar mempool
                txs <$ logWith tr TxSubmissionLocalMempoolCurrent
                    { transactions = encodeBase16 . fromShort . toRawTxIdHash . txId <$> txs
                    }

            , clearMempoolM = do
                snapshot <- atomically $ do
                    snapshot <- readTVar mempoolSnapshot
                    snapshot <$ writeTVar mempoolSnapshotOnLastSubmit snapshot
                whenJust snapshot $ \(unSlotNo -> atSlot, ns) ->
                    logWith tr TxSubmissionLocalMempoolCleared { atSlot, ns }
            }

        , localStateQueryClient
            (atomically $ takeTMVar evaluateExecutionUnitsRequest)
            (atomically . putTMVar evaluateExecutionUnitsResponse)

        , mempoolMonitoringClient mempool mempoolSnapshot
        )
  where
    mempoolMonitoringClient
        :: TMVar m [GenTx block]
        -> TVar m (Maybe (SlotNo, Word64))
        -> LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
    mempoolMonitoringClient mempool mempoolSnapshot =
        LocalTxMonitorClient $ pure $ LMM.SendMsgAcquire clientStAwaitAcquire
      where
        clientStAwaitAcquire :: SlotNo -> m (LMM.ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
        clientStAwaitAcquire slot = do
            now <- getMonotonicTimeNSec
            logWith tr $ TxSubmissionLocalMempoolNewSnapshot { atSlot = unSlotNo slot, ns = now }
            atomically $ void $ takeTMVar mempool
            clientStDrain slot now []

        clientStDrain :: SlotNo -> Word64 -> [GenTx block] -> m (LMM.ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
        clientStDrain !slot !start !txs =
            pure $ LMM.SendMsgNextTx $ \case
                Nothing -> do
                    end <- getMonotonicTimeNSec
                    let revTxs = reverse txs
                    atomically $ do
                        writeTVar mempoolSnapshot (Just (slot, start))
                        putTMVar mempool revTxs
                    logWith tr $ TxSubmissionLocalMempoolSynchronized
                        { took = Microseconds $ (end - start) `div` 1000
                        , transactions = encodeBase16 . fromShort . toRawTxIdHash . txId <$> revTxs
                        }
                    pure $ LMM.SendMsgAwaitAcquire clientStAwaitAcquire
                Just tx -> do
                    clientStDrain slot start (tx : txs)

    localStateQueryClient
        :: m (SomeEvaluationInAnyEra crypto)
        -> (EvaluateTransactionResponse block -> m ())
        -> LocalStateQueryClient block (Point block) (Query block) m ()
    localStateQueryClient await reply =
        LocalStateQueryClient clientStIdle
      where
        clientStIdle
            :: m (LSQ.ClientStIdle block (Point block) (Query block) m ())
        clientStIdle = pure $ do
            -- NOTE: This little 'dance' of acquiring first is needed because of:
            --
            --     https://github.com/CardanoSolutions/ogmios/issues/230
            --
            -- and ideally, can be removed once the upstream fix:
            --
            --     https://github.com/input-output-hk/ouroboros-network/pull/3844
            --
            -- has shipped with cardano-node (and it's been long-enough that we
            -- can exclude old clients from needing this).
            LSQ.SendMsgAcquire VolatileTip $ LSQ.ClientStAcquiring
                { LSQ.recvMsgAcquired = do
                    reAcquire <$> await
                , LSQ.recvMsgFailure =
                    const clientStIdle
                }

        clientStAcquiring
            :: SomeEvaluationInAnyEra crypto
            -> LSQ.ClientStAcquiring block (Point block) (Query block) m ()
        clientStAcquiring args =
            LSQ.ClientStAcquiring
                { LSQ.recvMsgAcquired = pure $ selectEra
                    -- Default / Fallback
                    (\era -> do
                        reply (nodeTipTooOld era)
                        pure $ LSQ.SendMsgRelease clientStIdle
                    )
                    -- Babbage
                    (clientStAcquired0 @_ @(BabbageEra crypto) args evaluateExecutionUnits)
                    -- Conway
                    (clientStAcquired0 @_ @(ConwayEra crypto) args evaluateExecutionUnits)
                , LSQ.recvMsgFailure =
                    const $ pure $ LSQ.SendMsgAcquire VolatileTip (clientStAcquiring args)
                }

        reAcquire
            :: SomeEvaluationInAnyEra crypto
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        reAcquire
            = LSQ.SendMsgReAcquire VolatileTip . clientStAcquiring

        clientStAcquired0
            :: forall proto era. (CanEvaluateScriptsInEra era, EraCrypto era ~ crypto)
            => SomeEvaluationInAnyEra crypto
            -> (  PParams era
               -> SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTransactionResponse block
               )
            -> HoistQuery proto era
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
            :: forall proto era. (CanEvaluateScriptsInEra era, EraCrypto era ~ crypto)
            => SomeEvaluationInAnyEra crypto
            -> (  SystemStart
               -> EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTransactionResponse block
               )
            -> HoistQuery proto era
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired1 args callback hoistQuery = do
            let query = GetSystemStart
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \systemStart ->
                    pure $ clientStAcquired2 args (callback systemStart) hoistQuery
                }

        clientStAcquired2
            :: forall proto era. (CanEvaluateScriptsInEra era, EraCrypto era ~ crypto)
            => SomeEvaluationInAnyEra crypto
            -> (  EpochInfo (Except PastHorizonException)
               -> UTxO era
               -> Tx era
               -> EvaluateTransactionResponse block
               )
            -> HoistQuery proto era
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired2 args callback hoistQuery = do
            let query = BlockQuery $ QueryHardFork GetInterpreter
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \(interpreterToEpochInfo -> epochInfo) ->
                    pure $ clientStAcquired3 args (callback epochInfo) hoistQuery
                }

        clientStAcquired3
            :: forall proto era. (CanEvaluateScriptsInEra era, EraCrypto era ~ crypto)
            => SomeEvaluationInAnyEra crypto
            -> (  UTxO era
               -> Tx era
               -> EvaluateTransactionResponse block
               )
            -> HoistQuery proto era
            -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
        clientStAcquired3 args callback hoistQuery = do
            let query = BlockQuery $ hoistQuery $ GetUTxOByTxIn $ inputsInAnyEra args
             in LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { LSQ.recvMsgResult = \case
                    Right networkUtxo -> do
                        reply $! newEvaluateTransactionResponse @era
                            callback EvaluationFailure networkUtxo args
                        pure (LSQ.SendMsgRelease clientStIdle)
                    Left{} ->
                        pure $ reAcquire args
                }

type HoistQuery proto era =
    forall r a crypto. (crypto ~ EraCrypto era, CardanoQueryResult crypto r ~ a)
        => BlockQuery (ShelleyBlock (proto crypto) era) r
        -> BlockQuery (CardanoBlock crypto) a

-- | Run local-state queries in either Babbage or Conway depending on where the network is at.
--
-- This is crucial to allow the server to *dynamically* switch to the right era after the hard-fork.
selectEra
    :: forall block crypto m.
        ( block ~ HardForkBlock (CardanoEras crypto)
        , Applicative m
        )

    -- Default selector, when the network era is older than Alonzo and doesn't support Phase-2 scripts.
    => ( Text -> m (LSQ.ClientStAcquired block (Point block) (Query block) m ())
       )

    -- Selector for the Babbage era.
    -> ( HoistQuery Praos (BabbageEra crypto) -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
       )

    -- Selector for the Babbage era.
    -> ( HoistQuery Praos (ConwayEra crypto) -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
       )

    -> LSQ.ClientStAcquired block (Point block) (Query block) m ()
selectEra fallback asBabbage asConway =
    LSQ.SendMsgQuery (Ledger.BlockQuery $ HF.QueryHardFork HF.GetCurrentEra) $
    LSQ.ClientStQuerying
        { LSQ.recvMsgResult = \case
            EraIndex                   Z{}       -> fallback "Byron"
            EraIndex                (S Z{})      -> fallback "Shelley"
            EraIndex             (S (S Z{}))     -> fallback "Allegra"
            EraIndex          (S (S (S Z{})))    -> fallback "Mary"
            EraIndex       (S (S (S (S Z{}))))   -> fallback "Alonzo"
            EraIndex    (S (S (S (S (S Z{})))))  -> pure (asBabbage QueryIfCurrentBabbage)
            EraIndex (S (S (S (S (S (S Z{})))))) -> pure (asConway QueryIfCurrentConway)
        }

--
-- SomeEvaluationInAnyEra
--

data SomeEvaluationInAnyEra crypto where
    SomeEvaluationInAnyEra
        :: forall era crypto. (CanEvaluateScriptsInEra era, EraCrypto era ~ crypto)
        => !(UTxO era)
        -> !(Tx era)
        -> SomeEvaluationInAnyEra crypto

-- | Return all unspent transaction outputs needed for evaluation. This includes
-- standard inputs, but also reference ones and collaterals.
inputsInAnyEra
    :: SomeEvaluationInAnyEra crypto
    -> Set (TxIn crypto)
inputsInAnyEra (SomeEvaluationInAnyEra _ tx) =
    tx ^. bodyTxL . inputsTxBodyL
    <>
    tx ^. bodyTxL . collateralInputsTxBodyL
    <>
    tx ^. bodyTxL . referenceInputsTxBodyL

newEvaluateTransactionResponse
    :: forall era crypto result.
        ( CanEvaluateScriptsInEra era
        , crypto ~ EraCrypto era
        )
    => (UTxO era -> Tx era -> result)
    -> (EvaluateTransactionError crypto -> result)
    -> UTxO era -- ^ Utxo fetched from the network
    -> SomeEvaluationInAnyEra crypto -- ^ Tx & additional utxo
    -> result
newEvaluateTransactionResponse callback onError (UTxO networkUtxo) args =
    case translateToNetworkEra @era args of
        Nothing ->
            error "impossible: arguments are not translatable to network era. \
                  \This can't happen because 'selectEra' enforces that queries \
                  \are only executed if the network is either in 'Babbage' or \
                  \'Conway'. The arguments themselves can also only be 'Babbage' \
                  \or 'Conway'. Thus, we can't reach this point with any other \
                  \eras. We _could_ potentially capture this proof in the types \
                  \to convince the compiler of this."
        Just (UTxO userProvidedUtxo, tx) ->
            let
                intersection = Map.intersectionWith (==) userProvidedUtxo networkUtxo
            in
                if and intersection
                then
                    callback (UTxO $ userProvidedUtxo <> networkUtxo) tx
                else
                    intersection
                        & Map.keysSet
                        & OverlappingAdditionalUtxo
                        & onError

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
    :: forall eraNetwork.
        ( CanEvaluateScriptsInEra eraNetwork
        )
    => SomeEvaluationInAnyEra (EraCrypto eraNetwork)
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
            eraArgs    = typeRep @eraArgs
            eraBabbage = typeRep @(BabbageEra StandardCrypto)
            eraConway  = typeRep @(ConwayEra StandardCrypto)

            sameEra =
                case testEquality eraNetwork eraArgs of
                    Just Refl ->
                        Just (utxo, tx)
                    _ ->
                        Nothing

            babbageToConway =
                case (testEquality eraNetwork eraConway, testEquality eraArgs eraBabbage) of
                    (Just Refl, Just Refl) -> do
                        Just (upgrade utxo, upgrade tx)
                    _ ->
                        Nothing
          in
            sameEra <|> babbageToConway

--
-- Logs
--

data TraceTxSubmission  where
    TxSubmissionEvaluateArguments
        :: { utxoEra :: Text, transactionEra :: Text }
        -> TraceTxSubmission
    TxSubmissionLocalMempoolTryRead
        :: TraceTxSubmission
    TxSubmissionLocalMempoolCurrent
        :: { transactions :: [Text] }
        -> TraceTxSubmission
    TxSubmissionLocalMempoolCleared
        :: { atSlot :: Word64, ns :: Word64 }
        -> TraceTxSubmission
    TxSubmissionLocalMempoolNewSnapshot
        :: { atSlot :: Word64, ns :: Word64 }
        -> TraceTxSubmission
    TxSubmissionLocalMempoolSynchronized
        :: { took :: Microseconds, transactions :: [Text] }
        -> TraceTxSubmission
    TxSubmissionEvaluateUtxoProvidedByUser
        :: { utxoRefs :: [Text] }
        -> TraceTxSubmission
    TxSubmissionEvaluateUtxoInferredFromMempool
        :: { utxoRefs :: [Text] }
        -> TraceTxSubmission
    deriving (Show, Generic)

instance ToJSON TraceTxSubmission where
    toEncoding = genericToEncoding Json.defaultOptions

instance HasSeverityAnnotation TraceTxSubmission where
    getSeverityAnnotation = \case
        TxSubmissionEvaluateArguments{} -> Debug
        TxSubmissionLocalMempoolTryRead{} -> Debug
        TxSubmissionLocalMempoolCurrent{} -> Debug
        TxSubmissionLocalMempoolCleared{} -> Info
        TxSubmissionLocalMempoolNewSnapshot{} -> Info
        TxSubmissionLocalMempoolSynchronized{ took } ->
            if took > threshold then Warning else Info
        TxSubmissionEvaluateUtxoProvidedByUser{ utxoRefs } ->
            if null utxoRefs then Debug else Info
        TxSubmissionEvaluateUtxoInferredFromMempool{ utxoRefs } ->
            if null utxoRefs then Debug else Info
      where
        threshold = Microseconds (1_000_000 :: Integer)
