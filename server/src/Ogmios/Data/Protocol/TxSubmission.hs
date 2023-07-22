--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- NOTE:
-- Needed to derive 'ToJSON' and 'Show' instances for 'SubmitResult'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Protocol.TxSubmission
    ( -- * Codecs
      TxSubmissionCodecs (..)
    , mkTxSubmissionCodecs

      -- * Messages
    , TxSubmissionMessage (..)

      -- ** SubmitTransaction
    , SubmitTransaction (..)
    , _decodeSubmitTransaction
    , SubmitTransactionResponse (..)
    , _encodeSubmitTransactionResponse
    , mkSubmitTransactionResponse

      -- ** EvaluateTransaction
    , EvaluateTransaction (..)
    , _decodeEvaluateTransaction
    , EvaluateTransactionResponse (..)
    , EvaluateTransactionError (..)
    , NodeTipTooOldError (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , unsupportedEra
    , nodeTipTooOld
    , _encodeEvaluateTransactionResponse
    , CanEvaluateScriptsInEra

      -- ** Re-exports
    , AlonzoEra
    , ConwayEra
    , BabbageEra
    , EpochInfo
    , ExUnits
    , GenTxId
    , HasTxId
    , PastHorizonException
    , RdmrPtr
    , TransactionScriptFailure
    , SerializedTransaction
    , SubmitTransactionError
    , SystemStart
    , Core.PParams
    , Core.Tx
    , TxIn
    , UTxO (..)
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Alonzo
    ( AlonzoEra
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript
    , ExUnits (..)
    , Script
    )
import Cardano.Ledger.Alonzo.Tx
    ( AlonzoEraTx
    )
import Cardano.Ledger.Alonzo.TxInfo
    ( ExtendedUTxO
    , TranslationError (..)
    )
import Cardano.Ledger.Alonzo.TxWits
    ( RdmrPtr (..)
    )
import Cardano.Ledger.Alonzo.UTxO
    ( AlonzoScriptsNeeded (..)
    )
import Cardano.Ledger.Api
    ( TransactionScriptFailure
    , evalTxExUnits
    )
import Cardano.Ledger.Babbage
    ( BabbageEra
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageEraTxBody
    )
import Cardano.Ledger.Conway
    ( ConwayEra
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Era
    ( EraCrypto
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Cardano.Ledger.TxIn
    ( TxIn
    )
import Cardano.Ledger.UTxO
    ( EraUTxO (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( Crypto
    , GenTxId
    , SerializedTransaction
    , SubmitTransactionError
    )
import Cardano.Slotting.EpochInfo
    ( EpochInfo
    , hoistEpochInfo
    )
import Cardano.Slotting.Time
    ( SystemStart
    )
import Control.Arrow
    ( left
    )
import Control.Monad.Trans.Except
    ( Except
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO
    )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..)
    )

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Era

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map

--
-- Codecs
--

data TxSubmissionCodecs block = TxSubmissionCodecs
    { decodeSubmitTransaction
        :: ByteString
        -> Maybe (Rpc.Request (SubmitTransaction block))
    , encodeSubmitTransactionResponse
        :: Rpc.Response (SubmitTransactionResponse block)
        -> Json
    , decodeEvaluateTransaction
        :: ByteString
        -> Maybe (Rpc.Request (EvaluateTransaction block))
    , encodeEvaluateTransactionResponse
        :: Rpc.Response (EvaluateTransactionResponse block)
        -> Json
    }

mkTxSubmissionCodecs
    :: forall block.
        ( FromJSON (SerializedTransaction block)
        , FromJSON (MultiEraUTxO block)
        )
    => (GenTxId block -> Json)
    -> (SubmitTransactionError block -> Json)
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (TransactionScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> (TranslationError (Crypto block) -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs encodeTxId encodeSubmitTransactionError stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError =
    TxSubmissionCodecs
        { decodeSubmitTransaction =
            decodeWith _decodeSubmitTransaction
        , encodeSubmitTransactionResponse =
            _encodeSubmitTransactionResponse (Proxy @block)
                encodeTxId
                encodeSubmitTransactionError
        , decodeEvaluateTransaction =
            decodeWith _decodeEvaluateTransaction
        , encodeEvaluateTransactionResponse =
            _encodeEvaluateTransactionResponse (Proxy @block)
                stringifyRdmrPtr
                encodeExUnits
                encodeScriptFailure
                encodeTxIn
                encodeTranslationError
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgSubmitTransaction
        (SubmitTransaction block)
        (Rpc.ToResponse (SubmitTransactionResponse block))
        Rpc.ToFault
    | MsgEvaluateTransaction
        (EvaluateTransaction block)
        (Rpc.ToResponse (EvaluateTransactionResponse block))
        Rpc.ToFault

--
-- SubmitTransaction
--

data SubmitTransaction block
    = SubmitTransaction { transaction :: SerializedTransaction block }
    deriving (Generic)
deriving instance Show (SerializedTransaction block) => Show (SubmitTransaction block)

_decodeSubmitTransaction
    :: FromJSON (SerializedTransaction block)
    => Json.Value
    -> Json.Parser (Rpc.Request (SubmitTransaction block))
_decodeSubmitTransaction =
    Rpc.genericFromJSON Rpc.defaultOptions

data SubmitTransactionResponse block
    = SubmitTransactionSuccess (GenTxId block)
    | SubmitTransactionFailure (SubmitTransactionError block)
    deriving (Generic)
deriving instance
    ( Show (SubmitTransactionError block)
    , Show (GenTxId block)
    ) => Show (SubmitTransactionResponse block)

_encodeSubmitTransactionResponse
    :: forall block. ()
    => Proxy block
    -> (GenTxId block -> Json)
    -> (SubmitTransactionError block -> Json)
    -> Rpc.Response (SubmitTransactionResponse block)
    -> Json
_encodeSubmitTransactionResponse _proxy encodeTransactionId encodeSubmitTransactionError =
    Rpc.mkResponse $ \resolve reject -> \case
        SubmitTransactionSuccess i ->
            resolve $ encodeObject
                ( "transaction" .= encodeObject
                    ( "id" .= encodeTransactionId i
                    )
                )
        SubmitTransactionFailure e ->
            -- TODO: More fine-grained error codes
            reject (Rpc.FaultCustom 3005)
                "Transaction submission failed"
                ( pure $ encodeSubmitTransactionError e
                )

-- | Translate an ouroboros-network's 'SubmitResult' into our own
-- 'SubmitTransactionResponse' which also carries a transaction id.
mkSubmitTransactionResponse
    :: HasTxId (SerializedTransaction block)
    => SerializedTransaction block
    -> SubmitResult (SubmitTransactionError block)
    -> SubmitTransactionResponse block
mkSubmitTransactionResponse tx = \case
    SubmitSuccess ->
        SubmitTransactionSuccess (txId tx)
    SubmitFail e ->
        SubmitTransactionFailure e

--
-- EvaluateTransaction
--

data EvaluateTransaction block
    = EvaluateTransaction
        { transaction :: SerializedTransaction block
        , additionalUtxoSet :: MultiEraUTxO block
        }
    deriving (Generic)
deriving instance
    ( Show (SerializedTransaction block)
    , Show (MultiEraUTxO block)
    ) => Show (EvaluateTransaction block)

_decodeEvaluateTransaction
    :: forall block.
        ( FromJSON (SerializedTransaction block)
        , FromJSON (MultiEraUTxO block)
        )
    => Json.Value
    -> Json.Parser (Rpc.Request (EvaluateTransaction block))
_decodeEvaluateTransaction =
    Rpc.genericFromJSON $ Rpc.defaultOptions
        { Rpc.onMissingField = \fieldName ->
            if fieldName == "additionalUtxoSet" then
                pure (Json.Array mempty)
            else
                Rpc.onMissingField Rpc.defaultOptions fieldName
        }

data EvaluateTransactionResponse block
    = EvaluationFailure (EvaluateTransactionError block)
    | EvaluationResult (Map RdmrPtr ExUnits)
    deriving (Show)

data EvaluateTransactionError block
    = ScriptExecutionFailures (Map RdmrPtr [TransactionScriptFailure (Crypto block)])
    | IncompatibleEra Text
    | UnsupportedEra Text
    | OverlappingAdditionalUtxo (Set (TxIn (Crypto block)))
    | NodeTipTooOldErr NodeTipTooOldError
    | CannotCreateEvaluationContext (TranslationError (Crypto block))
    deriving (Show)

data NodeTipTooOldError = NodeTipTooOld
    { currentNodeEra :: Text
    , minimumRequiredEra :: Text
    }
    deriving (Show)

-- | Shorthand constructor for 'EvaluateTransactionResponse'
unsupportedEra :: Text -> EvaluateTransactionResponse block
unsupportedEra =
    EvaluationFailure . UnsupportedEra

-- | Shorthand constructor for 'EvaluateTransactionResponse'
incompatibleEra :: Text -> EvaluateTransactionResponse block
incompatibleEra =
    EvaluationFailure . IncompatibleEra

-- | Shorthand constructor for 'EvaluateTransactionResponse'
nodeTipTooOld :: Text -> EvaluateTransactionResponse block
nodeTipTooOld currentNodeEra =
    EvaluationFailure (NodeTipTooOldErr $
        NodeTipTooOld { currentNodeEra, minimumRequiredEra }
    )
  where
    minimumRequiredEra = "alonzo"

_encodeEvaluateTransactionResponse
    :: forall block. ()
    => Proxy block
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (TransactionScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> (TranslationError (Crypto block) -> Json)
    -> Rpc.Response (EvaluateTransactionResponse block)
    -> Json
_encodeEvaluateTransactionResponse _proxy stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError =
    Rpc.mkResponse $ \resolve reject -> \case
        EvaluationResult result ->
            resolve $ encodeObject
                ( "budgets" .= encodeMap stringifyRdmrPtr encodeExUnits result
                )
        EvaluationFailure (IncompatibleEra era) ->
            reject (Rpc.FaultCustom 3000)
                "Trying to evaluate a transaction from an old era (prior to Alonzo)."
                (pure $ encodeObject
                    ( "incompatibleEra" .=
                        encodeEraName era
                    )
                )
        EvaluationFailure (UnsupportedEra era) ->
            reject (Rpc.FaultCustom 3001)
                "Trying to evaluate a transaction from an era that's no longer supported \
                \(e.g. Alonzo). Please use a more recent transaction format."
                (pure $ encodeObject
                    ( "unsupportedEra" .=
                        encodeEraName era
                    )
                )
        EvaluationFailure (OverlappingAdditionalUtxo inputs) ->
            reject (Rpc.FaultCustom 3002)
                "Some user-provided additional UTxO entries overlap with those that exist \
                \in the ledger."
                (pure $ encodeObject
                    ( "overlappingOutputReferences" .=
                        encodeFoldable encodeTxIn inputs
                    )
                )
        EvaluationFailure (NodeTipTooOldErr err) ->
            reject (Rpc.FaultCustom 3003)
                "The node is still synchronizing and the ledger isn't yet in an era where \
                \scripts are enabled (i.e. Alonzo and beyond)."
                (pure $ encodeObject
                    ( "currentNodeEra" .=
                        encodeEraName (currentNodeEra err) <>
                      "minimumRequiredEra" .=
                        encodeEraName (minimumRequiredEra err)
                    )
                )
        EvaluationFailure (CannotCreateEvaluationContext err) ->
            reject (Rpc.FaultCustom 3004)
                "Unable to create the evaluation context from the given transaction."
                (pure $ encodeObject
                    ( "reason" .=
                        encodeTranslationError err
                    )
                )
        EvaluationFailure (ScriptExecutionFailures failures) ->
            -- TODO: Breakdown / promote nested errors into fine-grained errors
            reject (Rpc.FaultCustom 3005)
                "Some scripts of the transactions terminated with error(s)."
                (pure $ encodeMap
                    stringifyRdmrPtr
                    (encodeList encodeScriptFailure)
                    failures
                )

-- | A constraint synonym to bundle together constraints needed to run a script
-- evaluation in any era after Alonzo (incl.).
type CanEvaluateScriptsInEra era =
      ( AlonzoEraTx era
      , BabbageEraTxBody era
      , ExtendedUTxO era
      , EraUTxO era
      , ScriptsNeeded era ~ AlonzoScriptsNeeded era
      , Script era ~ AlonzoScript era
      , EraCrypto era ~ StandardCrypto
      )

-- | Evaluate script executions units for the given transaction.
evaluateExecutionUnits
    :: forall era block.
      ( CanEvaluateScriptsInEra era
      , Era.EraCrypto era ~ Crypto block
      )
    => Core.PParams era
        -- ^ Protocol parameters
    -> SystemStart
        -- ^ Start of the blockchain, for converting slots to UTC times
    -> EpochInfo (Except PastHorizonException)
        -- ^ Information about epoch sizes, for converting slots to UTC times
    -> UTxO era
        -- ^ A UTXO needed to resolve inputs
    -> Core.Tx era
        -- ^ The actual transaction
    -> EvaluateTransactionResponse block
evaluateExecutionUnits pparams systemStart epochInfo utxo tx = case evaluation of
    Left err ->
        EvaluationFailure (CannotCreateEvaluationContext err)
    Right reports ->
        let (failures, successes) =
                Map.foldrWithKey aggregateReports (mempty, mempty)  reports
         in if null failures
            then EvaluationResult successes
            else EvaluationFailure $ ScriptExecutionFailures failures
  where
    aggregateReports
        :: RdmrPtr
        -> Either (TransactionScriptFailure (Era.EraCrypto era)) ExUnits
        -> (Map RdmrPtr [TransactionScriptFailure (Era.EraCrypto era)], Map RdmrPtr ExUnits)
        -> (Map RdmrPtr [TransactionScriptFailure (Era.EraCrypto era)], Map RdmrPtr ExUnits)
    aggregateReports ptr result (failures, successes) = case result of
        Left scriptFailure ->
            ( Map.unionWith (++) (Map.singleton ptr [scriptFailure]) failures
            , successes
            )
        Right exUnits ->
            ( failures
            , Map.singleton ptr exUnits <> successes
            )

    evaluation
        :: Either
            (TranslationError (Crypto block))
            (Map RdmrPtr (Either (TransactionScriptFailure (Era.EraCrypto era)) ExUnits))
    evaluation =
        evalTxExUnits
          pparams
          tx
          utxo
          (hoistEpochInfo (left show . runIdentity . runExceptT) epochInfo)
          systemStart
