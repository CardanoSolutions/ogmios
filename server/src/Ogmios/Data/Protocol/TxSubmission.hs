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
import Cardano.Ledger.Babbage.TxBody
    ( BabbageEraTxBody
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
    ( GenTxId
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
import Ogmios.Data.Ledger.ScriptFailure
    ( pickScriptFailure
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

import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Core as Core

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Encoding as Json
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
        ( FromJSON (MultiEraDecoder (SerializedTransaction block))
        , FromJSON (MultiEraUTxO block)
        )
    => (GenTxId block -> Json)
    -> (RdmrPtr -> Json)
    -> (ExUnits -> Json)
    -> (TxIn (BlockCrypto block) -> Json)
    -> (TranslationError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> TransactionScriptFailure (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs
    encodeTxId
    encodeRdmrPtr
    encodeExUnits
    encodeTxIn
    encodeTranslationError
    encodeSubmitTransactionError
    encodeScriptFailure
    encodeDeserialisationFailure
    =
    TxSubmissionCodecs
        { decodeSubmitTransaction =
            decodeWith _decodeSubmitTransaction
        , encodeSubmitTransactionResponse =
            _encodeSubmitTransactionResponse (Proxy @block)
                encodeTxId
                encodeSubmitTransactionError
                encodeDeserialisationFailure
        , decodeEvaluateTransaction =
            decodeWith _decodeEvaluateTransaction
        , encodeEvaluateTransactionResponse =
            _encodeEvaluateTransactionResponse (Proxy @block)
                encodeRdmrPtr
                encodeExUnits
                encodeTxIn
                encodeTranslationError
                encodeScriptFailure
                encodeDeserialisationFailure
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
    = SubmitTransaction { transaction :: MultiEraDecoder (SerializedTransaction block) }
    deriving (Generic)
deriving instance Show (SerializedTransaction block) => Show (SubmitTransaction block)

_decodeSubmitTransaction
    :: FromJSON (MultiEraDecoder (SerializedTransaction block))
    => Json.Value
    -> Json.Parser (Rpc.Request (SubmitTransaction block))
_decodeSubmitTransaction =
    Rpc.genericFromJSON Rpc.defaultOptions

data SubmitTransactionResponse block
    = SubmitTransactionSuccess (GenTxId block)
    | SubmitTransactionFailure (SubmitTransactionError block)
    | SubmitTransactionDeserialisationFailure [(SomeShelleyEra, Binary.DecoderError, Word)]
    deriving (Generic)
deriving instance
    ( Show (SubmitTransactionError block)
    , Show (GenTxId block)
    ) => Show (SubmitTransactionResponse block)

_encodeSubmitTransactionResponse
    :: forall block. ()
    => Proxy block
    -> (GenTxId block -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (SubmitTransactionResponse block)
    -> Json
_encodeSubmitTransactionResponse _proxy
    encodeTransactionId
    encodeSubmitTransactionError
    encodeDeserialisationFailure
    =
    Rpc.mkResponse $ \resolve reject -> \case
        SubmitTransactionSuccess i ->
            resolve $ encodeObject
                ( "transaction" .= encodeObject
                    ( "id" .= encodeTransactionId i
                    )
                )
        SubmitTransactionFailure e ->
            encodeSubmitTransactionError reject e
        SubmitTransactionDeserialisationFailure errs ->
            encodeDeserialisationFailure reject errs

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
        { transaction :: MultiEraDecoder (SerializedTransaction block)
        , additionalUtxoSet :: MultiEraUTxO block
        }
    deriving (Generic)
deriving instance
    ( Show (SerializedTransaction block)
    , Show (MultiEraUTxO block)
    ) => Show (EvaluateTransaction block)

_decodeEvaluateTransaction
    :: forall block.
        ( FromJSON (MultiEraDecoder (SerializedTransaction block))
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
    | EvaluateTransactionDeserialisationFailure [(SomeShelleyEra, Binary.DecoderError, Word)]
    deriving (Show)

data EvaluateTransactionError block
    = ScriptExecutionFailures (Map RdmrPtr [TransactionScriptFailure (BlockCrypto block)])
    | IncompatibleEra Text
    | UnsupportedEra Text
    | OverlappingAdditionalUtxo (Set (TxIn (BlockCrypto block)))
    | NodeTipTooOldErr NodeTipTooOldError
    | CannotCreateEvaluationContext (TranslationError (BlockCrypto block))
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
    -> (RdmrPtr -> Json)
    -> (ExUnits -> Json)
    -> (TxIn (BlockCrypto block) -> Json)
    -> (TranslationError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> TransactionScriptFailure (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (EvaluateTransactionResponse block)
    -> Json
_encodeEvaluateTransactionResponse _proxy
    encodeRdmrPtr
    encodeExUnits
    encodeTxIn
    encodeTranslationError
    encodeScriptFailure
    encodeDeserialisationFailure
    =
    Rpc.mkResponse $ \resolve reject -> \case
        EvaluationResult budgets ->
            resolve $ encodeList identity $ Map.foldrWithKey
                (\ptr result xs ->
                    encodeObject
                        ( "validator" .= encodeRdmrPtr ptr
                       <> "budget" .= encodeExUnits result
                        ) : xs
                ) [] budgets

        EvaluateTransactionDeserialisationFailure errs ->
            encodeDeserialisationFailure reject errs

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
            reject (Rpc.FaultCustom 3010)
                "Some scripts of the transactions terminated with error(s)."
                (pure $ encodeList identity $ Map.foldrWithKey
                    (\ptr e xs ->
                        if null e then
                            xs
                        else
                            let embed code msg details = encodeObject
                                    ( "validator" .= encodeRdmrPtr ptr
                                   <> "error" .= encodeObject
                                        ( "code" .= Json.toEncoding code
                                       <> "message" .= Json.toEncoding msg
                                       <> maybe mempty (Json.pair "data") details
                                        )
                                    )
                             in encodeScriptFailure embed (pickScriptFailure e) : xs
                    ) [] failures
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
      , EraCrypto era ~ BlockCrypto block
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
        -> Either (TransactionScriptFailure (EraCrypto era)) ExUnits
        -> (Map RdmrPtr [TransactionScriptFailure (EraCrypto era)], Map RdmrPtr ExUnits)
        -> (Map RdmrPtr [TransactionScriptFailure (EraCrypto era)], Map RdmrPtr ExUnits)
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
            (TranslationError (BlockCrypto block))
            (Map RdmrPtr (Either (TransactionScriptFailure (EraCrypto era)) ExUnits))
    evaluation =
        evalTxExUnits
          pparams
          tx
          utxo
          (hoistEpochInfo (left show . runIdentity . runExceptT) epochInfo)
          systemStart
