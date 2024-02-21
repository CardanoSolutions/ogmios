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

import Cardano.Ledger.Alonzo.Plutus.Context
    ( ContextError (..)
    , EraPlutusContext
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript
    , AsIndex
    , ExUnits (..)
    , PlutusPurpose
    , Script
    )
import Cardano.Ledger.Alonzo.Tx
    ( AlonzoEraTx
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
import Ogmios.Data.Ledger
    ( ContextErrorInAnyEra (..)
    , ScriptPurposeIndexInAnyEra (..)
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( EvaluateTransactionError (..)
    , NodeTipTooOldError (..)
    , TransactionScriptFailureInAnyEra (..)
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
    => Rpc.Options
    -> (GenTxId block -> Json)
    -> (ScriptPurposeIndexInAnyEra (BlockCrypto block) -> Json)
    -> (ExUnits -> Json)
    -> (Rpc.EmbedFault -> EvaluateTransactionError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs
    opts
    encodeTxId
    encodeScriptPurposeIndex
    encodeExUnits
    encodeEvaluationError
    encodeSubmitTransactionError
    encodeDeserialisationFailure
    =
    TxSubmissionCodecs
        { decodeSubmitTransaction =
            decodeWith _decodeSubmitTransaction
        , encodeSubmitTransactionResponse =
            _encodeSubmitTransactionResponse (Proxy @block)
                opts
                encodeTxId
                encodeSubmitTransactionError
                encodeDeserialisationFailure
        , decodeEvaluateTransaction =
            decodeWith _decodeEvaluateTransaction
        , encodeEvaluateTransactionResponse =
            _encodeEvaluateTransactionResponse (Proxy @block)
                opts
                encodeScriptPurposeIndex
                encodeExUnits
                encodeEvaluationError
                encodeDeserialisationFailure
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgSubmitTransaction
        (SubmitTransaction block)
        (Rpc.ToResponse (SubmitTransactionResponse block))
    | MsgEvaluateTransaction
        (EvaluateTransaction block)
        (Rpc.ToResponse (EvaluateTransactionResponse block))

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
    -> Rpc.Options
    -> (GenTxId block -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (SubmitTransactionResponse block)
    -> Json
_encodeSubmitTransactionResponse _proxy
    opts
    encodeTransactionId
    encodeSubmitTransactionError
    encodeDeserialisationFailure
    =
    Rpc.mkResponse opts $ \resolve reject -> \case
        SubmitTransactionSuccess i ->
            resolve $ encodeObject ("transaction" .= encodeTransactionId i)
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
        , additionalUtxo :: MultiEraUTxO block
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
            if fieldName == "additionalUtxo" then
                pure (Json.Array mempty)
            else
                Rpc.onMissingField Rpc.defaultOptions fieldName
        }

data EvaluateTransactionResponse block
    = EvaluationFailure (EvaluateTransactionError (BlockCrypto block))
    | EvaluationResult (Map (ScriptPurposeIndexInAnyEra (BlockCrypto block)) ExUnits)
    | EvaluateTransactionDeserialisationFailure [(SomeShelleyEra, Binary.DecoderError, Word)]

deriving instance Crypto (BlockCrypto block) => Show (EvaluateTransactionResponse block)

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

-- TODO: Move those instances somewhere near other JSON instances.
_encodeEvaluateTransactionResponse
    :: forall block. ()
    => Proxy block
    -> Rpc.Options
    -> (ScriptPurposeIndexInAnyEra (BlockCrypto block) -> Json)
    -> (ExUnits -> Json)
    -> (Rpc.EmbedFault -> EvaluateTransactionError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (EvaluateTransactionResponse block)
    -> Json
_encodeEvaluateTransactionResponse _proxy
    opts
    encodeRdmrPtr
    encodeExUnits
    encodeEvaluationError
    encodeDeserialisationFailure
    =
    Rpc.mkResponse opts $ \resolve reject -> \case
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

        EvaluationFailure e ->
            encodeEvaluationError reject e

-- | A constraint synonym to bundle together constraints needed to run a script
-- evaluation in any era after Alonzo (incl.).
type CanEvaluateScriptsInEra era =
      ( AlonzoEraTx era
      , BabbageEraTxBody era
      , EraPlutusContext era
      , EraUTxO era
      , ScriptsNeeded era ~ AlonzoScriptsNeeded era
      , Script era ~ AlonzoScript era
      , EraPlutusContext era
      , IsAlonzoBasedEra era
      , Crypto (EraCrypto era)
      )

-- | Evaluate script executions units for the given transaction.
evaluateExecutionUnits
    :: forall era block ix crypto.
      ( CanEvaluateScriptsInEra (era crypto)
      , ix ~ ScriptPurposeIndexInAnyEra crypto
      , crypto ~ BlockCrypto block
      , crypto ~ EraCrypto (era crypto)
      )
    => Core.PParams (era crypto)
        -- ^ Protocol parameters
    -> SystemStart
        -- ^ Start of the blockchain, for converting slots to UTC times
    -> EpochInfo (Except PastHorizonException)
        -- ^ Information about epoch sizes, for converting slots to UTC times
    -> UTxO (era crypto)
        -- ^ A UTXO needed to resolve inputs
    -> Core.Tx (era crypto)
        -- ^ The actual transaction
    -> EvaluateTransactionResponse block
evaluateExecutionUnits pparams systemStart epochInfo utxo tx = case evaluation of
    Left err ->
        let errInAnyEra = ContextErrorInAnyEra (alonzoBasedEra @(era crypto), err)
         in EvaluationFailure (CannotCreateEvaluationContext errInAnyEra)
    Right reports ->
        let (failures, successes) =
                Map.foldrWithKey aggregateReports (mempty, mempty)  reports
         in if null failures
            then EvaluationResult successes
            else EvaluationFailure $ ScriptExecutionFailures failures
  where
    aggregateReports
        :: PlutusPurpose AsIndex (era crypto)
        -> Either (TransactionScriptFailure (era crypto)) ExUnits
        -> (Map ix [TransactionScriptFailureInAnyEra crypto], Map ix ExUnits)
        -> (Map ix [TransactionScriptFailureInAnyEra crypto], Map ix ExUnits)
    aggregateReports ptr result (failures, successes) =
        case result of
            Left scriptFailure ->
                let failureInAnyEra = TransactionScriptFailureInAnyEra @crypto (alonzoBasedEra @(era crypto), scriptFailure)
                 in ( Map.unionWith (++) (Map.singleton ix [failureInAnyEra]) failures
                    , successes
                    )
            Right exUnits ->
                ( failures
                , Map.singleton ix exUnits <> successes
                )
      where
        ix = ScriptPurposeIndexInAnyEra (alonzoBasedEra @(era crypto), ptr)

    evaluation
        :: Either
            (ContextError (era crypto))
            (Map
                (PlutusPurpose AsIndex (era crypto))
                (Either (TransactionScriptFailure (era crypto)) ExUnits)
            )
    evaluation =
        evalTxExUnits
          pparams
          tx
          utxo
          (hoistEpochInfo (left show . runIdentity . runExceptT) epochInfo)
          systemStart
