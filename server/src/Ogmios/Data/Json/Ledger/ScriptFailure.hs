--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Ledger.ScriptFailure where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Json.Ledger.PredicateFailure
    ( encodeContextErrorInAnyEra
    , encodePredicateFailure
    , encodeScriptPurposeIndexInAnyEra
    )
import Ogmios.Data.Json.Shelley
    ( encodeTxIn
    )
import Ogmios.Data.Ledger
    ( ContextErrorInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    , ScriptPurposeIndexInAnyEra (..)
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( EvaluateTransactionError (..)
    , NodeTipTooOldError (..)
    , TransactionScriptFailureInAnyEra (..)
    , pickScriptFailure
    )
import Prettyprinter
    ( pretty
    )

import qualified Cardano.Ledger.Api as Ledger
import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ogmios.Data.Json.Shelley as Shelley

-- NOTE: Transaction submission / evaluation error code range is 3000-4000;
--
-- - We reserve the first 3000, 3001 .. 3009 for protocol errors;
-- - 3010 is reserved for the top-level script failure that aggregate multiple script failures
-- - 3100+ is for transaction submission errors.
--
-- Which leaves us the range 3011 .. 3099 for script evaluation failures.
scriptFailureCode :: Int -> Rpc.FaultCode
scriptFailureCode  = Rpc.FaultCustom . (+ 3011)

encodeScriptFailure
    :: Rpc.EmbedFault
    -> TransactionScriptFailureInAnyEra
    -> Json
encodeScriptFailure reject = \case
    TransactionScriptFailureInAnyEra (era, Ledger.MissingScript ptr _) ->
        reject (scriptFailureCode 0)
            "An associated script witness is missing. Indeed, any script used in a transaction \
            \(when spending, minting, withdrawing or publishing certificates) must be provided \
            \in full with the transaction. Scripts must therefore be added either to the \
            \witness set or provided as a reference inputs should you use Plutus V2+ and \
            \a format from Babbage and beyond."
            (pure $ encodeObject
                ( "missingScripts" .= encodeFoldable (encodeScriptPurposeIndexInAnyEra . ScriptPurposeIndexInAnyEra . (era,)) [ptr]
                )
            )

    TransactionScriptFailureInAnyEra (_, Ledger.ValidationFailure _exunits err traces _debugLang) ->
        reject (scriptFailureCode 1)
            "Some of the scripts failed to evaluate to a positive outcome. The field \
            \'data.validationError' informs about the nature of the error, and 'data.traces' \
            \lists all the execution traces collected during the script execution."
            (pure $ encodeObject
                ( "validationError" .= encodeText (show (pretty err))
               <> "traces" .= encodeFoldable encodeText traces
                )
            )

    TransactionScriptFailureInAnyEra (_, Ledger.InvalidTxIn i) ->
        reject (scriptFailureCode 2)
            "A redeemer points to an input that isn't locked by a Plutus script. Double-check your \
            \redeemer pointers and note that, inputs are ordered lexicographically by the ledger \
            \(using their transaction id and output index). This order may differ from the one you \
            \originally defined in your transaction and may be the cause of misalignment of your \
            \redeemer pointers. The field 'data.unsuitableOutputReference' indicates which input \
            \was wrongly targeted by a redeemer."
            (pure $ encodeObject
                ( "unsuitableOutputReference" .=
                    encodeObject (Shelley.encodeTxIn i)
                )
            )

    TransactionScriptFailureInAnyEra (era, Ledger.RedeemerPointsToUnknownScriptHash ptr) ->
        encodePredicateFailure reject (ExtraneousRedeemers [ScriptPurposeIndexInAnyEra (era, ptr)])

    TransactionScriptFailureInAnyEra (_, Ledger.MissingDatum datumHash) ->
        encodePredicateFailure reject (MissingDatums (Set.singleton datumHash))

    TransactionScriptFailureInAnyEra (_, Ledger.UnknownTxIn i) ->
        encodePredicateFailure reject (UnknownUtxoReference (Set.singleton i))

    TransactionScriptFailureInAnyEra (_, Ledger.IncompatibleBudget budget) ->
        encodePredicateFailure reject (ExecutionBudgetOutOfBounds budget)

    TransactionScriptFailureInAnyEra (_, Ledger.NoCostModelInLedgerState lang) ->
        encodePredicateFailure reject (MissingCostModels [lang])

    TransactionScriptFailureInAnyEra (era, Ledger.ContextError err) ->
        let errInAnyEra = ContextErrorInAnyEra (era, err)
         in encodeEvaluationError reject (CannotCreateEvaluationContext errInAnyEra)

encodeEvaluationError
    :: Rpc.EmbedFault
    -> EvaluateTransactionError
    -> Json
encodeEvaluationError reject = \case
    IncompatibleEra era ->
        reject (Rpc.FaultCustom 3000)
            "Trying to evaluate a transaction from an old era (prior to Alonzo)."
            (pure $ encodeObject
                ( "incompatibleEra" .=
                    encodeEraName era
                )
            )

    UnsupportedEra era ->
        reject (Rpc.FaultCustom 3001)
            "Trying to evaluate a transaction from an era that's no longer supported \
            \(e.g. Alonzo). Please use a more recent transaction format."
            (pure $ encodeObject
                ( "unsupportedEra" .=
                    encodeEraName era
                )
            )

    OverlappingAdditionalUtxo inputs ->
        reject (Rpc.FaultCustom 3002)
            "Some user-provided additional UTxO entries overlap with those that exist \
            \in the ledger."
            (pure $ encodeObject
                ( "overlappingOutputReferences" .=
                    encodeFoldable (encodeObject . encodeTxIn) inputs
                )
            )

    CannotCreateEvaluationContext err ->
        reject (Rpc.FaultCustom 3004)
            "Unable to create the evaluation context from the given transaction."
            (pure $ encodeObject
                ( "reason" .=
                    encodeContextErrorInAnyEra err
                )
            )

    NodeTipTooOldErr err ->
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

    ScriptExecutionFailures failures ->
        reject (Rpc.FaultCustom 3010)
            "Some scripts of the transactions terminated with error(s)."
            (pure $ encodeList identity $ Map.foldrWithKey
                (\ix e xs ->
                    if null e then
                        xs
                    else
                        let embed code msg details = encodeObject
                                ( "validator" .= encodeScriptPurposeIndexInAnyEra ix
                               <> "error" .= encodeObject
                                    ( "code" .= Json.toEncoding code
                                   <> "message" .= Json.toEncoding msg
                                   <> maybe mempty (Json.pair "data") details
                                    )
                                )
                            x = encodeScriptFailure embed (pickScriptFailure e)

                         in x : xs
                ) [] failures
            )
