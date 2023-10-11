--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Ledger.ScriptFailure where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Json.Ledger.PredicateFailure
    ( encodePredicateFailure
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( SomeTransactionScriptFailure (..)
    )
import Prettyprinter
    ( pretty
    )

import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Ledger.Api as Ledger
import qualified Codec.Json.Rpc as Rpc
import qualified Data.Set as Set

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
    :: forall crypto.
        ( Crypto crypto
        )
    => Rpc.EmbedFault
    -> SomeTransactionScriptFailure crypto
    -> Json
encodeScriptFailure reject = \case
    SomeTransactionScriptFailure (Ledger.MissingScript ptr _) ->
        reject (scriptFailureCode 0)
            "An associated script witness is missing. Indeed, any script used in a transaction \
            \(when spending, minting, withdrawing or publishing certificates) must be provided \
            \in full with the transaction. Scripts must therefore be added either to the \
            \witness set or provided as a reference inputs should you use Plutus V2+ and \
            \a format from Babbage and beyond."
            (pure $ encodeObject
                ( "missingScripts" .= encodeFoldable Alonzo.encodeRdmrPtr [ptr]
                )
            )

    SomeTransactionScriptFailure (Ledger.ValidationFailure (Ledger.ValidationFailedV1 err traces _debugLang)) ->
        reject (scriptFailureCode 1)
            "Some of the (V1) scripts failed to evaluate to a positive outcome. The field \
            \'data.validationError' informs about the nature of the error, and 'data.traces' \
            \lists all the execution traces collected during the script execution."
            (pure $ encodeObject
                ( "validationError" .= encodeText (show (pretty err))
               <> "traces" .= encodeFoldable encodeText traces
                )
            )

    SomeTransactionScriptFailure (Ledger.ValidationFailure (Ledger.ValidationFailedV2 err traces _debugLang)) ->
        reject (scriptFailureCode 1)
            "Some of the (V2) scripts failed to evaluate to a positive outcome. The field \
            \'data.validationError' informs about the nature of the error, and 'data.traces' \
            \lists all the execution traces collected during the script execution."
            (pure $ encodeObject
                ( "validationError" .= encodeText (show (pretty err))
               <> "traces" .= encodeFoldable encodeText traces
                )
            )

    SomeTransactionScriptFailure (Ledger.ValidationFailure (Ledger.ValidationFailedV3 err traces _debugLang)) ->
        reject (scriptFailureCode 1)
            "Some of the (V3) scripts failed to evaluate to a positive outcome. The field \
            \'data.validationError' informs about the nature of the error, and 'data.traces' \
            \lists all the execution traces collected during the script execution."
            (pure $ encodeObject
                ( "validationError" .= encodeText (show (pretty err))
               <> "traces" .= encodeFoldable encodeText traces
                )
            )

    SomeTransactionScriptFailure (Ledger.InvalidTxIn i) ->
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

    SomeTransactionScriptFailure (Ledger.RedeemerNotNeeded ptr _) ->
        encodePredicateFailure @crypto reject (ExtraneousRedeemers [ptr])

    SomeTransactionScriptFailure (Ledger.RedeemerPointsToUnknownScriptHash ptr) ->
        encodePredicateFailure @crypto reject (ExtraneousRedeemers [ptr])

    SomeTransactionScriptFailure (Ledger.MissingDatum datumHash) ->
        encodePredicateFailure @crypto reject (MissingDatums (Set.singleton datumHash))

    SomeTransactionScriptFailure (Ledger.UnknownTxIn i) ->
        encodePredicateFailure @crypto reject (UnknownUtxoReference (Set.singleton i))

    SomeTransactionScriptFailure Ledger.IncompatibleBudget{} ->
        encodePredicateFailure @crypto reject InternalLedgerTypeConversionError

    SomeTransactionScriptFailure (Ledger.NoCostModelInLedgerState lang) ->
        encodePredicateFailure @crypto reject (MissingCostModels [lang])
