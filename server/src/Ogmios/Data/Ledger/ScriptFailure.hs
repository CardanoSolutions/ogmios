--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Ledger.ScriptFailure where

import Ogmios.Prelude

import Cardano.Ledger.TxIn
    ( TxIn
    )
import Ogmios.Data.Ledger
    ( ContextErrorInAnyEra
    , ScriptPurposeIndexInAnyEra
    )

import qualified Cardano.Ledger.Api as Ledger
import qualified Text.Show


data EvaluateTransactionError
    = ScriptExecutionFailures (Map ScriptPurposeIndexInAnyEra [TransactionScriptFailureInAnyEra])
    | IncompatibleEra Text
    | UnsupportedEra Text
    | OverlappingAdditionalUtxo (Set TxIn)
    | NodeTipTooOldErr NodeTipTooOldError
    | CannotCreateEvaluationContext ContextErrorInAnyEra

data NodeTipTooOldError = NodeTipTooOld
    { currentNodeEra :: Text
    , minimumRequiredEra :: Text
    }
    deriving (Show)

deriving instance Show EvaluateTransactionError

data TransactionScriptFailureInAnyEra =
    forall era. (Era era)
    => TransactionScriptFailureInAnyEra
        ( AlonzoBasedEra era
        , Ledger.TransactionScriptFailure era
        )

instance  Show TransactionScriptFailureInAnyEra where
    show = \case
        TransactionScriptFailureInAnyEra (AlonzoBasedEraAlonzo, e) -> show e
        TransactionScriptFailureInAnyEra (AlonzoBasedEraBabbage, e) -> show e
        TransactionScriptFailureInAnyEra (AlonzoBasedEraConway, e) -> show e

-- | Return the most relevant script failure from a list of errors.
--
-- See also 'Ogmios.Data.Ledger.PredicateFailure#pickPredicateFailure' for
-- details.
pickScriptFailure
    :: HasCallStack
    => [TransactionScriptFailureInAnyEra]
    -> TransactionScriptFailureInAnyEra
pickScriptFailure =
    head
    . fromMaybe (error "Empty list of script failures from the ledger!?")
    . nonEmpty
    . sortOn scriptFailurePriority

scriptFailurePriority
    :: TransactionScriptFailureInAnyEra
    -> Word
scriptFailurePriority = \case
    TransactionScriptFailureInAnyEra (_, Ledger.UnknownTxIn{}) -> 0
    TransactionScriptFailureInAnyEra (_, Ledger.MissingScript{}) -> 0

    TransactionScriptFailureInAnyEra (_, Ledger.RedeemerPointsToUnknownScriptHash{}) -> 1
    TransactionScriptFailureInAnyEra (_, Ledger.NoCostModelInLedgerState{}) -> 1

    TransactionScriptFailureInAnyEra (_, Ledger.InvalidTxIn{}) -> 2

    TransactionScriptFailureInAnyEra (_, Ledger.MissingDatum{}) -> 3

    TransactionScriptFailureInAnyEra (_, Ledger.ContextError{}) -> 4

    TransactionScriptFailureInAnyEra (_, Ledger.ValidationFailure{}) -> 5

    TransactionScriptFailureInAnyEra (_, Ledger.IncompatibleBudget{}) -> 999
