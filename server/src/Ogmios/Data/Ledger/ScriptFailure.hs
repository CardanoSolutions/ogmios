--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Ledger.ScriptFailure where

import Ogmios.Prelude

import qualified Cardano.Ledger.Api as Ledger
import qualified Text.Show

data SomeTransactionScriptFailure crypto =
    forall era. (EraCrypto (era crypto) ~ crypto)
    => SomeTransactionScriptFailure (Ledger.TransactionScriptFailure (era crypto))

instance Show (SomeTransactionScriptFailure crypto) where
    show _ = "SomeTransactionScriptFailure"

-- | Return the most relevant script failure from a list of errors.
--
-- See also 'Ogmios.Data.Ledger.PredicateFailure#pickPredicateFailure' for
-- details.
pickScriptFailure
    :: HasCallStack
    => [SomeTransactionScriptFailure crypto]
    -> SomeTransactionScriptFailure crypto
pickScriptFailure =
    head
    . fromMaybe (error "Empty list of script failures from the ledger!?")
    . nonEmpty
    . sortOn scriptFailurePriority

scriptFailurePriority
    :: SomeTransactionScriptFailure crypto
    -> Word
scriptFailurePriority = \case
    SomeTransactionScriptFailure Ledger.UnknownTxIn{} -> 0
    SomeTransactionScriptFailure Ledger.MissingScript{} -> 0

    SomeTransactionScriptFailure Ledger.RedeemerPointsToUnknownScriptHash{} -> 1
    SomeTransactionScriptFailure Ledger.NoCostModelInLedgerState{} -> 1

    SomeTransactionScriptFailure Ledger.InvalidTxIn{} -> 2

    SomeTransactionScriptFailure Ledger.MissingDatum{} -> 3

    SomeTransactionScriptFailure Ledger.RedeemerNotNeeded{} -> 4

    SomeTransactionScriptFailure Ledger.ValidationFailure{} -> 5

    SomeTransactionScriptFailure Ledger.IncompatibleBudget{} -> 999
