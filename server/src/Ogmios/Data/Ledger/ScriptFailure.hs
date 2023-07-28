
--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Ledger.ScriptFailure where

import Ogmios.Prelude

import qualified Cardano.Ledger.Api as Ledger

-- | Return the most relevant script failure from a list of errors.
--
-- See also 'Ogmios.Data.Ledger.PredicateFailure#pickPredicateFailure' for
-- details.
pickScriptFailure
    :: HasCallStack
    => [Ledger.TransactionScriptFailure crypto]
    -> Ledger.TransactionScriptFailure crypto
pickScriptFailure =
    head
    . fromMaybe (error "Empty list of script failures from the ledger!?")
    . nonEmpty
    . sortOn scriptFailurePriority

scriptFailurePriority
    :: Ledger.TransactionScriptFailure crypto
    -> Word
scriptFailurePriority = \case
    Ledger.UnknownTxIn{} -> 0
    Ledger.MissingScript{} -> 0

    Ledger.RedeemerPointsToUnknownScriptHash{} -> 1
    Ledger.NoCostModelInLedgerState{} -> 1

    Ledger.InvalidTxIn{} -> 2

    Ledger.MissingDatum{} -> 3

    Ledger.RedeemerNotNeeded{} -> 4

    Ledger.ValidationFailure{} -> 5

    Ledger.IncompatibleBudget{} -> 999
