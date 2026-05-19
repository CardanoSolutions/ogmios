--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings suppressed pending NonEmpty/NonEmptySet shape fixes for cardano-ledger-babbage 1.13.0.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-top-binds -Wno-redundant-constraints #-}

module Ogmios.Data.Ledger.PredicateFailure.Babbage where

import Ogmios.Prelude

import Cardano.Ledger.Core
    ( EraRule
    )
import Control.State.Transition
    ( STS (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    , TxOutInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodeDelegsFailure
    )

import qualified Ogmios.Data.Ledger.PredicateFailure.Alonzo as Alonzo

import qualified Cardano.Ledger.Babbage.Rules as Ba
import qualified Cardano.Ledger.Shelley.Rules as Sh

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure BabbageEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure AlonzoBasedEraBabbage (Alonzo.encodeUtxosFailure AlonzoBasedEraBabbage) e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era.
        ( Era era
        , PredicateFailure (EraRule "UTXO" era) ~ Ba.BabbageUtxoPredFailure era
        )
    => AlonzoBasedEra era
    -> (PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure)
    -> Ba.BabbageUtxowPredFailure era
    -> MultiEraPredicateFailure
encodeUtxowFailure _ _ _ =
    -- TODO(dijkstra): NonEmptySet scripts in cardano-ledger-babbage 1.13.0.
    error "TODO(dijkstra): encodeUtxowFailure Babbage"

encodeUtxoFailure
    :: forall era. (Era era)
    => AlonzoBasedEra era
    -> (PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure)
    -> Ba.BabbageUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure _ _ _ =
    -- TODO(dijkstra): NonEmpty insufficientlyFundedOutputs etc.
    error "TODO(dijkstra): encodeUtxoFailure Babbage"
