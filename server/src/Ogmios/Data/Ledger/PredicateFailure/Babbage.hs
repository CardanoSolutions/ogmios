--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
encodeUtxowFailure era encodeUtxosFailure = \case
    Ba.MalformedReferenceScripts scripts ->
        MalformedScripts scripts
    Ba.MalformedScriptWitnesses scripts ->
        MalformedScripts scripts
    Ba.AlonzoInBabbageUtxowPredFailure e ->
        Alonzo.encodeUtxowFailure era (encodeUtxoFailure era encodeUtxosFailure) e
    Ba.UtxoFailure e ->
        encodeUtxoFailure era encodeUtxosFailure e

encodeUtxoFailure
    :: forall era. (Era era)
    => AlonzoBasedEra era
    -> (PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure)
    -> Ba.BabbageUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure era encodeUtxosFailure = \case
    Ba.AlonzoInBabbageUtxoPredFailure e ->
        Alonzo.encodeUtxoFailure era encodeUtxosFailure e
    Ba.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Ba.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Ba.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out,minAda) ->
                    ( TxOutInAnyEra (toShelleyBasedEra era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
