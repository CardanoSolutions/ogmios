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

import qualified Cardano.Ledger.Alonzo.Rules as Al
import qualified Cardano.Ledger.Babbage.Rules as Ba
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Cardano.Ledger.Shelley.Rules as Sh

encodeLedgerFailure
    :: forall era crypto.
        ( Crypto crypto
        , era ~ BabbageEra crypto
        )
    => Sh.ShelleyLedgerPredFailure era
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure AlonzoBasedEraBabbage e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era crypto.
        ( Era (era crypto)
        , EraCrypto (era crypto) ~ crypto
        , PredicateFailure (EraRule "UTXOS" (era crypto)) ~ Cn.ConwayUtxosPredFailure (era crypto)
        , PredicateFailure (EraRule "UTXO" (era crypto)) ~ Ba.BabbageUtxoPredFailure (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> Ba.BabbageUtxowPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxowFailure era = \case
    Ba.MalformedReferenceScripts scripts ->
        MalformedScripts scripts
    Ba.MalformedScriptWitnesses scripts ->
        MalformedScripts scripts
    Ba.AlonzoInBabbageUtxowPredFailure e ->
        Alonzo.encodeUtxowFailure era (encodeUtxoFailure era) e
    Ba.UtxoFailure e ->
        encodeUtxoFailure era e

encodeUtxoFailure
    :: forall era crypto.
        ( Era (era crypto)
        , EraCrypto (era crypto) ~ crypto
        , PredicateFailure (EraRule "UTXOS" (era crypto)) ~ Cn.ConwayUtxosPredFailure (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> Ba.BabbageUtxoPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxoFailure era = \case
    Ba.AlonzoInBabbageUtxoPredFailure e ->
        Alonzo.encodeUtxoFailure era e
    Ba.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Ba.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out,minAda) ->
                    ( TxOutInAnyEra (toShelleyBasedEra era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Ba.BabbageNonDisjointRefInputs ins ->
        NonDisjointRefInputs ins
