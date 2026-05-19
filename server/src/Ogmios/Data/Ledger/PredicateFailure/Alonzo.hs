--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings disabled while removed constructors are stubbed.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-top-binds -Wno-redundant-constraints #-}

module Ogmios.Data.Ledger.PredicateFailure.Alonzo where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.Plutus.Evaluate
    ( CollectError (..)
    )
import Cardano.Ledger.Core
    ( EraRule
    )
import Cardano.Ledger.State
    ( UTxO (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( ContextErrorInAnyEra (..)
    , DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , ScriptPurposeIndexInAnyEra (..)
    , ScriptPurposeItemInAnyEra (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    , pickPredicateFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodeDelegsFailure
    )
import Relude.Unsafe
    ( fromJust
    )

import qualified Cardano.Ledger.Alonzo.Rules as Al
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    )
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map as Map
import qualified Ogmios.Data.Ledger.PredicateFailure.Shelley as Shelley

encodeLedgerFailure
    :: forall. ()
    => Sh.ShelleyLedgerPredFailure AlonzoEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure
            AlonzoBasedEraAlonzo
            (let era = AlonzoBasedEraAlonzo in encodeUtxoFailure era (encodeUtxosFailure era))
            e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> (Sh.PredicateFailure (EraRule "UTXO" era) -> MultiEraPredicateFailure)
    -> Al.AlonzoUtxowPredFailure era
    -> MultiEraPredicateFailure
encodeUtxowFailure _ _ _ =
    -- TODO(dijkstra): NonEmpty/NonEmptySet container types in cardano-ledger-alonzo 1.15.0 + Mismatch shape changes.
    error "TODO(dijkstra): encodeUtxowFailure Alonzo"

encodeUtxoFailure
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> (Sh.PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure)
    -> Al.AlonzoUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure _ _ _ =
    -- TODO(dijkstra): same shape issues as encodeUtxowFailure.
    error "TODO(dijkstra): encodeUtxoFailure Alonzo"

encodeUtxosFailure
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> Al.AlonzoUtxosPredFailure era
    -> MultiEraPredicateFailure
encodeUtxosFailure _era _ =
    -- TODO(dijkstra): CollectErrors now wraps NonEmpty (CollectError era), need toList.
    error "TODO(dijkstra): encodeUtxosFailure Alonzo"

encodeCollectErrors
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> [CollectError era]
    -> NonEmpty MultiEraPredicateFailure
encodeCollectErrors era errors =
    let missingRedeemersErrs
            | not (null missingRedeemers) =
                [ MissingRedeemers { missingRedeemers = ScriptPurposeItemInAnyEra . (era,) <$> missingRedeemers } ]
            | otherwise =
                []
     in

    let missingScriptsErrs
            | not (null missingScripts) =
                [ MissingScriptWitnesses { missingScripts } ]
            | otherwise =
                []
     in

    let missingCostModelsErrs
            | not (null missingCostModels) =
                [ MissingCostModels { missingCostModels } ]
            | otherwise =
                []
     in

    let badTranslationErrs = flip mapMaybe errors $ \case
            -- NOTE: Keep those pattern-match explicit for exhaustiveness check.
            BadTranslation err ->
                Just (UnableToCreateScriptContext (ContextErrorInAnyEra (era, err)))
            NoWitness{} ->
                Nothing
            NoCostModel{} ->
                Nothing
            NoRedeemer{} ->
                Nothing

     in
        nonEmpty (missingRedeemersErrs ++ missingScriptsErrs ++ missingCostModelsErrs ++ badTranslationErrs) & fromJust
  where
    missingRedeemers = mapMaybe
        (\case
            NoRedeemer purpose -> Just purpose
            _ -> Nothing
        ) errors

    missingScripts = fromList $ mapMaybe
        (\case
            NoWitness scriptHash -> Just scriptHash
            _ -> Nothing
        ) errors

    missingCostModels = mapMaybe
        (\case
            NoCostModel language -> Just language
            _ -> Nothing
        ) errors
