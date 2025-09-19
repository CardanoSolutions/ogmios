--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
encodeUtxowFailure era encodeUtxoFailureInEra = \case
    Al.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers }
    Al.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums }
    Al.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums }
    Al.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Al.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Al.MissingRequiredSigners keys ->
        MissingSignatures keys
    Al.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs }
    Al.ShelleyInAlonzoUtxowPredFailure e ->
        Shelley.encodeUtxowFailure encodeUtxoFailureInEra e

encodeUtxoFailure
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> (Sh.PredicateFailure (EraRule "UTXOS" era) -> MultiEraPredicateFailure)
    -> Al.AlonzoUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure era encodeUtxosFailure' = \case
    Al.BadInputsUTxO inputs ->
        UnknownUtxoReference inputs
    Al.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Al.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (toShelleyBasedEra era, out)) <$> outs in
        ValueSizeAboveLimit culpritOutputs
    Al.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize, maximumSize }
    Al.InputSetEmptyUTxO ->
        EmptyInputSet
    Al.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee)  ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Al.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (toShelleyBasedEra era, consumed) in
        let valueProduced = ValueInAnyEra (toShelleyBasedEra era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Al.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses invalidAddrs in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts invalidAccts in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (toShelleyBasedEra era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Al.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (toShelleyBasedEra era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Al.TriesToForgeADA ->
        MintingOrBurningAda
    Al.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Al.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys $ unUTxO utxo)
    Al.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Al.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (toShelleyBasedEra era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Al.NoCollateralInputs{} ->
        MissingCollateralInputs
    Al.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs { maximumCollateralInputs, countedCollateralInputs }
    Al.ExUnitsTooBigUTxO (Mismatch providedExUnits maximumExUnits) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Al.UtxosFailure e ->
        encodeUtxosFailure' e

encodeUtxosFailure
    :: forall era.
        ( Era era
        )
    => AlonzoBasedEra era
    -> Al.AlonzoUtxosPredFailure era
    -> MultiEraPredicateFailure
encodeUtxosFailure era = \case
    Al.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Al.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors era errors)
    Al.UpdateFailure{} ->
        InvalidProtocolParametersUpdate

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
