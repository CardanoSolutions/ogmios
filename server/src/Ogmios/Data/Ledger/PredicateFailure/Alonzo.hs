--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Alonzo where

import Ogmios.Prelude

import Cardano.Ledger.Core
    ( EraRule
    )
import Cardano.Ledger.UTxO
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

import qualified Data.Map as Map

import qualified Ogmios.Data.Ledger.PredicateFailure.Shelley as Shelley

import Cardano.Ledger.Alonzo.Plutus.Evaluate
    ( CollectError (..)
    )
import qualified Cardano.Ledger.Alonzo.Rules as Al
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Cardano.Ledger.Shelley.Rules as Sh

encodeLedgerFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Sh.ShelleyLedgerPredFailure (AlonzoEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure
            AlonzoBasedEraAlonzo
            (encodeUtxoFailure AlonzoBasedEraAlonzo)
            e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era crypto.
        ( crypto ~ EraCrypto (era crypto)
        , Era (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> (Sh.PredicateFailure (EraRule "UTXO" (era crypto)) -> MultiEraPredicateFailure crypto)
    -> Al.AlonzoUtxowPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
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
    Al.PPViewHashesDontMatch providedIntegrityHash computedIntegrityHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Al.MissingRequiredSigners keys ->
        MissingSignatures keys
    Al.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs }
    Al.ShelleyInAlonzoUtxowPredFailure e ->
        Shelley.encodeUtxowFailure encodeUtxoFailureInEra e

encodeUtxoFailure
    :: forall era crypto.
        ( Era (era crypto)
        , EraCrypto (era crypto) ~ crypto
        , Sh.PredicateFailure (EraRule "UTXOS" (era crypto)) ~ Cn.ConwayUtxosPredFailure (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> Cn.ConwayUtxoPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxoFailure era = \case
    Al.BadInputsUTxO inputs ->
        UnknownUtxoReference inputs
    Al.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Al.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (toShelleyBasedEra era, out)) <$> outs in
        ValueSizeAboveLimit culpritOutputs
    Al.MaxTxSizeUTxO measuredSize maximumSize ->
        TransactionTooLarge { measuredSize, maximumSize }
    Al.InputSetEmptyUTxO ->
        EmptyInputSet
    Al.FeeTooSmallUTxO minimumRequiredFee suppliedFee ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Al.ValueNotConservedUTxO consumed produced ->
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
    Al.WrongNetworkInTxBody expectedNetwork _providedNetwork ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Al.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (toShelleyBasedEra era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Al.NoCollateralInputs{} ->
        MissingCollateralInputs
    Al.TooManyCollateralInputs maximumCollateralInputs countedCollateralInputs ->
        TooManyCollateralInputs { maximumCollateralInputs, countedCollateralInputs }
    Al.ExUnitsTooBigUTxO maximumExUnits providedExUnits ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Al.UtxosFailure e ->
        encodeUtxosFailure era e

encodeUtxosFailure
    :: forall era crypto.
        ( crypto ~ EraCrypto (era crypto)
        , Era (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> Al.AlonzoUtxosPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxosFailure era = \case
    Al.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Al.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors era errors)
    Al.UpdateFailure{} ->
        InvalidProtocolParametersUpdate

encodeCollectErrors
    :: forall era crypto.
        ( crypto ~ EraCrypto (era crypto)
        , Era (era crypto)
        )
    => AlonzoBasedEra (era crypto)
    -> [CollectError (era crypto)]
    -> [MultiEraPredicateFailure crypto]
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
        missingRedeemersErrs ++ missingScriptsErrs ++ missingCostModelsErrs ++ badTranslationErrs
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
