--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Dijkstra where

import Ogmios.Prelude

import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    )
import Cardano.Ledger.Keys
    ( HasKeyRole (coerceKeyRole)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , ScriptPurposeIndexInAnyEra (..)
    , ScriptPurposeItemInAnyEra (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    , pickPredicateFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Alonzo
    ( encodeCollectErrors
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodePoolFailure
    )

import Cardano.Ledger.Address
    ()

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Cardano.Ledger.Dijkstra.Rules as Dn

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

----------
-- MEMPOOL
----------

encodeMempoolFailure
    :: Dn.DijkstraMempoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeMempoolFailure = \case
    Dn.LedgerFailure e ->
        encodeLedgerFailure e
    Dn.MempoolFailure mempoolError ->
        UnexpectedMempoolError { mempoolError }
    Dn.AllInputsAreSpent ->
        UnexpectedMempoolError
            { mempoolError = "All inputs are spent. Transaction has probably already been included" }

----------
-- LEDGER
----------

encodeLedgerFailure
    :: Dn.DijkstraLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Dn.DijkstraUtxowFailure e ->
        encodeUtxowFailure e
    Dn.DijkstraCertsFailure e ->
        encodeCertsFailure e
    Dn.DijkstraGovFailure e ->
        encodeGovFailure e
    Dn.DijkstraWdrlNotDelegatedToDRep ((Set.fromList . toList) -> marginalizedCredentials) ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Dn.DijkstraIncompleteWithdrawals _withdrawals ->
        IncompleteWithdrawals mempty
    Dn.DijkstraWithdrawalsMissingAccounts _withdrawals ->
        IncompleteWithdrawals mempty
    Dn.DijkstraTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Dn.DijkstraTxRefScriptsSizeTooBig (Mismatch (toInteger -> measuredSize) (toInteger -> maximumSize)) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }
    Dn.DijkstraSubLedgersFailure e ->
        encodeSubLedgersFailure e
    Dn.DijkstraSpendingOutputFromSameTx _txIds ->
        SpendingOutputFromSubTransaction

--------
-- GOV
--------

encodeGovFailure
    :: Dn.DijkstraGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovFailure = \case
    Dn.GovActionsDoNotExist (toList -> fromList -> governanceActions) ->
        UnknownGovernanceActions { governanceActions }
    Dn.MalformedProposal _govAction ->
        InvalidProtocolParametersUpdate
    Dn.ProposalProcedureNetworkIdMismatch rewardAccount expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (Set.singleton rewardAccount)
            }
    Dn.TreasuryWithdrawalsNetworkIdMismatch rewardAccounts expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (NESet.toSet rewardAccounts)
            }
    Dn.ProposalDepositIncorrect (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Dn.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Dn.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers = NESet.toSet conflictingMembers
            }
    Dn.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet (NEMap.toMap members)
            }
    Dn.InvalidPrevGovActionId proposal ->
        InvalidPreviousGovernanceAction $
            case Ledger.pProcGovAction proposal of
                Ledger.ParameterChange actionId _ _guardrail ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.PParamUpdatePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.HardForkInitiation actionId _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.HardForkPurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.UpdateCommittee actionId _ _ _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.CommitteePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.NoConfidence actionId ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.CommitteePurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.NewConstitution actionId _ ->
                    [ ( Ledger.pProcAnchor proposal
                      , Ledger.ConstitutionPurpose
                      , Ledger.unGovPurposeId <$> actionId
                      )
                    ]
                Ledger.TreasuryWithdrawals _withdrawals _guardrail ->
                    []
                Ledger.InfoAction ->
                    []
    Dn.VotingOnExpiredGovAction (toList -> voters) ->
        VotingOnExpiredActions voters
    Dn.ProposalCantFollow _ (Mismatch proposedVersion currentVersion) ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Dn.InvalidGuardrailsScriptHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Dn.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Dn.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Dn.VotersDoNotExist voters ->
        UnknownVoters (toList voters)
    Dn.ZeroTreasuryWithdrawals _ ->
        EmptyTreasuryWithdrawal
    Dn.ProposalReturnAccountDoesNotExist (Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Dn.TreasuryWithdrawalReturnAccountsDoNotExist (NE.head -> Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Dn.UnelectedCommitteeVoters _voters ->
        UnauthorizedVotes []

---------
-- CERTS
---------

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeCertsFailure = \case
    Cn.WithdrawalsNotInRewardsCERTS _credentials ->
        IncompleteWithdrawals mempty
    Cn.CertFailure e ->
        encodeCertFailure e

encodeCertFailure
    :: Cn.ConwayCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeCertFailure = \case
    Cn.DelegFailure e ->
        encodeDelegFailure e
    Cn.PoolFailure e ->
        encodePoolFailure e
    Cn.GovCertFailure e ->
        encodeGovCertFailure e

encodeDelegFailure
    :: Cn.ConwayDelegPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
    Cn.IncorrectDepositDELEG providedDeposit ->
        DepositMismatch { providedDeposit, expectedDeposit = SNothing }
    Cn.StakeKeyRegisteredDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.StakeKeyNotRegisteredDELEG unknownCredential ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.StakeKeyHasNonZeroAccountBalanceDELEG rewardAccountBalance ->
        RewardAccountNotEmpty { rewardAccountBalance }
    Cn.DepositIncorrectDELEG _deposit ->
        InvalidProtocolParametersUpdate
    Cn.RefundIncorrectDELEG _refund ->
        InvalidProtocolParametersUpdate
    Cn.DelegateeDRepNotRegisteredDELEG (coerceKeyRole -> unknownCredential) ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.DelegateeStakePoolNotRegisteredDELEG poolId ->
        UnknownStakePool poolId

encodeGovCertFailure
    :: Dn.DijkstraGovCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovCertFailure = \case
    Dn.DijkstraDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Dn.DijkstraDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    Dn.DijkstraDRepIncorrectDeposit (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Dn.DijkstraCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Dn.DijkstraDRepIncorrectRefund (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Dn.DijkstraCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

---------
-- UTxOW
---------

encodeUtxowFailure
    :: Dn.DijkstraUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxowFailure = \case
    Dn.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _computedBodyHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dn.UtxoFailure e ->
        encodeUtxoFailure e
    Dn.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers = toList missingRedeemers }
    Dn.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Dn.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Dn.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers = toList extraneousRedeemers }
    Dn.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dn.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Dn.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Dn.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Dn.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Dn.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Dn.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Dn.MissingTxMetadata hash ->
        MissingMetadata hash
    Dn.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Dn.InvalidMetadata ->
        InvalidMetadata
    Dn.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Dn.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dn.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
  where
    era = AlonzoBasedEraDijkstra

--------
-- UTxO
--------

encodeUtxoFailure
    :: Dn.DijkstraUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Dn.UtxosFailure e ->
        encodeUtxosFailure e
    Dn.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Dn.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Dn.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Dn.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Dn.InputSetEmptyUTxO ->
        EmptyInputSet
    Dn.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Dn.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Dn.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Dn.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Dn.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Dn.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys $ NEMap.toMap utxo)
    Dn.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Dn.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Dn.NoCollateralInputs{} ->
        MissingCollateralInputs
    Dn.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs { maximumCollateralInputs = fromIntegral maximumCollateralInputs, countedCollateralInputs = fromIntegral countedCollateralInputs }
    Dn.ExUnitsTooBigUTxO (Mismatch providedExUnits maximumExUnits) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Dn.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Dn.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Dn.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Dn.PtrPresentInCollateralReturn out ->
        PointerAddressInCollateralReturn { pointerAddressOutput = TxOutInAnyEra (era, out) }
  where
    era = ShelleyBasedEraDijkstra

---------
-- UTxOS
---------

encodeUtxosFailure
    :: Cn.ConwayUtxosPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxosFailure = \case
    Cn.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Cn.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors AlonzoBasedEraDijkstra (toList errors))

--------------
-- SUBLEDGERS
--------------

encodeSubLedgersFailure
    :: Dn.DijkstraSubLedgersPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgersFailure (Dn.SubLedgerFailure e) =
    encodeSubLedgerFailure e

encodeSubLedgerFailure
    :: Dn.DijkstraSubLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgerFailure = \case
    Dn.SubUtxowFailure e ->
        encodeSubUtxowFailure e
    Dn.SubCertsFailure e ->
        encodeSubCertsFailure e
    Dn.SubGovFailure e ->
        encodeSubGovFailure e
    Dn.SubWdrlNotDelegatedToDRep ((Set.fromList . toList) -> marginalizedCredentials) ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Dn.SubTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }

encodeSubGovFailure
    :: Dn.DijkstraSubGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubGovFailure (Dn.DijkstraSubGovPredFailure f) =
    encodeGovFailure f

encodeSubCertsFailure
    :: Dn.DijkstraSubCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertsFailure (Dn.SubCertFailure f) =
    encodeSubCertFailure f

encodeSubCertFailure
    :: Dn.DijkstraSubCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertFailure = \case
    Dn.SubDelegFailure (Dn.DijkstraSubDelegPredFailure f) ->
        encodeDelegFailure f
    Dn.SubPoolFailure (Dn.DijkstraSubPoolPredFailure f) ->
        encodePoolFailure f
    Dn.SubGovCertFailure (Dn.DijkstraSubGovCertPredFailure f) ->
        encodeGovCertFailure f

-----------
-- SUBUTXOW
-----------

encodeSubUtxowFailure
    :: Dn.DijkstraSubUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxowFailure = \case
    Dn.SubUtxoFailure e ->
        encodeSubUtxoFailure e
    Dn.SubInvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Dn.SubMissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Dn.SubMissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Dn.SubScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Dn.SubMissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Dn.SubMissingTxMetadata hash ->
        MissingMetadata hash
    Dn.SubConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Dn.SubInvalidMetadata ->
        InvalidMetadata
    Dn.SubExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Dn.SubMissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers = toList missingRedeemers }
    Dn.SubMissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Dn.SubNotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Dn.SubPPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Dn.SubUnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Dn.SubExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers = toList extraneousRedeemers }
    Dn.SubMalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dn.SubMalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Dn.SubScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _computedBodyHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
  where
    era = AlonzoBasedEraDijkstra

----------
-- SUBUTXO
----------

encodeSubUtxoFailure
    :: Dn.DijkstraSubUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxoFailure = \case
    Dn.SubBadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Dn.SubOutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Dn.SubMaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Dn.SubInputSetEmptyUTxO ->
        EmptyInputSet
    Dn.SubWrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.SubWrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.SubOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Dn.SubOutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Dn.SubOutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Dn.SubWrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Dn.SubOutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Dn.SubBabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
  where
    era = ShelleyBasedEraDijkstra
