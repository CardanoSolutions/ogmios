--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Dijkstra where

import Ogmios.Prelude

import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
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
import qualified Cardano.Ledger.Dijkstra.Rules as Di

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Ogmios.Data.Ledger.PredicateFailure.Conway as Conway

----------
-- MEMPOOL
----------

encodeMempoolFailure
    :: Di.DijkstraMempoolPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeMempoolFailure = \case
    Di.LedgerFailure e ->
        encodeLedgerFailure e
    Di.MempoolFailure mempoolError ->
        UnexpectedMempoolError { mempoolError }
    Di.AllInputsAreSpent ->
        UnexpectedMempoolError
            { mempoolError = "All inputs are spent. Transaction has probably already been included" }

----------
-- LEDGER
----------

encodeLedgerFailure
    :: Di.DijkstraLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Di.DijkstraUtxowFailure e ->
        encodeUtxowFailure e
    Di.DijkstraCertsFailure e ->
        encodeCertsFailure e
    Di.DijkstraGovFailure e ->
        encodeGovFailure e
    Di.DijkstraWdrlNotDelegatedToDRep ((Set.fromList . toList) -> marginalizedCredentials) ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Di.DijkstraWithdrawalsMissingAccounts (Ledger.unWithdrawals -> withdrawals) ->
        IncompleteWithdrawals withdrawals
    Di.DijkstraIncompleteWithdrawals withdrawals ->
        IncompleteWithdrawals $ Map.map (\Mismatch {mismatchExpected} -> mismatchExpected) (NEMap.toMap withdrawals)
    Di.DijkstraTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Di.DijkstraTxRefScriptsSizeTooBig (Mismatch (toInteger -> measuredSize) (toInteger -> maximumSize)) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }
    Di.DijkstraSubLedgersFailure e ->
        encodeSubLedgersFailure e
    Di.DijkstraSpendingOutputFromSameTx _txIds ->
        SpendingOutputFromSubTransaction

--------
-- GOV
--------

encodeGovFailure
    :: Di.DijkstraGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovFailure = \case
    Di.GovActionsDoNotExist (toList -> fromList -> governanceActions) ->
        UnknownGovernanceActions { governanceActions }
    Di.MalformedProposal _govAction ->
        InvalidProtocolParametersUpdate
    Di.ProposalProcedureNetworkIdMismatch rewardAccount expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (Set.singleton rewardAccount)
            }
    Di.TreasuryWithdrawalsNetworkIdMismatch rewardAccounts expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (NESet.toSet rewardAccounts)
            }
    Di.ProposalDepositIncorrect (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Di.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Di.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers = NESet.toSet conflictingMembers
            }
    Di.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet (NEMap.toMap members)
            }
    Di.InvalidPrevGovActionId proposal ->
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
    Di.VotingOnExpiredGovAction (toList -> voters) ->
        VotingOnExpiredActions voters
    Di.ProposalCantFollow _ (Mismatch proposedVersion currentVersion) ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Di.InvalidGuardrailsScriptHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Di.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Di.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Di.VotersDoNotExist voters ->
        UnknownVoters (toList voters)
    Di.ZeroTreasuryWithdrawals _ ->
        EmptyTreasuryWithdrawal
    Di.ProposalReturnAccountDoesNotExist (Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Di.TreasuryWithdrawalReturnAccountsDoNotExist (NE.head -> Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Di.UnelectedCommitteeVoters _voters ->
        UnauthorizedVotes []

---------
-- CERTS
---------

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeCertsFailure = \case
    Cn.WithdrawalsNotInRewardsCERTS (Ledger.unWithdrawals -> withdrawals) ->
        IncompleteWithdrawals withdrawals
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
encodeDelegFailure =
    Conway.encodeDelegFailure

encodeGovCertFailure
    :: Di.DijkstraGovCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeGovCertFailure = \case
    Di.DijkstraDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Di.DijkstraDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    Di.DijkstraDRepIncorrectDeposit (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Di.DijkstraCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Di.DijkstraDRepIncorrectRefund (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Di.DijkstraCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

---------
-- UTxOW
---------

encodeUtxowFailure
    :: Di.DijkstraUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxowFailure = \case
    Di.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _computedBodyHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.UtxoFailure e ->
        encodeUtxoFailure e
    Di.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers = toList missingRedeemers }
    Di.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Di.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Di.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers = toList extraneousRedeemers }
    Di.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Di.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Di.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Di.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Di.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Di.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Di.MissingTxMetadata hash ->
        MissingMetadata hash
    Di.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Di.InvalidMetadata ->
        InvalidMetadata
    Di.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Di.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
  where
    era = AlonzoBasedEraDijkstra

--------
-- UTxO
--------

encodeUtxoFailure
    :: Di.DijkstraUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Di.UtxosFailure e ->
        encodeUtxosFailure e
    Di.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Di.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Di.OutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Di.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Di.InputSetEmptyUTxO ->
        EmptyInputSet
    Di.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Di.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Di.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Di.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Di.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Di.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys $ NEMap.toMap utxo)
    Di.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Di.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Di.NoCollateralInputs{} ->
        MissingCollateralInputs
    Di.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs { maximumCollateralInputs = fromIntegral maximumCollateralInputs, countedCollateralInputs = fromIntegral countedCollateralInputs }
    Di.ExUnitsTooBigUTxO (Mismatch providedExUnits maximumExUnits) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Di.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Di.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Di.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Di.PtrPresentInCollateralReturn out ->
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
    :: Di.DijkstraSubLedgersPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgersFailure (Di.SubLedgerFailure e) =
    encodeSubLedgerFailure e

encodeSubLedgerFailure
    :: Di.DijkstraSubLedgerPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubLedgerFailure = \case
    Di.SubUtxowFailure e ->
        encodeSubUtxowFailure e
    Di.SubCertsFailure e ->
        encodeSubCertsFailure e
    Di.SubGovFailure e ->
        encodeSubGovFailure e
    Di.SubWdrlNotDelegatedToDRep ((Set.fromList . toList) -> marginalizedCredentials) ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Di.SubTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }

encodeSubGovFailure
    :: Di.DijkstraSubGovPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubGovFailure (Di.DijkstraSubGovPredFailure f) =
    encodeGovFailure f

encodeSubCertsFailure
    :: Di.DijkstraSubCertsPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertsFailure (Di.SubCertFailure f) =
    encodeSubCertFailure f

encodeSubCertFailure
    :: Di.DijkstraSubCertPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubCertFailure = \case
    Di.SubDelegFailure (Di.DijkstraSubDelegPredFailure f) ->
        encodeDelegFailure f
    Di.SubPoolFailure (Di.DijkstraSubPoolPredFailure f) ->
        encodePoolFailure f
    Di.SubGovCertFailure (Di.DijkstraSubGovCertPredFailure f) ->
        encodeGovCertFailure f

-----------
-- SUBUTXOW
-----------

encodeSubUtxowFailure
    :: Di.DijkstraSubUtxowPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxowFailure = \case
    Di.SubUtxoFailure e ->
        encodeSubUtxoFailure e
    Di.SubInvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Di.SubMissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Di.SubMissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Di.SubScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Di.SubMissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Di.SubMissingTxMetadata hash ->
        MissingMetadata hash
    Di.SubConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch { providedAuxiliaryDataHash, computedAuxiliaryDataHash }
    Di.SubInvalidMetadata ->
        InvalidMetadata
    Di.SubExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Di.SubMissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers = toList missingRedeemers }
    Di.SubMissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Di.SubNotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Di.SubPPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Di.SubUnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Di.SubExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers = toList extraneousRedeemers }
    Di.SubMalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.SubMalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
    Di.SubScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _computedBodyHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
  where
    era = AlonzoBasedEraDijkstra

----------
-- SUBUTXO
----------

encodeSubUtxoFailure
    :: Di.DijkstraSubUtxoPredFailure DijkstraEra
    -> MultiEraPredicateFailure
encodeSubUtxoFailure = \case
    Di.SubBadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Di.SubOutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Di.SubMaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Di.SubInputSetEmptyUTxO ->
        EmptyInputSet
    Di.SubWrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.SubWrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.SubOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Di.SubOutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Di.SubOutputTooBigUTxO outs ->
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Di.SubWrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Di.SubOutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Di.SubBabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out, minAda) ->
                    ( TxOutInAnyEra (era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
  where
    era = ShelleyBasedEraDijkstra
