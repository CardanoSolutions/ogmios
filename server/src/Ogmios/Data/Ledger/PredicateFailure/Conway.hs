--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Conway where

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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet

encodeLedgerFailure
    :: Cn.ConwayLedgerPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Cn.ConwayUtxowFailure e ->
        encodeUtxowFailure e
    Cn.ConwayCertsFailure e ->
        encodeCertsFailure e
    Cn.ConwayGovFailure e ->
        encodeGovFailure e
    Cn.ConwayWithdrawalsMissingAccounts _withdrawals ->
        IncompleteWithdrawals mempty
    Cn.ConwayIncompleteWithdrawals _withdrawals ->
        IncompleteWithdrawals mempty
    Cn.ConwayWdrlNotDelegatedToDRep ((Set.fromList . toList) -> marginalizedCredentials) ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Cn.ConwayTreasuryValueMismatch (Mismatch providedWithdrawal computedWithdrawal) ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Cn.ConwayTxRefScriptsSizeTooBig (Mismatch (toInteger -> measuredSize) (toInteger -> maximumSize)) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }
    Cn.ConwayMempoolFailure mempoolError ->
        UnexpectedMempoolError { mempoolError }

encodeGovFailure
    :: Cn.ConwayGovPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeGovFailure = \case
    Cn.GovActionsDoNotExist (toList -> fromList -> governanceActions) ->
        UnknownGovernanceActions { governanceActions }
    Cn.MalformedProposal _govAction ->
        InvalidProtocolParametersUpdate
    Cn.ProposalProcedureNetworkIdMismatch rewardAccount expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (Set.singleton rewardAccount)
            }
    Cn.TreasuryWithdrawalsNetworkIdMismatch rewardAccounts expectedNetwork ->
        NetworkMismatch
            { expectedNetwork
            , invalidEntities =
                DiscriminatedRewardAccounts (NESet.toSet rewardAccounts)
            }
    Cn.ProposalDepositIncorrect (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Cn.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Cn.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers = NESet.toSet conflictingMembers
            }
    Cn.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet (NEMap.toMap members)
            }
    Cn.InvalidPrevGovActionId proposal ->
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
    Cn.VotingOnExpiredGovAction (toList -> voters) ->
        VotingOnExpiredActions voters
    Cn.ProposalCantFollow _ (Mismatch proposedVersion currentVersion) ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Cn.InvalidGuardrailsScriptHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Cn.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Cn.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Cn.VotersDoNotExist voters ->
        UnknownVoters (toList voters)
    Cn.ZeroTreasuryWithdrawals _ ->
        EmptyTreasuryWithdrawal
    Cn.ProposalReturnAccountDoesNotExist (Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.TreasuryWithdrawalReturnAccountsDoNotExist (NE.head -> Ledger.AccountAddress _ (Ledger.AccountId unknownCredential)) ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.UnelectedCommitteeVoters _voters ->
        UnauthorizedVotes []

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeCertsFailure = \case
    Cn.WithdrawalsNotInRewardsCERTS _credentials ->
        IncompleteWithdrawals mempty
    Cn.CertFailure e ->
        encodeCertFailure e

encodeCertFailure
    :: Cn.ConwayCertPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeCertFailure = \case
    Cn.DelegFailure e ->
        encodeDelegFailure e
    Cn.PoolFailure e ->
        encodePoolFailure e
    Cn.GovCertFailure e ->
        encodeGovCertFailure e

encodeDelegFailure
    :: Cn.ConwayDelegPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
    -- NOTE: The discarded coin value here refers to the deposit as set in the
    -- transaction; it would be more useful and worth including if it were the
    -- expected one.
    Cn.IncorrectDepositDELEG providedDeposit -> -- !Coin
        DepositMismatch { providedDeposit, expectedDeposit = SNothing }
    Cn.StakeKeyRegisteredDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.StakeKeyNotRegisteredDELEG unknownCredential  ->
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
    :: Cn.ConwayGovCertPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeGovCertFailure = \case
    Cn.ConwayDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Cn.ConwayDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    -- NOTE: The discarded coin value here refers to the deposit as set in the
    -- transaction; it would be more useful and worth including if it were the
    -- expected one.
    Cn.ConwayDRepIncorrectDeposit (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Cn.ConwayCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Cn.ConwayDRepIncorrectRefund (Mismatch providedDeposit (SJust -> expectedDeposit)) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Cn.ConwayCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

encodeUtxoFailure
    :: Cn.ConwayUtxoPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Cn.UtxosFailure e ->
        encodeUtxosFailure e
    Cn.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Cn.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Cn.OutputTooBigUTxO outs ->
        -- TODO: In Conway, we now have access to the output 'actualSize' and
        -- the protocol parameters max value in addition to the output.
        --
        -- It would be good to report those value back in the error.
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Cn.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Cn.InputSetEmptyUTxO ->
        EmptyInputSet
    Cn.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Cn.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Cn.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Cn.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Cn.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Cn.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys $ NEMap.toMap utxo)
    Cn.WrongNetworkInTxBody (Mismatch _providedNetwork expectedNetwork) ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Cn.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Cn.NoCollateralInputs{} ->
        MissingCollateralInputs
    Cn.TooManyCollateralInputs (Mismatch countedCollateralInputs maximumCollateralInputs) ->
        TooManyCollateralInputs { maximumCollateralInputs = fromIntegral maximumCollateralInputs, countedCollateralInputs = fromIntegral countedCollateralInputs }
    Cn.ExUnitsTooBigUTxO (Mismatch providedExUnits maximumExUnits) ->
        ExecutionUnitsTooLarge { maximumExUnits, providedExUnits }
    Cn.IncorrectTotalCollateralField computedTotalCollateral declaredTotalCollateral ->
        TotalCollateralMismatch { computedTotalCollateral, declaredTotalCollateral }
    Cn.BabbageNonDisjointRefInputs xs ->
        ConflictingInputsAndReferences xs
    Cn.BabbageOutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\(out,minAda) ->
                    ( TxOutInAnyEra (era, out)
                    , Just minAda
                    )
                ) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
  where
    era = ShelleyBasedEraConway

encodeUtxosFailure
    :: Cn.ConwayUtxosPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeUtxosFailure = \case
    Cn.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Cn.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors AlonzoBasedEraConway (toList errors))

encodeUtxowFailure
    :: Cn.ConwayUtxowPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeUtxowFailure = \case
    Cn.ScriptIntegrityHashMismatch (Mismatch providedIntegrityHash computedIntegrityHash) _computedBodyHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Cn.UtxoFailure e ->
        encodeUtxoFailure e
    Cn.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers = toList missingRedeemers }
    Cn.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums = NESet.toSet missingDatums }
    Cn.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums = NESet.toSet extraneousDatums }
    Cn.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers = toList extraneousRedeemers }
    Cn.PPViewHashesDontMatch (Mismatch providedIntegrityHash computedIntegrityHash) ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Cn.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs = NESet.toSet orphanScriptInputs }
    Cn.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Cn.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Cn.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Cn.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Cn.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Cn.MissingTxMetadata hash ->
        MissingMetadata hash
    Cn.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch{providedAuxiliaryDataHash, computedAuxiliaryDataHash}
    Cn.InvalidMetadata ->
        InvalidMetadata
    Cn.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Cn.MalformedScriptWitnesses scripts ->
        MalformedScripts (NESet.toSet scripts)
    Cn.MalformedReferenceScripts scripts ->
        MalformedScripts (NESet.toSet scripts)
  where
    era = AlonzoBasedEraConway
