--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Conway where

import Ogmios.Prelude

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

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Api.UTxO as Ledger
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Data.Map as Map
import qualified Data.Set as Set

encodeLedgerFailure
    :: Crypto crypto
    => Cn.ConwayLedgerPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Cn.ConwayUtxowFailure e ->
        encodeUtxowFailure e
    Cn.ConwayCertsFailure e ->
        encodeCertsFailure e
    Cn.ConwayGovFailure e ->
        encodeGovFailure e
    Cn.ConwayWdrlNotDelegatedToDRep marginalizedCredentials ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Cn.ConwayTreasuryValueMismatch computedWithdrawal providedWithdrawal ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }
    Cn.ConwayTxRefScriptsSizeTooBig (toInteger -> measuredSize) (toInteger -> maximumSize) ->
        ReferenceScriptsTooLarge { measuredSize, maximumSize }

encodeGovFailure
    :: Cn.ConwayGovPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
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
                DiscriminatedRewardAccounts rewardAccounts
            }
    Cn.ProposalDepositIncorrect providedDeposit (SJust -> expectedDeposit) ->
        GovernanceProposalDepositMismatch
            { providedDeposit
            , expectedDeposit
            }
    Cn.DisallowedVoters (toList -> voters) ->
        UnauthorizedVotes voters
    Cn.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers
            }
    Cn.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet members
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
    Cn.ProposalCantFollow _ proposedVersion currentVersion ->
        InvalidHardForkVersionBump { proposedVersion, currentVersion }
    Cn.InvalidPolicyHash providedHash expectedHash ->
        ConstitutionGuardrailsHashMismatch { providedHash, expectedHash }
    Cn.DisallowedProposalDuringBootstrap _ ->
        UnauthorizedGovernanceAction
    Cn.DisallowedVotesDuringBootstrap votes ->
        UnauthorizedVotes (toList votes)
    Cn.VotersDoNotExist voters ->
        UnknownVoters (toList voters)

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeCertsFailure = \case
    Cn.WithdrawalsNotInRewardsCERTS credentials ->
        IncompleteWithdrawals credentials
    Cn.CertFailure e ->
        encodeCertFailure e

encodeCertFailure
    :: Cn.ConwayCertPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeCertFailure = \case
    Cn.DelegFailure e ->
        encodeDelegFailure e
    Cn.PoolFailure e ->
        encodePoolFailure e
    Cn.GovCertFailure e ->
        encodeGovCertFailure e

encodeDelegFailure
    :: Cn.ConwayDelegPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
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
    Cn.StakeKeyHasNonZeroRewardAccountBalanceDELEG rewardAccountBalance ->
        RewardAccountNotEmpty { rewardAccountBalance }
    Cn.DRepAlreadyRegisteredForStakeKeyDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.DelegateeNotRegisteredDELEG poolId ->
        UnknownStakePool poolId

encodeGovCertFailure
    :: Cn.ConwayGovCertPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeGovCertFailure = \case
    Cn.ConwayDRepAlreadyRegistered knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Cn.ConwayDRepNotRegistered unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    -- NOTE: The discarded coin value here refers to the deposit as set in the
    -- transaction; it would be more useful and worth including if it were the
    -- expected one.
    Cn.ConwayDRepIncorrectDeposit providedDeposit (SJust -> expectedDeposit) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Cn.ConwayCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
    Cn.ConwayDRepIncorrectRefund providedDeposit (SJust -> expectedDeposit) ->
        DepositMismatch { providedDeposit, expectedDeposit }
    Cn.ConwayCommitteeIsUnknown unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }

encodeUtxoFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Cn.ConwayUtxoPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxoFailure = \case
    Cn.UtxosFailure e ->
        encodeUtxosFailure e
    Cn.BadInputsUTxO inputs ->
        UnknownUtxoReference inputs
    Cn.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Cn.OutputTooBigUTxO outs ->
        -- TODO: In Conway, we now have access to the output 'actualSize' and
        -- the protocol parameters max value in addition to the output.
        --
        -- It would be good to report those value back in the error.
        let culpritOutputs = (\(_, _, out) -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit culpritOutputs
    Cn.MaxTxSizeUTxO measuredSize maximumSize ->
        TransactionTooLarge { measuredSize, maximumSize }
    Cn.InputSetEmptyUTxO ->
        EmptyInputSet
    Cn.FeeTooSmallUTxO minimumRequiredFee suppliedFee ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Cn.ValueNotConservedUTxO consumed produced ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Cn.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses invalidAddrs in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts invalidAccts in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Cn.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Cn.InsufficientCollateral providedCollateral minimumRequiredCollateral ->
        InsufficientCollateral { providedCollateral, minimumRequiredCollateral }
    Cn.ScriptsNotPaidUTxO utxo ->
        CollateralInputLockedByScript (Map.keys $ Ledger.unUTxO utxo)
    Cn.WrongNetworkInTxBody expectedNetwork _providedNetwork ->
        let invalidEntities = DiscriminatedTransaction in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Cn.OutsideForecast slot ->
        SlotOutsideForeseeableFuture { slot }
    Cn.CollateralContainsNonADA value ->
        let valueInAnyEra = ValueInAnyEra (era, value) in
        NonAdaValueAsCollateral valueInAnyEra
    Cn.NoCollateralInputs{} ->
        MissingCollateralInputs
    Cn.TooManyCollateralInputs maximumCollateralInputs countedCollateralInputs ->
        TooManyCollateralInputs { maximumCollateralInputs, countedCollateralInputs }
    Cn.ExUnitsTooBigUTxO maximumExUnits providedExUnits ->
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
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
  where
    era = ShelleyBasedEraConway

encodeUtxosFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Cn.ConwayUtxosPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxosFailure = \case
    Cn.ValidationTagMismatch validationTag mismatchReason ->
        ValidationTagMismatch { validationTag, mismatchReason }
    Cn.CollectErrors errors ->
        pickPredicateFailure (encodeCollectErrors AlonzoBasedEraConway errors)

encodeUtxowFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Cn.ConwayUtxowPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxowFailure = \case
    Cn.UtxoFailure e ->
        encodeUtxoFailure e
    Cn.MissingRedeemers redeemers ->
        let missingRedeemers = ScriptPurposeItemInAnyEra . (era,) . fst <$> redeemers
         in MissingRedeemers { missingRedeemers }
    Cn.MissingRequiredDatums missingDatums _providedDatums ->
        MissingDatums { missingDatums }
    Cn.NotAllowedSupplementalDatums extraneousDatums _acceptableDatums ->
        ExtraneousDatums { extraneousDatums }
    Cn.ExtraRedeemers redeemers ->
        let extraneousRedeemers = ScriptPurposeIndexInAnyEra . (era,) <$> redeemers
         in ExtraneousRedeemers { extraneousRedeemers }
    Cn.PPViewHashesDontMatch providedIntegrityHash computedIntegrityHash ->
        ScriptIntegrityHashMismatch { providedIntegrityHash, computedIntegrityHash }
    Cn.UnspendableUTxONoDatumHash orphanScriptInputs ->
        OrphanScriptInputs { orphanScriptInputs }
    Cn.InvalidWitnessesUTXOW wits ->
        InvalidSignatures wits
    Cn.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures keys
    Cn.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses scripts
    Cn.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript scripts
    Cn.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Cn.MissingTxMetadata hash ->
        MissingMetadata hash
    Cn.ConflictingMetadataHash providedAuxiliaryDataHash computedAuxiliaryDataHash ->
        MetadataHashMismatch{providedAuxiliaryDataHash, computedAuxiliaryDataHash}
    Cn.InvalidMetadata ->
        InvalidMetadata
    Cn.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses scripts
    Cn.MalformedScriptWitnesses scripts ->
        MalformedScripts scripts
    Cn.MalformedReferenceScripts scripts ->
        MalformedScripts scripts
  where
    era = AlonzoBasedEraConway
