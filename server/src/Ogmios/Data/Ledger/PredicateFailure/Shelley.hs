--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Shelley where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( unWithdrawals
    )
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    , mismatchSupplied
    )
import Cardano.Ledger.Core
    ( EraRule
    )
import Control.State.Transition
    ( STS (..)
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , TxOutInAnyEra (..)
    , ValidityInterval (..)
    , ValueInAnyEra (..)
    )

import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set.NonEmpty as NESet

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure ShelleyEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e
    Sh.ShelleyIncompleteWithdrawals ws ->
        IncompleteWithdrawals
            { withdrawals = mismatchSupplied <$> NEMap.toMap ws
            }
    -- TODO: both ledger failures currently fold into IncompleteWithdrawals;
    -- introduce a dedicated WithdrawalsMissingAccounts variant.
    Sh.ShelleyWithdrawalsMissingAccounts ws ->
        IncompleteWithdrawals
            { withdrawals = unWithdrawals ws
            }

encodeUtxowFailure
    :: forall era. ()
    => (PredicateFailure (EraRule "UTXO" era) -> MultiEraPredicateFailure)
    -> Sh.ShelleyUtxowPredFailure era
    -> MultiEraPredicateFailure
encodeUtxowFailure encodeUtxoFailure_ = \case
    Sh.InvalidWitnessesUTXOW wits ->
        InvalidSignatures (toList wits)
    Sh.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures (NESet.toSet keys)
    Sh.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses (NESet.toSet scripts)
    Sh.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript (NESet.toSet scripts)
    Sh.MIRInsufficientGenesisSigsUTXOW{} ->
        InvalidMIRTransfer
    Sh.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Sh.MissingTxMetadata hash ->
        MissingMetadata hash
    Sh.ConflictingMetadataHash (Mismatch providedAuxiliaryDataHash computedAuxiliaryDataHash) ->
        MetadataHashMismatch{providedAuxiliaryDataHash, computedAuxiliaryDataHash}
    Sh.InvalidMetadata ->
        InvalidMetadata
    Sh.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses (NESet.toSet scripts)
    Sh.UtxoFailure e ->
        encodeUtxoFailure_ e

encodeUtxoFailure
    :: Sh.ShelleyUtxoPredFailure ShelleyEra
    -> MultiEraPredicateFailure
encodeUtxoFailure = \case
    Sh.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Sh.ExpiredUTxO (Mismatch timeToLive currentSlot) ->
        let validityInterval = ValidityInterval
                { invalidBefore = SNothing
                , invalidHereafter = SJust timeToLive
                }
         in TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Sh.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge
            { measuredSize = toInteger measuredSize
            , maximumSize = toInteger maximumSize
            }
    Sh.InputSetEmptyUTxO ->
        EmptyInputSet
    Sh.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall{minimumRequiredFee, suppliedFee}
    Sh.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (ShelleyBasedEraShelley, consumed) in
        let valueProduced = ValueInAnyEra (ShelleyBasedEraShelley, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Sh.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (ShelleyBasedEraShelley, out), Nothing)) <$> toList outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Sh.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (ShelleyBasedEraShelley, out)) <$> toList outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Sh.UpdateFailure{} ->
        InvalidProtocolParametersUpdate

encodeDelegsFailure
    :: forall era.
        ( PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
        , PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
        , PredicateFailure (EraRule "DELPL" era) ~ Sh.ShelleyDelplPredFailure era
        )
    => Sh.ShelleyDelegsPredFailure era
    -> MultiEraPredicateFailure
encodeDelegsFailure = \case
    Sh.DelplFailure e ->
        encodeDeplFailure e

encodeDeplFailure
    :: forall era.
        ( PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
        , PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
        )
    => Sh.ShelleyDelplPredFailure era
    -> MultiEraPredicateFailure
encodeDeplFailure = \case
    Sh.PoolFailure e ->
        encodePoolFailure e
    Sh.DelegFailure e ->
        encodeDelegFailure e

encodePoolFailure
    :: Sh.ShelleyPoolPredFailure era
    -> MultiEraPredicateFailure
encodePoolFailure = \case
    Sh.StakePoolNotRegisteredOnKeyPOOL poolId ->
        UnknownStakePool { poolId }
    Sh.StakePoolRetirementWrongEpochPOOL (Mismatch currentEpoch listedEpoch) (Mismatch _ firstInvalidEpoch) ->
        InvalidStakePoolRetirementEpoch { currentEpoch, listedEpoch, firstInvalidEpoch }
    Sh.StakePoolCostTooLowPOOL (Mismatch declaredCost minimumPoolCost) ->
        StakePoolCostTooLow { declaredCost, minimumPoolCost }
    Sh.WrongNetworkPOOL (Mismatch _ expectedNetwork) poolId ->
        let invalidEntities = DiscriminatedPoolRegistrationCertificate poolId in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.PoolMedataHashTooBig poolId computedMetadataHashSize ->
        StakePoolMetadataHashTooLarge { poolId, computedMetadataHashSize }
    -- TODO: VRF-key reuse across pool registrations isn't represented
    -- distinctly in MultiEraPredicateFailure; fold into UnknownStakePool
    -- referencing the conflicting poolId for now.
    Sh.VRFKeyHashAlreadyRegistered poolId _vrfKeyHash ->
        UnknownStakePool { poolId }

encodeDelegFailure
    :: Sh.ShelleyDelegPredFailure era
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
    Sh.DelegAccountAlreadyRegistered (Sh.AccountAlreadyRegistered credential) ->
        StakeCredentialAlreadyRegistered credential
    Sh.StakeKeyNotRegisteredDELEG credential ->
        StakeCredentialNotRegistered credential
    Sh.StakeDelegationImpossibleDELEG credential ->
        StakeCredentialNotRegistered credential
    Sh.StakeKeyNonZeroAccountBalanceDELEG balance ->
        RewardAccountNotEmpty balance
    Sh.GenesisKeyNotInMappingDELEG{} ->
        InvalidGenesisDelegation
    Sh.DuplicateGenesisVRFDELEG{} ->
        InvalidGenesisDelegation
    Sh.DuplicateGenesisDelegateDELEG{} ->
        InvalidGenesisDelegation
    Sh.InsufficientForInstantaneousRewardsDELEG{} ->
        InvalidMIRTransfer
    Sh.MIRCertificateTooLateinEpochDELEG{} ->
        InvalidMIRTransfer
    Sh.MIRTransferNotCurrentlyAllowed ->
        InvalidMIRTransfer
    Sh.MIRNegativesNotCurrentlyAllowed ->
        InvalidMIRTransfer
    Sh.InsufficientForTransferDELEG{} ->
        InvalidMIRTransfer
    Sh.MIRProducesNegativeUpdate ->
        InvalidMIRTransfer
    Sh.DelegateeNotRegisteredDELEG poolId ->
        UnknownStakePool { poolId }
    Sh.MIRNegativeTransfer{} ->
        InvalidMIRTransfer
    Sh.WrongCertificateTypeDELEG ->
        UnrecognizedCertificateType
