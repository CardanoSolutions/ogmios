--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Ogmios.Data.Ledger.PredicateFailure.Shelley where

import Ogmios.Prelude

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

encodeLedgerFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Sh.ShelleyLedgerPredFailure (ShelleyEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era crypto.
        ( crypto ~ EraCrypto era
        )
    => (PredicateFailure (EraRule "UTXO" era) -> MultiEraPredicateFailure crypto)
    -> Sh.ShelleyUtxowPredFailure era
    -> MultiEraPredicateFailure crypto
encodeUtxowFailure encodeUtxoFailure_ = \case
    Sh.InvalidWitnessesUTXOW wits ->
        InvalidSignatures wits
    Sh.MissingVKeyWitnessesUTXOW keys ->
        MissingSignatures keys
    Sh.MissingScriptWitnessesUTXOW scripts ->
        MissingScriptWitnesses scripts
    Sh.ScriptWitnessNotValidatingUTXOW scripts ->
        FailingScript scripts
    Sh.MIRInsufficientGenesisSigsUTXOW{} ->
        InvalidMIRTransfer
    Sh.MissingTxBodyMetadataHash hash ->
        MissingMetadataHash hash
    Sh.MissingTxMetadata hash ->
        MissingMetadata hash
    Sh.ConflictingMetadataHash providedAuxiliaryDataHash computedAuxiliaryDataHash ->
        MetadataHashMismatch{providedAuxiliaryDataHash, computedAuxiliaryDataHash}
    Sh.InvalidMetadata ->
        InvalidMetadata
    Sh.ExtraneousScriptWitnessesUTXOW scripts ->
        ExtraneousScriptWitnesses scripts
    Sh.UtxoFailure e ->
        encodeUtxoFailure_ e

encodeUtxoFailure
    :: Crypto crypto
    => Sh.ShelleyUtxoPredFailure (ShelleyEra crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxoFailure = \case
    Sh.BadInputsUTxO inputs ->
        UnknownUtxoReference inputs
    Sh.ExpiredUTxO timeToLive currentSlot ->
        let validityInterval = ValidityInterval
                { invalidBefore = SNothing
                , invalidHereafter = SJust timeToLive
                }
         in TransactionOutsideValidityInterval{ validityInterval, currentSlot }
    Sh.MaxTxSizeUTxO measuredSize maximumSize ->
        TransactionTooLarge { measuredSize, maximumSize }
    Sh.InputSetEmptyUTxO ->
        EmptyInputSet
    Sh.FeeTooSmallUTxO minimumRequiredFee suppliedFee ->
        TransactionFeeTooSmall{minimumRequiredFee, suppliedFee}
    Sh.ValueNotConservedUTxO consumed produced ->
        let valueConsumed = ValueInAnyEra (ShelleyBasedEraShelley, consumed) in
        let valueProduced = ValueInAnyEra (ShelleyBasedEraShelley, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Sh.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses invalidAddrs in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts invalidAccts in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (ShelleyBasedEraShelley, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Sh.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (ShelleyBasedEraShelley, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Sh.UpdateFailure{} ->
        InvalidProtocolParametersUpdate

encodeDelegsFailure
    :: PredicateFailure (EraRule "DELPL" era) ~ Sh.ShelleyDelplPredFailure era
    => PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
    => PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
    => Sh.ShelleyDelegsPredFailure era
    -> MultiEraPredicateFailure (EraCrypto era)
encodeDelegsFailure = \case
    Sh.DelegateeNotRegisteredDELEG poolId ->
        UnknownStakePool poolId
    Sh.WithdrawalsNotInRewardsDELEGS withdrawals ->
        IncompleteWithdrawals withdrawals
    Sh.DelplFailure e ->
        encodeDeplFailure e

encodeDeplFailure
    :: forall era.
        ( PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
        , PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
        )
    => Sh.ShelleyDelplPredFailure era
    -> MultiEraPredicateFailure (EraCrypto era)
encodeDeplFailure = \case
    Sh.PoolFailure e ->
        encodePoolFailure e
    Sh.DelegFailure e ->
        encodeDelegFailure e

encodePoolFailure
    :: Sh.ShelleyPoolPredFailure era
    -> MultiEraPredicateFailure (EraCrypto era)
encodePoolFailure = \case
    Sh.StakePoolNotRegisteredOnKeyPOOL poolId ->
        UnknownStakePool { poolId }
    Sh.StakePoolRetirementWrongEpochPOOL currentEpoch listedEpoch firstInvalidEpoch ->
        InvalidStakePoolRetirementEpoch { currentEpoch, listedEpoch, firstInvalidEpoch }
    Sh.WrongCertificateTypePOOL{} ->
        UnrecognizedCertificateType
    Sh.StakePoolCostTooLowPOOL declaredCost minimumPoolCost ->
        StakePoolCostTooLow { declaredCost, minimumPoolCost }
    Sh.WrongNetworkPOOL _specified expectedNetwork poolId ->
        let invalidEntities = DiscriminatedPoolRegistrationCertificate poolId in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Sh.PoolMedataHashTooBig poolId computedMetadataHashSize ->
        StakePoolMetadataHashTooLarge { poolId, computedMetadataHashSize }

encodeDelegFailure
    :: Sh.ShelleyDelegPredFailure era
    -> MultiEraPredicateFailure (EraCrypto era)
encodeDelegFailure = \case
    Sh.StakeKeyAlreadyRegisteredDELEG credential ->
        StakeCredentialAlreadyRegistered credential
    Sh.StakeKeyInRewardsDELEG credential ->
        StakeCredentialAlreadyRegistered credential
    Sh.StakeKeyNotRegisteredDELEG credential ->
        StakeCredentialNotRegistered credential
    Sh.StakeDelegationImpossibleDELEG credential ->
        StakeCredentialNotRegistered credential
    Sh.StakeKeyNonZeroAccountBalanceDELEG balance ->
        RewardAccountNotEmpty (fromMaybe mempty balance)
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
    Sh.MIRNegativeTransfer{} ->
        InvalidMIRTransfer
    Sh.WrongCertificateTypeDELEG ->
        UnrecognizedCertificateType
