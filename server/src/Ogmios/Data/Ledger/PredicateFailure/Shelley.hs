--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Ogmios.Data.Ledger.PredicateFailure.Shelley where

import Ogmios.Prelude

import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
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

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set.NonEmpty as NESet

encodeLedgerFailureInEra
    :: forall era. ()
    => (PredicateFailure (EraRule "UTXOW" era) -> MultiEraPredicateFailure)
    -> (PredicateFailure (EraRule "DELEGS" era) -> MultiEraPredicateFailure)
    -> Sh.ShelleyLedgerPredFailure era
    -> MultiEraPredicateFailure
encodeLedgerFailureInEra encodeUtxowFailureInEra encodeDelegsFailureInEra = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailureInEra  e
    Sh.DelegsFailure e ->
        encodeDelegsFailureInEra e
    Sh.ShelleyWithdrawalsMissingAccounts (Ledger.unWithdrawals -> withdrawals) ->
        IncompleteWithdrawals withdrawals
    Sh.ShelleyIncompleteWithdrawals withdrawals ->
        IncompleteWithdrawals $ Map.map (\Mismatch {mismatchExpected} -> mismatchExpected) (NEMap.toMap withdrawals)

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure ShelleyEra
    -> MultiEraPredicateFailure
encodeLedgerFailure =
    encodeLedgerFailureInEra (encodeUtxowFailure encodeUtxoFailure) encodeDelegsFailure

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
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
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
                (\out -> (TxOutInAnyEra (ShelleyBasedEraShelley, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Sh.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (ShelleyBasedEraShelley, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Sh.UpdateFailure{} ->
        InvalidProtocolParametersUpdate

encodeDelegsFailure
    :: PredicateFailure (EraRule "DELPL" era) ~ Sh.ShelleyDelplPredFailure era
    => PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
    => PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
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
    Sh.VRFKeyHashAlreadyRegistered poolId alreadyRegisteredVrfKey ->
        StakePoolVRFKeyAlreadyRegistered { poolId, alreadyRegisteredVrfKey }

encodeDelegFailure
    :: Sh.ShelleyDelegPredFailure era
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
    Sh.DelegateeNotRegisteredDELEG poolId ->
        UnknownStakePool poolId
    Sh.StakeKeyAlreadyRegisteredDELEG credential ->
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
    Sh.MIRNegativeTransfer{} ->
        InvalidMIRTransfer
    Sh.WrongCertificateTypeDELEG ->
        UnrecognizedCertificateType
