--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings disabled while encodeUtxowFailure/encodeUtxoFailure are stubbed for cardano-ledger-shelley 1.18.1's NonEmpty containers.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-incomplete-patterns -Wno-redundant-constraints -Wno-unused-top-binds #-}

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

import qualified Cardano.Ledger.Shelley.Rules as Sh

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure ShelleyEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxowFailure
    :: forall era. ()
    => (PredicateFailure (EraRule "UTXO" era) -> MultiEraPredicateFailure)
    -> Sh.ShelleyUtxowPredFailure era
    -> MultiEraPredicateFailure
encodeUtxowFailure _ _ =
    -- TODO(dijkstra): InvalidSignatures/MissingSignatures/MissingScriptWitnesses arms now receive NonEmpty/NonEmptySet from cardano-ledger 1.18.1 — need toList conversions for our MultiEraPredicateFailure shape.
    error "TODO(dijkstra): encodeUtxowFailure Shelley"

encodeUtxoFailure
    :: Sh.ShelleyUtxoPredFailure ShelleyEra
    -> MultiEraPredicateFailure
encodeUtxoFailure _ =
    -- TODO(dijkstra): same shape issues as encodeUtxowFailure (NonEmpty containers, possibly renamed constructors in cardano-ledger-shelley 1.18.1).
    error "TODO(dijkstra): encodeUtxoFailure Shelley"

encodeDelegsFailure
    :: PredicateFailure (EraRule "DELPL" era) ~ Sh.ShelleyDelplPredFailure era
    => PredicateFailure (EraRule "POOL" era)  ~ Sh.ShelleyPoolPredFailure era
    => PredicateFailure (EraRule "DELEG" era) ~ Sh.ShelleyDelegPredFailure era
    => Sh.ShelleyDelegsPredFailure era
    -> MultiEraPredicateFailure
encodeDelegsFailure _ =
    -- TODO(dijkstra): ShelleyDelegsPredFailure constructors changed in cardano-ledger-shelley 1.18.1 (WithdrawalsNotInRewardsDELEGS removed, DelegateeNotRegisteredDELEG now lives on a sibling type). Needs reshape.
    error "TODO(dijkstra): encodeDelegsFailure Shelley"

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

encodeDelegFailure
    :: Sh.ShelleyDelegPredFailure era
    -> MultiEraPredicateFailure
encodeDelegFailure = \case
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
