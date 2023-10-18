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
    )
import Ogmios.Data.Ledger.PredicateFailure.Babbage
    ( encodeUtxowFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodePoolFailure
    )

import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Data.Map as Map
import qualified Data.Set as Set

encodeLedgerFailure
    :: Crypto crypto
    => Cn.ConwayLedgerPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Cn.ConwayUtxowFailure e ->
        encodeUtxowFailure ShelleyBasedEraConway e
    Cn.ConwayCertsFailure e ->
        encodeCertsFailure e
    Cn.ConwayGovFailure e ->
        encodeGovFailure e
    Cn.ConwayWdrlNotDelegatedToDRep marginalizedCredentials ->
        ForbiddenWithdrawal { marginalizedCredentials }
    Cn.ConwayTreasuryValueMismatch computedWithdrawal providedWithdrawal ->
        TreasuryWithdrawalMismatch { providedWithdrawal, computedWithdrawal }

encodeGovFailure
    :: Cn.ConwayGovPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeGovFailure = \case
    Cn.GovActionsDoNotExist governanceActions ->
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
    Cn.DisallowedVoters voters ->
        UnauthorizedVotes voters
    Cn.ConflictingCommitteeUpdate conflictingMembers ->
        ConflictingCommitteeUpdate
            { conflictingMembers
            }
    Cn.ExpirationEpochTooSmall members ->
        InvalidCommitteeUpdate
            { alreadyRetiredMembers = Map.keysSet members
            }

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeCertsFailure = \case
    Cn.DelegateeNotRegisteredDELEG poolIds ->
        UnknownStakePool poolIds
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
        StakeCredentialDepositMismatch { providedDeposit, expectedDeposit = SNothing }
    Cn.StakeKeyRegisteredDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.StakeKeyNotRegisteredDELEG unknownCredential  ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.StakeKeyHasNonZeroRewardAccountBalanceDELEG rewardAccountBalance ->
        RewardAccountNotEmpty { rewardAccountBalance }
    Cn.DRepAlreadyRegisteredForStakeKeyDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.WrongCertificateTypeDELEG ->
        UnrecognizedCertificateType

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
        StakeCredentialDepositMismatch { providedDeposit, expectedDeposit }
    Cn.ConwayCommitteeHasPreviouslyResigned unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
