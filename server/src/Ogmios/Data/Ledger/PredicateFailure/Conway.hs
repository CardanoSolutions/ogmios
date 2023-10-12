--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Conway where

import Ogmios.Prelude

import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Babbage
    ( encodeUtxowFailure
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodePoolFailure
    )

import qualified Cardano.Ledger.Conway.Rules as Cn

encodeLedgerFailure
    :: Crypto crypto
    => Cn.ConwayLedgerPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Cn.ConwayUtxowFailure e ->
        encodeUtxowFailure ShelleyBasedEraConway e
    Cn.ConwayCertsFailure e ->
        encodeCertsFailure e
    Cn.ConwayTallyFailure e ->
        encodeTallyFailure e
    Cn.ConwayWdrlNotDelegatedToDRep marginalizedCredentials ->
        ForbiddenWithdrawal { marginalizedCredentials }

encodeTallyFailure
    :: Cn.ConwayTallyPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeTallyFailure = \case
    Cn.GovernanceActionDoesNotExist governanceAction ->
        UnknownGovernanceAction { governanceAction }

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
    Cn.VDelFailure e ->
        encodeVDelFailure e

encodeDelegFailure
    :: Cn.ConwayDelegPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeDelegFailure = \case
    -- NOTE: The discarded coin value here refers to the deposit as set in the
    -- transaction; it would be more useful and worth including if it were the
    -- expected one.
    Cn.IncorrectDepositDELEG _coin -> -- !Coin
        StakeCredentialDepositMismatch
    Cn.StakeKeyAlreadyRegisteredDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.StakeKeyNotRegisteredDELEG unknownCredential  ->
        StakeCredentialNotRegistered { unknownCredential }
    Cn.StakeKeyHasNonZeroAccountBalanceDELEG rewardAccountBalance ->
        RewardAccountNotEmpty { rewardAccountBalance }
    Cn.DRepAlreadyRegisteredForStakeKeyDELEG knownCredential ->
        StakeCredentialAlreadyRegistered { knownCredential }
    Cn.WrongCertificateTypeDELEG ->
        UnrecognizedCertificateType

encodeVDelFailure
    :: Cn.ConwayVDelPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeVDelFailure = \case
    Cn.ConwayDRepAlreadyRegisteredVDEL knownDelegateRepresentative ->
        DRepAlreadyRegistered { knownDelegateRepresentative }
    Cn.ConwayDRepNotRegisteredVDEL unknownDelegateRepresentative ->
        DRepNotRegistered { unknownDelegateRepresentative }
    -- NOTE: The discarded coin value here refers to the deposit as set in the
    -- transaction; it would be more useful and worth including if it were the
    -- expected one.
    Cn.ConwayDRepIncorrectDepositVDEL _coin ->
        StakeCredentialDepositMismatch
    Cn.ConwayCommitteeHasResignedVDEL unknownConstitutionalCommitteeMember ->
        UnknownConstitutionalCommitteeMember { unknownConstitutionalCommitteeMember }
