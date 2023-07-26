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
    ( encodeDeplFailure
    )

import qualified Cardano.Ledger.Conway.Rules as Cn

encodeLedgerFailure
    :: Crypto crypto
    => Cn.ConwayLedgerPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Cn.ConwayUtxowFailure e ->
        encodeUtxowFailure ShelleyBasedEraConway e
    Cn.ConwayDelegsFailure e ->
        encodeDelegsFailure e
    Cn.ConwayTallyFailure e ->
        encodeTallyFailure e

encodeTallyFailure
    :: Cn.ConwayTallyPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeTallyFailure = \case
    Cn.VoterDoesNotHaveRole voter requiredRole ->
        UnauthorizedVote { voter, requiredRole }
    Cn.GovernanceActionDoesNotExist governanceAction ->
        UnknownGovernanceAction { governanceAction }

encodeDelegsFailure
    :: Cn.ConwayDelegsPredFailure (ConwayEra crypto)
    -> MultiEraPredicateFailure crypto
encodeDelegsFailure = \case
    Cn.DelegateeNotRegisteredDELEG poolIds ->
        UnknownStakePool poolIds
    Cn.WithdrawalsNotInRewardsDELEGS credentials ->
        IncompleteWithdrawals credentials
    Cn.CertFailure e ->
        encodeDeplFailure e
