--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings suppressed; most encode functions stubbed pending Conway-1.22.1 / Dijkstra constructor reshape.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-top-binds -Wno-redundant-constraints -Wno-deprecations #-}

module Ogmios.Data.Ledger.PredicateFailure.Conway where

import Ogmios.Prelude

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

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Api.UTxO as Ledger
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    )
import qualified Cardano.Ledger.Conway.Rules as Cn
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

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
encodeGovFailure _ =
    error "TODO(dijkstra): encodeGovFailure Conway"

encodeCertsFailure
    :: Cn.ConwayCertsPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeCertsFailure _ =
    error "TODO(dijkstra): encodeCertsFailure Conway"

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
encodeUtxoFailure _ =
    error "TODO(dijkstra): encodeUtxoFailure Conway"

encodeUtxosFailure
    :: Cn.ConwayUtxosPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeUtxosFailure _ =
    error "TODO(dijkstra): encodeUtxosFailure Conway"

encodeUtxowFailure
    :: Cn.ConwayUtxowPredFailure ConwayEra
    -> MultiEraPredicateFailure
encodeUtxowFailure _ =
    error "TODO(dijkstra): encodeUtxowFailure Conway"
