--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | This module contains top-level definitions used essentially to wrap types
-- found in ledger predicate failures. Some of those types (e.g. Value, UTxO,
-- ...) are era-dependent and parameterized with an `era` type. This is
-- particularly annoying as we need to bubble up the era to every callee while
-- having ultimately an era-agnostic final JSON representation.
--
-- So we wrap some of those type using existentials, and providing coherent JSON
-- definitions later on. This allows to simply parameterized types by `crypto`
-- which is basically always instantiated to `StandardCrypto`.
module Ogmios.Data.Ledger where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( Addr (..)
    , RewardAccount (..)
    )
import Cardano.Ledger.Alonzo.Plutus.Context
    ( ContextError
    )
import Cardano.Ledger.Api
    ( AsIx
    , AsItem
    )
import Cardano.Ledger.Conway.Core
    ( AlonzoEraScript (..)
    )
import Cardano.Ledger.Core
    ( TxOut
    , Value
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )

import qualified Prelude

data DiscriminatedEntities crypto
    = DiscriminatedAddresses (Set (Addr crypto))
    | DiscriminatedRewardAccounts (Set (RewardAccount crypto))
    | DiscriminatedPoolRegistrationCertificate (KeyHash 'StakePool crypto)
    | DiscriminatedTransaction
    deriving (Show, Ord, Eq)

data ValueInAnyEra crypto =
    forall era. Era (era crypto) =>
        ValueInAnyEra
            ( ShelleyBasedEra (era crypto)
            , Value (era crypto)
            )

data TxOutInAnyEra crypto =
    forall era. Era (era crypto) =>
        TxOutInAnyEra
            ( ShelleyBasedEra (era crypto)
            , TxOut (era crypto)
            )

data ScriptPurposeItemInAnyEra crypto =
    forall era. Era (era crypto) =>
        ScriptPurposeItemInAnyEra
            ( AlonzoBasedEra (era crypto)
            , PlutusPurpose AsItem (era crypto)
            )

data ScriptPurposeIndexInAnyEra crypto =
    forall era. Era (era crypto) =>
        ScriptPurposeIndexInAnyEra
            ( AlonzoBasedEra (era crypto)
            , PlutusPurpose AsIx (era crypto)
            )

instance Crypto crypto => Show (ScriptPurposeIndexInAnyEra crypto) where
    show = show . scriptPurposeInMostRecentEra

instance Crypto crypto => Eq (ScriptPurposeIndexInAnyEra crypto) where
    (==) = (==) `on` scriptPurposeInMostRecentEra

instance Crypto crypto => Ord (ScriptPurposeIndexInAnyEra crypto) where
    compare = compare `on` scriptPurposeInMostRecentEra

scriptPurposeInMostRecentEra
    :: ScriptPurposeIndexInAnyEra crypto
    -> PlutusPurpose AsIx (MostRecentEra (CardanoBlock crypto))
scriptPurposeInMostRecentEra = \case
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraAlonzo, ix) ->
        upgradePlutusPurposeAsIx (upgradePlutusPurposeAsIx ix)
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraBabbage, ix) ->
        upgradePlutusPurposeAsIx ix
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraConway, ix) ->
        ix

data ContextErrorInAnyEra crypto =
    forall era. Era (era crypto) =>
        ContextErrorInAnyEra
            ( AlonzoBasedEra (era crypto)
            , ContextError (era crypto)
            )

instance Show (ContextErrorInAnyEra crypto) where
    show = \case
        ContextErrorInAnyEra (AlonzoBasedEraAlonzo, e) ->
            show e
        ContextErrorInAnyEra (AlonzoBasedEraBabbage, e) ->
            show e
        ContextErrorInAnyEra (AlonzoBasedEraConway, e) ->
            show e
