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
    ( AsItem
    , AsIx
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

import qualified Prelude

data DiscriminatedEntities
    = DiscriminatedAddresses (Set Addr)
    | DiscriminatedRewardAccounts (Set RewardAccount)
    | DiscriminatedPoolRegistrationCertificate (KeyHash 'StakePool)
    | DiscriminatedTransaction
    deriving (Show, Ord, Eq)

data ValueInAnyEra =
    forall era. Era era =>
        ValueInAnyEra
            ( ShelleyBasedEra era
            , Value era
            )

data TxOutInAnyEra =
    forall era. Era era =>
        TxOutInAnyEra
            ( ShelleyBasedEra era
            , TxOut era
            )

data ScriptPurposeItemInAnyEra =
    forall era. Era era =>
        ScriptPurposeItemInAnyEra
            ( AlonzoBasedEra era
            , PlutusPurpose AsItem era
            )

data ScriptPurposeIndexInAnyEra =
    forall era. Era era =>
        ScriptPurposeIndexInAnyEra
            ( AlonzoBasedEra era
            , PlutusPurpose AsIx era
            )

instance Show ScriptPurposeIndexInAnyEra where
    show = show . scriptPurposeInMostRecentEra

instance Eq ScriptPurposeIndexInAnyEra where
    (==) = (==) `on` scriptPurposeInMostRecentEra

instance Ord ScriptPurposeIndexInAnyEra where
    compare = compare `on` scriptPurposeInMostRecentEra

scriptPurposeInMostRecentEra
    :: ScriptPurposeIndexInAnyEra
    -> PlutusPurpose AsIx (MostRecentEra (CardanoBlock crypto))
scriptPurposeInMostRecentEra = \case
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraAlonzo, ix) ->
        upgradePlutusPurposeAsIx (upgradePlutusPurposeAsIx ix)
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraBabbage, ix) ->
        upgradePlutusPurposeAsIx ix
    ScriptPurposeIndexInAnyEra (AlonzoBasedEraConway, ix) ->
        ix

data ContextErrorInAnyEra =
    forall era. Era era =>
        ContextErrorInAnyEra
            ( AlonzoBasedEra era
            , ContextError era
            )

instance Show ContextErrorInAnyEra where
    show = \case
        ContextErrorInAnyEra (AlonzoBasedEraAlonzo, e) ->
            show e
        ContextErrorInAnyEra (AlonzoBasedEraBabbage, e) ->
            show e
        ContextErrorInAnyEra (AlonzoBasedEraConway, e) ->
            show e
