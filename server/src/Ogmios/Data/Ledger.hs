--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger where

import Ogmios.Prelude

import Cardano.Ledger.Address
    ( Addr (..)
    , RewardAcnt (..)
    )
import Cardano.Ledger.Core
    ( TxOut
    , Value
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )

data ValueInAnyEra crypto =
    forall era. Era (era crypto)
    => ValueInAnyEra (ShelleyBasedEra (era crypto), Value (era crypto))

data TxOutInAnyEra crypto =
    forall era. Era (era crypto)
    => TxOutInAnyEra (ShelleyBasedEra (era crypto), TxOut (era crypto))

data DiscriminatedEntities crypto
    = DiscriminatedAddresses (Set (Addr crypto))
    | DiscriminatedRewardAccounts (Set (RewardAcnt crypto))
    | DiscriminatedPoolRegistrationCertificate (KeyHash 'StakePool crypto)
    | DiscriminatedTransaction
    deriving (Show, Ord, Eq)
