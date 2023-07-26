--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Ledger.PredicateFailure where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Ledger.PredicateFailure
    ( Addr (..)
    , AuxiliaryDataHash (..)
    , Coin
    , Credential (..)
    , DiscriminatedEntities (..)
    , EpochNo (..)
    , Hash
    , KeyHash (..)
    , KeyRole (..)
    , MultiEraPredicateFailure (..)
    , Network (..)
    , ProtVer (..)
    , RewardAcnt (..)
    , ScriptHash (..)
    , SlotNo (..)
    , TxIn (..)
    , TxOutInAnyEra (..)
    , VKey (..)
    , ValueInAnyEra (..)
    )

encodePredicateFailure
    :: Crypto crypto
    => MultiEraPredicateFailure crypto
    -> Json
encodePredicateFailure =
    error "TODO"
