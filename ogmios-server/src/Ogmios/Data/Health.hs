--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Data.Health
    ( -- * Heath
      Health (..)
    , emptyHealth
    ) where

import Relude

import Ogmios.Data.Metrics
    ( Metrics, emptyMetrics )

import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.Time.Clock
    ( UTCTime )
import Ouroboros.Network.Block
    ( Tip (..) )

import qualified Data.Aeson as Json

-- | Capture some health heartbeat of the application. This is populated by two
-- things:
--
-- - A metric store which measure runtime statistics.
-- - An Ourobors local chain-sync client which follows the chain's tip.
data Health block = Health
    { lastKnownTip :: Tip block
    -- ^ Last known tip of the core node.
    , lastTipUpdate :: Maybe UTCTime
    -- ^ Date at which the last update was received.
    , metrics :: Metrics
    -- ^ Application metrics measured at regular interval
    } deriving (Generic, Eq, Show)

instance ToJSON (Tip block) => ToJSON (Health block) where
    toJSON = genericToJSON Json.defaultOptions

emptyHealth :: Health block
emptyHealth = Health
    { lastKnownTip = TipGenesis
    , lastTipUpdate = Nothing
    , metrics = emptyMetrics
    }
