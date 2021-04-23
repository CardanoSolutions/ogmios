--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Data.Health
    ( -- * Heath
      Health (..)
    , emptyHealth
    , modifyHealth
      -- ** NetworkSynchronization
    , NetworkSynchronization
    , mkNetworkSynchronization
      -- ** CardanoEra
    , CardanoEra (..)
    ) where

import Relude hiding
    ( TVar, atomically, readTVar, writeTVar )

import Ogmios.Control.MonadSTM
    ( MonadSTM, TVar, atomically, readTVar, writeTVar )
import Ogmios.Data.Metrics
    ( Metrics, emptyMetrics )

import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.Time.Clock
    ( UTCTime, diffUTCTime )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..), SystemStart (..) )
import Ouroboros.Network.Block
    ( Tip (..) )

import qualified Data.Aeson as Json

-- | Capture some health heartbeat of the application. This is populated by two
-- things:
--
-- - A metric store which measure runtime statistics.
-- - An Ouroboros local chain-sync client which follows the chain's tip.
data Health block = Health
    { startTime :: UTCTime
    -- ^ Time at which the application was started
    , lastKnownTip :: !(Tip block)
    -- ^ Last known tip of the core node.
    , lastTipUpdate :: !(Maybe UTCTime)
    -- ^ Date at which the last update was received.
    , networkSynchronization :: !NetworkSynchronization
    -- ^ Percentage indicator of how far the node is from the network.
    , currentEra :: !CardanoEra
    -- ^ Current node's era.
    , metrics :: !Metrics
    -- ^ Application metrics measured at regular interval
    } deriving (Generic, Eq, Show)

instance ToJSON (Tip block) => ToJSON (Health block) where
    toJSON = genericToJSON Json.defaultOptions

emptyHealth :: UTCTime -> Health block
emptyHealth startTime = Health
    { startTime
    , lastKnownTip = TipGenesis
    , lastTipUpdate = Nothing
    , networkSynchronization = NetworkSynchronization 0
    , currentEra = Byron
    , metrics = emptyMetrics
    }

modifyHealth
    :: forall m block.
        ( MonadSTM m
        )
    => TVar m (Health block)
    -> (Health block -> Health block)
    -> m (Health block)
modifyHealth tvar fn =
    atomically $ do
        health <- fn <$> readTVar tvar
        health <$ writeTVar tvar health


-- | Captures how far is our underlying node from the network, in percentage.
-- This is calculated using:
--
-- - The era start
-- - The era's slot length
-- - The current time
-- - The current node tip
newtype NetworkSynchronization = NetworkSynchronization Double
    deriving (Generic, Eq, Show)

instance ToJSON NetworkSynchronization where
    toJSON (NetworkSynchronization p) = toJSON p

-- | Calculate the network synchronization from various parameters.
mkNetworkSynchronization
    :: SystemStart -- System's start
    -> UTCTime -- Current Time
    -> RelativeTime -- Tip time, relative to the system start
    -> NetworkSynchronization
mkNetworkSynchronization systemStart now relativeSlotTime =
    let
        num = round $ getRelativeTime relativeSlotTime :: Integer
        den = round $ now `diffUTCTime` getSystemStart systemStart :: Integer
        p = 100
    in
        NetworkSynchronization $ fromIntegral (num * p `div` den) / fromIntegral p


-- | A Cardano era, starting from Byron and onwards.
data CardanoEra
    = Byron
    | Shelley
    | Allegra
    | Mary
    | Alonzo
    deriving (Generic, Show, Eq, Enum, Bounded)

instance ToJSON CardanoEra where
    toJSON = genericToJSON Json.defaultOptions
