--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Data.Health
    ( -- * Heath
      Health (..)
    , ConnectionStatus (..)
    , Tip (..)
    , SlotInEpoch (..)
    , EpochNo (..)
    , emptyHealth
    , modifyHealth

      -- ** NetworkSynchronization
    , NetworkSynchronization (..)
    , SystemStart (..)
    , RelativeTime (..)
    , mkNetworkSynchronization

      -- ** CardanoEra
    , CardanoEra (..)
    , eraIndexToCardanoEra
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadSTM
    ( MonadSTM
    , TVar
    , atomically
    , readTVar
    , writeTVar
    )
import Ogmios.Data.Metrics
    ( Metrics
    , emptyMetrics
    )

import Cardano.Slotting.Slot
    ( EpochNo (..)
    )
import Data.Aeson
    ( ToJSON (..)
    , genericToEncoding
    )
import Data.ByteString.Builder.Scientific
    ( FPFormat (Fixed)
    , formatScientificBuilder
    )
import Data.Ratio
    ( (%)
    )
import Data.SOP.Strict
    ( NS (..)
    )
import Data.Scientific
    ( Scientific
    , unsafeFromRational
    )
import Data.Time.Clock
    ( UTCTime
    , diffUTCTime
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime (..)
    , SystemStart (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..)
    )
import Ouroboros.Network.Block
    ( Tip (..)
    )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding as Json
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras
    )

-- | Reflect the current state of the connection with the underlying node.
data ConnectionStatus
    = Connected
    | Disconnected
    deriving stock (Generic, Eq, Show)

instance ToJSON ConnectionStatus where
    toJSON = \case
        Connected -> Json.String "connected"
        Disconnected -> Json.String "disconnected"

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
    , networkSynchronization :: !(Maybe NetworkSynchronization)
    -- ^ Percentage indicator of how far the node is from the network.
    , currentEra :: !(Maybe CardanoEra)
    -- ^ Current node's era.
    , metrics :: !Metrics
    -- ^ Application metrics measured at regular interval.
    , connectionStatus :: !ConnectionStatus
    -- ^ State of the connection with the underlying node.
    , currentEpoch :: !(Maybe EpochNo)
    -- ^ Current known epoch number
    , slotInEpoch :: !(Maybe SlotInEpoch)
    -- ^ Relative slot number within the epoch
    } deriving stock (Generic, Eq, Show)

instance ToJSON (Tip block) => ToJSON (Health block) where
    toEncoding = genericToEncoding Json.defaultOptions

emptyHealth :: UTCTime -> Health block
emptyHealth startTime = Health
    { startTime
    , lastKnownTip = TipGenesis
    , lastTipUpdate = empty
    , networkSynchronization = empty
    , currentEra = empty
    , metrics = emptyMetrics
    , connectionStatus = Disconnected
    , currentEpoch = empty
    , slotInEpoch = empty
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

newtype SlotInEpoch = SlotInEpoch
    { getSlotInEpoch :: Word64
    } deriving stock (Generic, Eq, Show)
      deriving newtype (ToJSON)

-- | Captures how far is our underlying node from the network, in percentage.
-- This is calculated using:
--
-- - The era start
-- - The era's slot length
-- - The current time
-- - The current node tip
newtype NetworkSynchronization = NetworkSynchronization Scientific
    deriving stock (Generic, Eq, Ord, Show)

instance ToJSON NetworkSynchronization where
    toJSON _ = error "'toJSON' called on 'NetworkSynchronization'. This should never happen. Use 'toEncoding' instead."
    toEncoding (NetworkSynchronization s) =
        -- NOTE: Using a specific encoder here to avoid turning the value into
        -- scientific notation. Indeed, for small decimals values, aeson
        -- automatically turn the representation into scientific notation with
        -- exponent. While this is useful and harmless in many cases, it makes
        -- consuming this value a bit harder from scripts. Since we know (by
        -- construction) that the network value will necessarily have a maximum
        -- of 5 decimals, we encode it as a number with a fixed number (=5) of
        -- decimals.
        --
        -- >>> encode (NetworkSynchronization 1.0)
        -- 1.00000
        --
        -- >>> encode (NetworkSynchronization 1.4e-3)
        -- 0.00140
        --
        -- etc...
        Json.unsafeToEncoding (formatScientificBuilder Fixed (Just 5) s)

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
        tolerance = 60
        p = 100000
    in
        if abs (num - den) <= tolerance then
            NetworkSynchronization 1
        else
            NetworkSynchronization
                $ unsafeFromRational
                $ min 1 (((num * p) `div` den) % p)

-- | A Cardano era, starting from Byron and onwards.
data CardanoEra
    = Byron
    | Shelley
    | Allegra
    | Mary
    | Alonzo
    | Babbage
    deriving stock (Generic, Show, Eq, Enum, Bounded)
    deriving anyclass (ToJSON)

eraIndexToCardanoEra
    :: forall crypto. ()
    => EraIndex (CardanoEras crypto)
    -> CardanoEra
eraIndexToCardanoEra = \case
    EraIndex                Z{}      -> Byron
    EraIndex             (S Z{})     -> Shelley
    EraIndex          (S (S Z{}))    -> Allegra
    EraIndex       (S (S (S Z{})))   -> Mary
    EraIndex    (S (S (S (S Z{}))))  -> Alonzo
    EraIndex (S (S (S (S (S Z{}))))) -> Babbage
