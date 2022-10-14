--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.App.Configuration
    (
    -- * Configuration
    Configuration (..)
    , Severity (..)

    -- * NetworkParameters
    , NetworkParameters (..)
    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart

    -- * Logging
    , TraceConfiguration (..)
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )

import Cardano.Chain.Slotting
    ( EpochSlots (..)
    )
import Data.Aeson
    ( ToJSON
    , genericToEncoding
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..)
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )

import qualified Data.Aeson as Json

data Configuration = Configuration
    { nodeSocket :: !FilePath
    , nodeConfig :: !FilePath
    , serverHost :: !String
    , serverPort :: !Int
    , connectionTimeout :: !Int
    , maxInFlight :: !Int
    } deriving (Generic, Eq, Show)

data NetworkParameters = NetworkParameters
    { networkMagic :: !NetworkMagic
    , systemStart :: !SystemStart
    , slotsPerEpoch :: !EpochSlots
    } deriving stock (Generic, Eq, Show)
      deriving anyclass (ToJSON)

mkSystemStart :: Int -> SystemStart
mkSystemStart =
    SystemStart . posixSecondsToUTCTime . toPicoResolution . toEnum
  where
    toPicoResolution = (*1000000000000)

deriving newtype instance ToJSON EpochSlots
deriving newtype instance ToJSON SystemStart
deriving newtype instance ToJSON NetworkMagic

--
-- Logging
--

data TraceConfiguration where
    ConfigurationNetwork
        :: { networkParameters :: NetworkParameters }
        -> TraceConfiguration
    deriving stock (Generic, Show)

instance ToJSON TraceConfiguration where
    toEncoding =
        genericToEncoding Json.defaultOptions

instance HasSeverityAnnotation TraceConfiguration where
    getSeverityAnnotation = \case
        ConfigurationNetwork{} -> Info
