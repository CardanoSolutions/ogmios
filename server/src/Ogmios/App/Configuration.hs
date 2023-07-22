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

    -- * Genesis Configurations
    , readByronGenesis
    , readShelleyGenesis
    , readAlonzoGenesis
    , readConwayGenesis

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

import Cardano.Chain.Slotting
    ( EpochSlots (..)
    )
import Data.Aeson
    ( ToJSON
    , genericToEncoding
    )
import Data.Aeson.Lens
    ( _String
    , key
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Ogmios.Data.Json.Query
    ( ByronEra
    , GenesisConfig
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra
    , ConwayEra
    , ShelleyEra
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import System.FilePath
    ( replaceFileName
    )

import qualified Cardano.Chain.Genesis as Byron
import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml

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

readByronGenesis :: MonadIO m => FilePath -> m (GenesisConfig ByronEra)
readByronGenesis configFile = do
    nodeConfig <- Yaml.decodeFileThrow @_ @Json.Value configFile
    case nodeConfig ^? key "ByronGenesisFile" . _String of
        Nothing ->
            liftIO $ fail "Missing 'ByronGenesisFile' from node's configuration."
        Just (toString -> genesisFile) -> do
            result <- runExceptT (fst <$> Byron.readGenesisData (replaceFileName configFile genesisFile))
            case result of
                Left e ->
                  liftIO $ fail ("Invalid Byron genesis configuration: " <> show e)
                Right genesisConfig ->
                    return genesisConfig

readShelleyGenesis :: MonadIO m => FilePath -> m (GenesisConfig ShelleyEra)
readShelleyGenesis configFile = do
    nodeConfig <- Yaml.decodeFileThrow @_ @Json.Value configFile
    case nodeConfig ^? key "ShelleyGenesisFile" . _String of
        Nothing ->
            liftIO $ fail "Missing 'ShelleyGenesisFile' from node's configuration."
        Just (toString -> genesisFile) -> do
            Yaml.decodeFileThrow (replaceFileName configFile genesisFile)

readAlonzoGenesis :: MonadIO m => FilePath -> m (GenesisConfig AlonzoEra)
readAlonzoGenesis configFile = do
    nodeConfig <- Yaml.decodeFileThrow @_ @Json.Value configFile
    case nodeConfig ^? key "AlonzoGenesisFile" . _String of
        Nothing ->
            liftIO $ fail "Missing 'AlonzoGenesisFile' from node's configuration."
        Just (toString -> genesisFile) -> do
            Yaml.decodeFileThrow (replaceFileName configFile genesisFile)

readConwayGenesis :: MonadIO m => FilePath -> m (GenesisConfig ConwayEra)
readConwayGenesis configFile = do
    nodeConfig <- Yaml.decodeFileThrow @_ @Json.Value configFile
    case nodeConfig ^? key "ConwayGenesisFile" . _String of
        Nothing ->
            liftIO $ fail "Missing 'ConwayGenesisFile' from node's configuration."
        Just (toString -> genesisFile) -> do
            Yaml.decodeFileThrow (replaceFileName configFile genesisFile)

deriving newtype instance ToJSON EpochSlots
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
