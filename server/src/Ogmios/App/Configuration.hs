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
    , IncludeCbor (..)
    , MetadataFormat (..)
    , includeAllCbor
    , omitOptionalCbor
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
import Control.Arrow
    ( left
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
import Ogmios.Data.Json.Prelude
    ( IncludeCbor (..)
    , MetadataFormat (..)
    , includeAllCbor
    , omitOptionalCbor
    )
import Ogmios.Data.Json.Query
    ( GenesisConfig
    )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..)
    )
import Ouroboros.Network.Magic
    ( NetworkMagic (..)
    )
import System.FilePath
    ( replaceFileName
    )

import qualified Cardano.Chain.Genesis as Byron
import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

data Configuration = Configuration
    { nodeSocket :: !FilePath
    , nodeConfig :: !FilePath
    , serverHost :: !String
    , serverPort :: !Int
    , connectionTimeout :: !Int
    , maxInFlight :: !Int
    , includeCbor :: !IncludeCbor
    , metadataFormat :: !MetadataFormat
    , strictRpc :: !Bool
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

readConwayGenesis :: MonadIO m => FilePath -> m (Either Text (GenesisConfig ConwayEra))
readConwayGenesis configFile = do
    parseConfigResult <- liftIO $ Yaml.decodeFileEither @Json.Value configFile
    case parseConfigResult of
        Left e -> do
            pure $ Left $
                "Invalid (non-JSON or non-YAML) or missing node's configuration file: " <> formatParseException e
        Right nodeConfig ->
            case nodeConfig ^? key "ConwayGenesisFile" . _String of
                Nothing ->
                    pure $ Left "Missing 'ConwayGenesisFile' from node's configuration."
                Just (toString -> genesisFile) -> do
                    parseGenesisResult <- liftIO $ Yaml.decodeFileEither (replaceFileName configFile genesisFile)
                    pure $ left
                        (\e ->
                            "Could not decode the genesis configuration probably \
                            \because of a mismatch between the configuration and \
                            \the current version of the underlying ledger library \
                            \in-use in Ogmios: " <> formatParseException e
                        )
                        parseGenesisResult
  where
    formatParseException :: Yaml.ParseException -> Text
    formatParseException
        = T.dropWhileEnd (== '.')
        . T.replace "Error in $[" "invalid item ["
        . T.replace "Error in $: " ""
        . T.replace "Error in $: key" "field"
        . T.replace "\n" " "
        . T.replace "Aeson exception:\n" ""
        . toText
        . Yaml.prettyPrintParseException

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
