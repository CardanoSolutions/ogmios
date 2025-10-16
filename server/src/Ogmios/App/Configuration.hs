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
    ( _JSON'
    , _String
    , _Value
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
import qualified Cardano.Ledger.Alonzo.Genesis as Ledger
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Plutus as Ledger
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.Set as Set
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
            -- We have to decode Alonzo into an intermediate value first because
            -- the default JSON decoder from the cardano-ledge can no longer
            -- decode the genesis configuration.
            --
            -- The reason being that the decoder checks for the existence of
            -- various parameters in the cost model for Plutus V1. Yet, new
            -- parameters were added retro-actively to the cost model, so they
            -- aren't present in the genesis config. But the decoder doesn't
            -- care and fails.
            --
            -- So we have to manually add some placeholder key values to the
            -- base genesis, so that the decoder can succeed; only to drop them
            -- after from the map because they aren't actually part of the
            -- genesis configuration.
            value :: Yaml.Value <- Yaml.decodeFileThrow (replaceFileName configFile genesisFile)

            let (costModelsWithFutureParams, sourceParamNames) = withFutureParameters (value ^. getter)
                  where
                    getter = key "costModels" . key "PlutusV1" . _JSON'

            let valueWithPlaceholders = value & setter .~ Json.toJSON costModelsWithFutureParams
                  where
                    setter = key "costModels" . key "PlutusV1" . _Value

            genesis <- liftIO $ Yaml.decodeThrow (Yaml.encode valueWithPlaceholders)

            pure (withoutFutureParameters sourceParamNames genesis)
  where
    withFutureParameters :: Map Text Int64  -> (Map Text Int64, Set Text)
    withFutureParameters genesisParams = foldr
        (\paramName (params, sourceParamNames) ->
            case Map.lookup paramName params of
              Just{} -> (params, Set.insert paramName sourceParamNames)
              Nothing -> (Map.insert paramName 0 params, sourceParamNames)
        )
        (genesisParams, Set.empty)
        allV1ParamNames

    allV1ParamNames :: [Text]
    allV1ParamNames = Ledger.costModelParamNames Ledger.PlutusV1

    withoutFutureParameters :: Set Text -> GenesisConfig AlonzoEra -> GenesisConfig AlonzoEra
    withoutFutureParameters sourceParamNames config =
        let
            inner = Ledger.unAlonzoGenesisWrapper config
            costModels = Ledger.uappCostModels inner
            costModelsPruned = Map.adjust
                (either (error . show) identity
                    . Ledger.mkCostModel Ledger.PlutusV1
                    . Map.elems
                    . (`Map.restrictKeys` sourceParamNames)
                    . Ledger.costModelToMap
                )
                Ledger.PlutusV1
                (Ledger.costModelsValid costModels)
         in
            Ledger.AlonzoGenesisWrapper (inner { Ledger.uappCostModels = Ledger.mkCostModels costModelsPruned })


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
