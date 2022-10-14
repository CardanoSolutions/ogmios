--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Options
    ( -- * Command
      Command (..)
    , parseOptions
    , parseOptionsPure

      -- * Options
    , nodeSocketOption
    , nodeConfigOption
    , serverHostOption
    , serverPortOption
    , connectionTimeoutOption
    , tracersOption
    , logLevelOption

      -- * Types
    , NetworkParameters (..)
    , parseNetworkParameters

      -- * Tracers
    , Tracers (..)
    ) where

import Ogmios.Prelude

import Ogmios.App.Health
    ( Health
    , TraceHealth
    )
import Ogmios.App.Metrics
    ( TraceMetrics
    )
import Ogmios.App.Server
    ( TraceServer
    )
import Ogmios.App.Server.WebSocket
    ( TraceWebSocket
    )
import Ogmios.Control.MonadLog
    ( Severity (..)
    , Tracer
    , TracerDefinition (..)
    , TracerHKD
    , defaultTracers
    )

import Cardano.Network.Protocol.NodeToClient
    ( Block
    )
import Control.Monad.Trans.Except
    ( throwE
    , withExceptT
    )
import Data.Aeson.Lens
    ( _Integer
    , _String
    , key
    )
import Data.Char
    ( toUpper
    )
import Data.Time.Format.ISO8601
    ( iso8601ParseM
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    , EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , SystemStart (..)
    , TraceConfiguration (..)
    )
import Options.Applicative.Help.Pretty
    ( indent
    , string
    , vsep
    )
import Safe
    ( readMay
    )
import System.FilePath.Posix
    ( replaceFileName
    )

import Options.Applicative

import qualified Data.Text as T
import qualified Data.Yaml as Yaml

--
-- Command-line commands
--

data Command (f :: Type -> Type)
    = Start (f NetworkParameters) Configuration (Tracers IO 'MinSeverities)
    | Version
    | HealthCheck { healthCheckPort :: !Int }

deriving instance Eq (f NetworkParameters) => Eq (Command f)
deriving instance Show (f NetworkParameters) => Show (Command f)

parseOptions :: IO (Command Identity)
parseOptions =
    customExecParser (prefs showHelpOnEmpty) parserInfo >>= \case
        Version -> pure Version
        HealthCheck{healthCheckPort} -> pure HealthCheck{healthCheckPort}
        Start _ cfg@Configuration{nodeConfig} lvl -> do
            networkParameters <- parseNetworkParameters nodeConfig
            pure $ Start (Identity networkParameters) cfg lvl

parseOptionsPure :: [String] -> Either String (Command Proxy)
parseOptionsPure args =
    case execParserPure defaultPrefs parserInfo args of
        Success a -> Right a
        Failure e -> Left (show e)
        CompletionInvoked{} -> Left "Completion Invoked."

parserInfo :: ParserInfo (Command Proxy)
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Ogmios - A JSON-WSP WebSocket adaptor for cardano-node"
    <> header (toString $ unwords
        [ "Provides a bridge between cardano-node and WebSocket clients."
        , "Ogmios translates the existing CBOR-based Ouroboros mini-protocols"
        , "into JSON-WSP-based protocols, through WebSocket channels."
        ])
    <> footerDoc (Just $ vsep
        [ string "Examples:"
        , indent 2 $ string "Connecting to the mainnet:"
        , indent 4 $ string "$ ogmios --node-socket /path/to/node.socket --node-config /path/to/node/config"
        ])
  where
    parser =
        versionOptionOrCommand
        <|>
        healthCheckCommand
        <|>
        (Start Proxy
            <$> (Configuration
                    <$> nodeSocketOption
                    <*> nodeConfigOption
                    <*> serverHostOption
                    <*> serverPortOption
                    <*> connectionTimeoutOption
                    <*> maxInFlightOption
                )
            <*> (tracersOption <|> Tracers
                    <$> fmap Const (logLevelOption "health")
                    <*> fmap Const (logLevelOption "metrics")
                    <*> fmap Const (logLevelOption "websocket")
                    <*> fmap Const (logLevelOption "server")
                    <*> fmap Const (logLevelOption "options")
                )
        )

--
-- Command-line options
--

-- | --node-socket=FILEPATH
nodeSocketOption :: Parser FilePath
nodeSocketOption = option str $ mempty
    <> long "node-socket"
    <> metavar "FILEPATH"
    <> help "Path to the node socket."
    <> completer (bashCompleter "file")

-- | --node-config=FILEPATH
nodeConfigOption :: Parser FilePath
nodeConfigOption = option str $ mempty
    <> long "node-config"
    <> metavar "FILEPATH"
    <> help "Path to the node configuration file."
    <> completer (bashCompleter "file")

-- | [--host=IPv4], default: 127.0.0.1
serverHostOption :: Parser String
serverHostOption = option str $ mempty
    <> long "host"
    <> metavar "IPv4"
    <> help "Address to bind to."
    <> value "127.0.0.1"
    <> showDefault
    <> completer (bashCompleter "hostname")

-- | [--port=TCP/PORT], default: 1337
serverPortOption :: Parser Int
serverPortOption = option auto $ mempty
    <> long "port"
    <> metavar "TCP/PORT"
    <> help "Port to listen on."
    <> value 1337
    <> showDefault

-- | [--timeout=SECONDS], default: 90s
connectionTimeoutOption :: Parser Int
connectionTimeoutOption = option auto $ mempty
    <> long "timeout"
    <> metavar "SECONDS"
    <> help "Number of seconds of inactivity after which the server should close client connections."
    <> value 90
    <> showDefault

-- | [--max-in-flight=INT], default: 1000
maxInFlightOption :: Parser Int
maxInFlightOption = option auto $ mempty
    <> long "max-in-flight"
    <> metavar "INT"
    <> help "Max number of ChainSync requests which can be pipelined at once. Only applies to the chain-sync protocol."
    <> value 1000
    <> showDefault

-- | [--log-level=SEVERITY]
tracersOption :: Parser (Tracers m 'MinSeverities)
tracersOption = fmap defaultTracers $ option readSeverityM $ mempty
    <> long "log-level"
    <> metavar "SEVERITY"
    <> helpDoc (Just doc)
    <> completer (listCompleter severities)
  where
    doc =
        vsep $ string <$> mconcat
            [ [ "Minimal severity of all log messages." ]
            , ("- " <>) <$> severities
            , [ "Or alternatively, to turn a logger off:" ]
            , [ "- Off" ]
            ]

-- | [--log-level-{COMPONENT}=SEVERITY], default: Info
logLevelOption :: Text -> Parser (Maybe Severity)
logLevelOption component =
    option readSeverityM $ mempty
        <> long ("log-level-" <> toString component)
        <> metavar "SEVERITY"
        <> helpDoc (Just doc)
        <> value (Just Info)
        <> showDefault
        <> completer (listCompleter severities)
  where
    doc =
        string $ "Minimal severity of " <> toString component <> " log messages."

-- | [--version|-v] | version
versionOptionOrCommand :: Parser (Command f)
versionOptionOrCommand =
    flag' Version (mconcat
        [ long "version"
        , short 'v'
        , help helpText
        ])
  <|>
    subparser (mconcat
        [ hidden
        , command "version" $ info (pure Version) (progDesc helpText)
        ])
  where
    helpText = "Show the software current version and build revision."

-- | health-check
healthCheckCommand :: Parser (Command f)
healthCheckCommand =
    subparser $ command "health-check" $ info (helper <*> parser) $ mempty
        <> progDesc helpText
        <> headerDoc (Just $ vsep
            [ string $ toString $ unwords
                [ "Handy command to check whether an Ogmios server is up-and-running,"
                , "and correctly connected to a network / cardano-node."
                ]
            , mempty
            , string $ toString $ unwords
                [ "This can, for example, be wired to Docker's HEALTHCHECK"
                , "feature easily."
                ]
            ])
  where
    parser = HealthCheck <$> serverPortOption
    helpText = "Performs a health check against a running server."

--
-- Environment
--

parseNetworkParameters :: FilePath -> IO NetworkParameters
parseNetworkParameters configFile = runOrDie $ do
    config <- decodeYaml configFile
    let genesisFiles = (,)
            <$> config ^? key "ByronGenesisFile" . _String
            <*> config ^? key "ShelleyGenesisFile" . _String
    case genesisFiles of
        Nothing ->
            throwE "Missing 'ByronGenesisFile' and/or 'ShelleyGenesisFile' from Cardano's configuration?"
        Just (toString -> byronGenesisFile, toString -> shelleyGenesisFile) -> do
            byronGenesis   <- decodeYaml (replaceFileName configFile byronGenesisFile)
            shelleyGenesis <- decodeYaml (replaceFileName configFile shelleyGenesisFile)

            let mSecurityParam = byronGenesis ^? key "protocolConsts" . key "k" . _Integer
            let mNetworkMagic = shelleyGenesis ^? key "networkMagic" . _Integer
            let mSystemStart = iso8601ParseM . toString =<< shelleyGenesis ^? key "systemStart" . _String

            case (,,) <$> mNetworkMagic <*> mSystemStart <*> mSecurityParam of
                Nothing -> do
                    let
                        invalidParams = catMaybes
                            [ mSecurityParam & maybe
                                    (Just "ByronGenesis^protocolConsts.k")
                                    (const Nothing)
                            , mNetworkMagic & maybe
                                    (Just "ShelleyGenesis^networkMagic")
                                    (const Nothing)
                            , mSystemStart & maybe
                                    (Just "ShelleyGenesis^systemStart")
                                    (const Nothing)
                            ]
                      in
                        throwE $ toString $ unlines
                            [ "Failed to parse genesis parameters."
                            , "There's an issue with the following parameters:"
                            , ""
                            , "\t- " <> T.intercalate "\n\t- " invalidParams
                            ]
                Just (nm, ss, k) ->
                    return NetworkParameters
                        { networkMagic =
                            NetworkMagic (fromIntegral nm)
                        , systemStart =
                            SystemStart ss
                        , slotsPerEpoch  =
                            EpochSlots (fromIntegral $ 10 * k)
                        }
  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either die pure

    prettyParseException :: Yaml.ParseException -> String
    prettyParseException e = "Failed to decode JSON (or YAML) file: " <> show e

    decodeYaml :: FilePath -> ExceptT String IO Yaml.Value
    decodeYaml = withExceptT prettyParseException . ExceptT . Yaml.decodeFileEither

--
-- Tracers
--

data Tracers m (kind :: TracerDefinition) = Tracers
    { tracerHealth
        :: TracerHKD kind (Tracer m (TraceHealth (Health Block)))
    , tracerMetrics
        :: TracerHKD kind (Tracer m TraceMetrics)
    , tracerWebSocket
        :: TracerHKD kind (Tracer m TraceWebSocket)
    , tracerServer
        :: TracerHKD kind (Tracer m TraceServer)
    , tracerConfiguration
        :: TracerHKD kind (Tracer m TraceConfiguration)
    } deriving (Generic)

deriving instance Show (Tracers m 'MinSeverities)
deriving instance Eq (Tracers m 'MinSeverities)

--
-- Helpers
--

readSeverityM :: ReadM (Maybe Severity)
readSeverityM = maybeReader $ \case
    [] -> Nothing
    (toUpper -> h):q ->
        if h:q == "Off" then
            Just Nothing
        else
            Just <$> readMay (h:q)

severities :: [String]
severities =
    show @_ @Severity <$> [minBound .. maxBound]
