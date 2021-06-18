--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.App.Options
    ( -- * Command
      Command (..)
    , parseOptions
    , parserInfo

      -- ** Options
    , Options (..)
    , nodeSocketOption
    , nodeConfigOption
    , serverHostOption
    , serverPortOption
    , connectionTimeoutOption
    , logLevelOption

      -- *** Types
    , NetworkParameters (..)
    , parseNetworkParameters

    , NetworkMagic (..)
    , EpochSlots (..)
    , SystemStart (..)
    , mkSystemStart
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadLog
    ( Severity (..) )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley
    ( ShelleyEra )
import Cardano.Node.Configuration.POM
    ( PartialNodeConfiguration (..), parseNodeConfigurationFP )
import Cardano.Node.Protocol.Shelley
    ( readGenesisAny )
import Cardano.Node.Types
    ( ConfigYamlFilePath (..)
    , NodeProtocolConfiguration (..)
    , NodeShelleyProtocolConfiguration (..)
    )
import Cardano.Slotting.Slot
    ( EpochSize (..) )
import Control.Monad.Trans.Except
    ( except, throwE, withExceptT )
import Data.Aeson
    ( ToJSON )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Shelley.Spec.Ledger.Genesis
    ( ShelleyGenesis (..) )

import Options.Applicative

--
-- Command-line commands
--

data Command (f :: Type -> Type)
    = Start (f NetworkParameters)  Options
    | Version

parseOptions :: IO (Command Identity)
parseOptions =
    customExecParser (prefs showHelpOnEmpty) parserInfo >>= \case
        Version -> pure Version
        Start _ opts@Options{nodeConfig} -> do
            networkParameters <- parseNetworkParameters nodeConfig
            pure $ Start (Identity networkParameters) opts

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
        versionOption
        <|>
        (Start Proxy <$>
            (Options
                <$> nodeSocketOption
                <*> nodeConfigOption
                <*> serverHostOption
                <*> serverPortOption
                <*> connectionTimeoutOption
                <*> maxInFlightOption
                <*> logLevelOption
            )
        )

--
-- Command-line options
--

data Options = Options
    { nodeSocket :: !FilePath
    , nodeConfig :: !FilePath
    , serverHost :: !String
    , serverPort :: !Int
    , connectionTimeout :: !Int
    , maxInFlight :: !Int
    , logLevel :: !Severity
    } deriving (Generic, Eq, Show)

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
    <> help "Max number of ChainSync requests which can be pipelined at once. Only apply to the chain-sync protocol."
    <> value 1000
    <> showDefault

-- | [--log-level=SEVERITY], default: Info
logLevelOption :: Parser Severity
logLevelOption = option auto $ mempty
    <> long "log-level"
    <> metavar "SEVERITY"
    <> helpDoc (Just doc)
    <> value Info
    <> showDefault
    <> completer (listCompleter severities)
  where
    severities =
        show @_ @Severity <$> [minBound .. maxBound]
    doc =
        vsep $ string <$> mconcat
            [ [ "Minimal severity required for logging." ]
            , [ separator ]
            , ("- " <>) <$> severities
            , [ separator ]
            ]

    separator :: String
    separator =
        replicate 20 '-'


-- | [--version|-v] | version
versionOption :: Parser (Command f)
versionOption =
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

--
-- Environment
--

data NetworkParameters = NetworkParameters
    { networkMagic :: !NetworkMagic
    , systemStart :: !SystemStart
    , slotsPerEpoch :: !EpochSlots
    } deriving stock (Generic, Eq, Show)
      deriving anyclass (ToJSON)

deriving newtype instance ToJSON EpochSlots
deriving newtype instance ToJSON SystemStart
deriving newtype instance ToJSON NetworkMagic

parseNetworkParameters
    :: FilePath
    -> IO NetworkParameters
parseNetworkParameters (ConfigYamlFilePath -> configFile) = runOrDie $ do
    parseConfiguration >>= \case
        NodeProtocolConfigurationCardano _ shelley _ _ -> do
            genesis <- readGenesisFile shelley
            pure NetworkParameters
                { networkMagic = NetworkMagic $ sgNetworkMagic genesis
                , systemStart = SystemStart $ sgSystemStart genesis
                , slotsPerEpoch = coerce $ sgEpochLength genesis
                }
        _ ->
            throwE "Can't use single-era configurations, only Cardano."

  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either die pure

    lastToEither :: Last a -> Either () a
    lastToEither (Last x) = maybe (Left ()) Right x

    parseConfiguration
        :: ExceptT String IO NodeProtocolConfiguration
    parseConfiguration =
        withExceptT (const "Couldn't parse cardano-node's configuration") $ do
            config <- lift $ parseNodeConfigurationFP (Just configFile)
            except $ lastToEither (pncProtocolConfig config)

    readGenesisFile
        :: NodeShelleyProtocolConfiguration
        -> ExceptT String IO (ShelleyGenesis (ShelleyEra StandardCrypto))
    readGenesisFile shelley = do
        let genesisFile = npcShelleyGenesisFile shelley
        let genesisHash = npcShelleyGenesisFileHash shelley
        withExceptT (("Couldn't parse Shelley's genesis file: " <>) . show) $
            fst <$> readGenesisAny genesisFile genesisHash

--
-- Helpers
--

mkSystemStart :: Int -> SystemStart
mkSystemStart =
    SystemStart . posixSecondsToUTCTime . toPicoResolution . toEnum
  where
    toPicoResolution = (*1000000000000)
