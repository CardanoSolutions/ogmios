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
    , serverHostOption
    , serverPortOption
    , connectionTimeoutOption
    , logLevelOption

      -- ** Environment
      -- *** ENV Var
    , envOgmiosNetwork

      -- *** Types
    , NetworkParameters (..)
    , mainnetNetworkParameters
    , testnetNetworkParameters
    , stagingNetworkParameters

    , NetworkMagic (..)
    , EpochSlots (..)
    , defaultSlotsPerEpoch
    , SystemStart (..)
    , mkSystemStart

      -- *** Parsing 'NetworkParameters'
    , lookupNetworkParameters
    , parseNetworkParameters
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadLog
    ( Severity (..) )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Data.Aeson
    ( ToJSON )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( SystemStart (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Safe
    ( readMay )
import System.Environment
    ( lookupEnv )

import Options.Applicative

import qualified Data.Text as T

--
-- Command-line commands
--

data Command
    = Start Options
    | Version

parseOptions :: IO (NetworkParameters, Command)
parseOptions = (,)
    <$> lookupNetworkParameters
    <*> customExecParser (prefs showHelpOnEmpty) parserInfo

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Ogmios - A JSON-WSP WebSocket adaptor for cardano-node"
    <> header (toString $ unwords
        [ "Provides a bridge between cardano-node and WebSocket clients."
        , "Ogmios translates the existing CBOR-based Ouroboros mini-protocols"
        , "into JSON-WSP-based protocols, through WebSocket channels."
        ])
    <> footerDoc (Just $ vsep
        [ string "Available ENV variables:"
        , indent 2 $ string $ envOgmiosNetwork <> "           Configure the target network."
        , indent 27 $ string separator
        , indent 27 $ string "- mainnet"
        , indent 27 $ string "- testnet"
        , indent 27 $ string "- staging"
        , indent 27 $ string "- <MAGIC>:<SYSTEM-START>[:<SLOTS-PER-EPOCH]"
        , indent 27 $ string $ separator <> " (default: mainnet)"
        , string ""
        , string "Examples:"
        , indent 2 $ string "Connecting to the mainnet:"
        , indent 4 $ string "$ ogmios --node-socket /path/to/node.socket"
        , string ""
        , indent 2 $ string "Connecting to the testnet:"
        , indent 4 $ string "$ OGMIOS_NETWORK=testnet ogmios --node-socket /path/to/node.socket"
        , string ""
        , indent 2 $ string "Connecting to the testnet using explicit parameters:"
        , indent 4 $ string "$ OGMIOS_NETWORK=1097911063:1563999616 ogmios --node-socket /path/to/node.socket"
        , string ""
        , indent 2 $ string "Connecting to the Guild network:"
        , indent 4 $ string "$ OGMIOS_NETWORK=141:1612317107:3600 ogmios --node-socket /path/to/node.socket"
        ])
  where
    parser =
        versionOption
        <|>
        (Start <$>
            (Options
                <$> nodeSocketOption
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

-- | [--version|-v] | version
versionOption :: Parser Command
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

envOgmiosNetwork :: String
envOgmiosNetwork = "OGMIOS_NETWORK"

-- | Lookup environment for a given version data name, default to mainnet.
lookupNetworkParameters
    :: IO NetworkParameters
lookupNetworkParameters = do
    mStr <- lookupEnv envOgmiosNetwork
    let params = maybe (Just mainnetNetworkParameters) parseNetworkParameters mStr
    maybe (die err) pure params
  where
    err = "Couldn't parse " <> envOgmiosNetwork <> ". Have a look at the usage using '--help'."

-- | Pure parser for 'NetworkParameters'
parseNetworkParameters
    :: String
    -> Maybe NetworkParameters
parseNetworkParameters = \case
    "mainnet" -> do
        pure mainnetNetworkParameters
    "testnet" -> do
        pure testnetNetworkParameters
    "staging" -> do
        pure stagingNetworkParameters
    custom -> do
        let strs = toString <$> T.splitOn ":" (toText custom)
        case strs of
            [magicStr, systemStartStr] -> NetworkParameters
                <$> fmap NetworkMagic (readMay magicStr)
                <*> fmap SystemStart (readAsPosixTime systemStartStr)
                <*> pure defaultSlotsPerEpoch
            [magicStr, systemStartStr, slotStr] -> NetworkParameters
                <$> fmap NetworkMagic (readMay magicStr)
                <*> fmap SystemStart (readAsPosixTime systemStartStr)
                <*> fmap EpochSlots (readMay slotStr)
            _ ->
                Nothing

-- Hard-coded mainnet network parameters
mainnetNetworkParameters
    :: NetworkParameters
mainnetNetworkParameters =
    NetworkParameters
        { networkMagic = NetworkMagic 764824073
        , systemStart = SystemStart $ posixSecondsToUTCTime 1506203091
        , slotsPerEpoch = defaultSlotsPerEpoch
        }

-- Hard-coded testnet network parameters
testnetNetworkParameters
    :: NetworkParameters
testnetNetworkParameters =
    NetworkParameters
        { networkMagic = NetworkMagic 1097911063
        , systemStart = SystemStart $ posixSecondsToUTCTime 1563999616
        , slotsPerEpoch = defaultSlotsPerEpoch
        }

-- Hard-coded staging network parameters
stagingNetworkParameters
    :: NetworkParameters
stagingNetworkParameters =
    NetworkParameters
        { networkMagic = NetworkMagic 633343913
        , systemStart = SystemStart $ posixSecondsToUTCTime 1506450213
        , slotsPerEpoch = defaultSlotsPerEpoch
        }

-- Hard-coded genesis slots per epoch
defaultSlotsPerEpoch
    :: EpochSlots
defaultSlotsPerEpoch =
    EpochSlots 21600

--
-- Helpers
--

separator :: String
separator =
    replicate 20 '-'

readAsPosixTime :: String -> Maybe UTCTime
readAsPosixTime =
    fmap posixSecondsToUTCTime . readMay . (<> "s")

mkSystemStart :: Int -> SystemStart
mkSystemStart =
    SystemStart . posixSecondsToUTCTime . toPicoResolution . toEnum
  where
    toPicoResolution = (*1000000000000)
