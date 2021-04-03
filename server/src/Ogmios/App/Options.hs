--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

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
    , NetworkParameters (..)
    , NetworkMagic (..)
    , EpochSlots (..)
    , envOgmiosNetwork
    , lookupNetworkMagic
    ) where

import Relude

import Ogmios.Control.MonadLog
    ( Severity (..) )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Safe
    ( readMay )
import System.Environment
    ( lookupEnv )

import Options.Applicative

--
-- Command-line commands
--

data Command
    = Start Options
    | Version

parseOptions :: IO (NetworkParameters, Command)
parseOptions = (,)
    <$> lookupNetworkMagic
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
        , indent 27 $ string "- <INT>"
        , indent 27 $ string $ separator <> " (default: mainnet)"
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
    , logLevel :: !Severity
    } deriving (Generic, Eq, Show)

-- | --node-socket=FILEPATH
nodeSocketOption :: Parser FilePath
nodeSocketOption = option str $ mempty
    <> long "node-socket"
    <> metavar "FILEPATH"
    <> help "Path to the node socket."

-- | [--host=IPv4], default: 127.0.0.1
serverHostOption :: Parser String
serverHostOption = option str $ mempty
    <> long "host"
    <> metavar "IPv4"
    <> help "Address to bind to."
    <> value "127.0.0.1"
    <> showDefault

-- | [--port=TCP/PORT], default: 1337
serverPortOption :: Parser Int
serverPortOption = option auto $ mempty
    <> long "port"
    <> metavar "TCP/PORT"
    <> help "Port to listen on."
    <> value 1337
    <> showDefault

-- | [--websocket-timeout], default: 90s
connectionTimeoutOption :: Parser Int
connectionTimeoutOption = option auto $ mempty
    <> long "timeout"
    <> metavar "SECONDS"
    <> help "Number of seconds of inactivity after which the server should close client connections."
    <> value 90
    <> showDefault

-- | [--log-level=SEVERITY], default: Info
logLevelOption :: Parser Severity
logLevelOption = option auto $ mempty
    <> long "log-level"
    <> metavar "SEVERITY"
    <> helpDoc (Just (vsep (string <$> severities)))
    <> value Info
    <> showDefault
  where
    severities = mconcat
        [ [ "Minimal severity required for logging." ]
        , [ separator ]
        , ("- " <>) . show @_ @Severity <$> [minBound .. maxBound]
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
    { slotsPerEpoch :: !EpochSlots
    , networkMagic :: !NetworkMagic
    } deriving (Generic, Eq, Show)

envOgmiosNetwork :: String
envOgmiosNetwork = "OGMIOS_NETWORK"

-- | Lookup environment for a given version data name, default to mainnet.
lookupNetworkMagic
    :: IO NetworkParameters
lookupNetworkMagic = do
    lookupEnv envOgmiosNetwork >>= \case
        Nothing -> do
            pure $ NetworkParameters defaultSlotsPerEpoch mainnetNetworkMagic
        Just "mainnet" -> do
            pure $ NetworkParameters defaultSlotsPerEpoch mainnetNetworkMagic
        Just "testnet" -> do
            pure $ NetworkParameters defaultSlotsPerEpoch testnetNetworkMagic
        Just "staging" -> do
            pure $ NetworkParameters defaultSlotsPerEpoch stagingNetworkMagic
        Just custom ->
            case readMay custom of
                Just n -> do
                    let magic = NetworkMagic n
                    pure $ NetworkParameters defaultSlotsPerEpoch magic
                Nothing -> do
                    exitFailure

-- Hard-coded mainnet version data
mainnetNetworkMagic
    :: NetworkMagic
mainnetNetworkMagic =
    NetworkMagic 764824073

-- Hard-coded testnet version data
testnetNetworkMagic
    :: NetworkMagic
testnetNetworkMagic =
    NetworkMagic 1097911063

-- Hard-coded staging version data
stagingNetworkMagic
    :: NetworkMagic
stagingNetworkMagic =
    NetworkMagic 633343913

-- Hard-coded genesis slots per epoch
defaultSlotsPerEpoch
    :: EpochSlots
defaultSlotsPerEpoch =
    EpochSlots 21600

--
-- Helpers
--

separator :: String
separator = replicate 20 '-'
