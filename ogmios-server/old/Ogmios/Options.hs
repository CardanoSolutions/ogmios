--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Options
    ( Options (..)
    , Command (..)

    -- * Applicative Options Parser
    , parseOptions
    ) where

import Prelude

import Options.Applicative

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import Safe
    ( readMay )
import System.Environment
    ( lookupEnv )
import System.Exit
    ( exitFailure )

data Command
    = Start Options
    | Version

data Options = Options
    { nodeSocket :: FilePath
    , host :: String
    , port :: Int
    , logLevel :: Severity
    }

parseOptions :: IO ((NodeToClientVersionData, EpochSlots), Command)
parseOptions = (,)
    <$> lookupVersionData
    <*> customExecParser (prefs showHelpOnEmpty) parserInfo

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Ogmios - A JSON-WSP WebSocket adaptor for cardano-node"
    <> header (unwords
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
        , indent 27 $ string "- <custom:INT>"
        , indent 27 $ string $ separator <> " (default: mainnet)"
        ])
  where
    parser =
        versionOption
        <|>
        (Start <$>
            (Options
                <$> nodeSocketOption
                <*> hostOption
                <*> portOption
                <*> logLevelOption
            )
        )

--  _____       _   _
-- |  _  |     | | (_)
-- | | | |_ __ | |_ _  ___  _ __  ___
-- | | | | '_ \| __| |/ _ \| '_ \/ __|
-- \ \_/ / |_) | |_| | (_) | | | \__ \
--  \___/| .__/ \__|_|\___/|_| |_|___/
--       | |
--       |_|

-- | --node-socket=FILEPATH
nodeSocketOption :: Parser FilePath
nodeSocketOption = option str $ mempty
    <> long "node-socket"
    <> metavar "FILEPATH"
    <> help "Path to the node socket."

-- | [--host=IPv4], default: 127.0.0.1
hostOption :: Parser String
hostOption = option str $ mempty
    <> long "host"
    <> metavar "IPv4"
    <> help "Address to bind to."
    <> value "127.0.0.1"
    <> showDefault

-- | [--port=TCP/PORT], default: 1337
portOption :: Parser Int
portOption = option auto $ mempty
    <> long "port"
    <> metavar "TCP/PORT"
    <> help "Port to listen on."
    <> value 1337
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
        , ("- " <>) . show @Severity <$> [minBound .. maxBound]
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

separator :: String
separator = replicate 20 '-'


--  _____           _                                      _
-- |  ___|         (_)                                    | |
-- | |__ _ ____   ___ _ __ ___  _ __  _ __ ___   ___ _ __ | |_
-- |  __| '_ \ \ / / | '__/ _ \| '_ \| '_ ` _ \ / _ \ '_ \| __|
-- | |__| | | \ V /| | | | (_) | | | | | | | | |  __/ | | | |_
-- \____/_| |_|\_/ |_|_|  \___/|_| |_|_| |_| |_|\___|_| |_|\__|

envOgmiosNetwork :: String
envOgmiosNetwork = "OGMIOS_NETWORK"

-- | Lookup environment for a given version data name, default to mainnet.
lookupVersionData
    :: IO (NodeToClientVersionData, EpochSlots)
lookupVersionData = do
    lookupEnv envOgmiosNetwork >>= \case
        Nothing -> do
            pure (mainnetVersionData, defaultEpochSlots)
        Just "mainnet" -> do
            pure (mainnetVersionData, defaultEpochSlots)
        Just "testnet" -> do
            pure (testnetVersionData, defaultEpochSlots)
        Just "staging" -> do
            pure (stagingVersionData, defaultEpochSlots)
        Just custom ->
            case readMay custom of
                Just n -> do
                    let magic = NetworkMagic n
                    pure (customVersionData magic, defaultEpochSlots)
                Nothing -> do
                    exitFailure

-- Hard-coded mainnet version data
mainnetVersionData
    :: NodeToClientVersionData
mainnetVersionData =
    customVersionData (NetworkMagic 764824073)

-- Hard-coded testnet version data
testnetVersionData
    :: NodeToClientVersionData
testnetVersionData =
    customVersionData (NetworkMagic 1097911063)

-- Hard-coded staging version data
stagingVersionData
    :: NodeToClientVersionData
stagingVersionData =
    customVersionData (NetworkMagic 633343913)

-- A custom / unknown version data
customVersionData
    :: NetworkMagic
    -> NodeToClientVersionData
customVersionData networkMagic =
    ( NodeToClientVersionData { networkMagic } )

-- Hard-coded genesis slots per epoch
defaultEpochSlots
    :: EpochSlots
defaultEpochSlots =
    EpochSlots 21600
