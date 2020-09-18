--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Ogmios.Options.Applicative
    ( run
    , Options (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Data.Git.Revision.TH
    ( gitRemoteGetURL, gitRevParseHEAD )
import Data.Version
    ( showVersion )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import Paths_ogmios
    ( version )

import Options.Applicative hiding
    ( action )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Command
    = Start Options
    | Version

data Options = Options
    { nodeSocket :: FilePath
    , host :: String
    , port :: Int
    , logLevel :: Severity
    }

run :: (Options -> IO ()) -> IO ()
run action = do
    parseOptions >>= \case
        Version -> do
            B8.putStrLn $ T.encodeUtf8 $ T.pack $ unlines
                [ $(gitRemoteGetURL) <> "@" <> $(gitRevParseHEAD)
                , showVersion version
                ]
        Start opts -> do
            action opts
  where
    parseOptions :: IO Command
    parseOptions =
        customExecParser preferences parserInfo
      where
        preferences = prefs showHelpOnEmpty

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
        , indent 2 $ string "OGMIOS_NETWORK           Configure the target network."
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
