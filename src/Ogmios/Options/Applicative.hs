--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Ogmios.Options.Applicative
    ( Options (..)
    , parseOptions
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Data.Git.Revision.TH
    ( gitRevParseHEAD )
import Options.Applicative.Help.Pretty
    ( hardline, indent, string, vsep )

import Options.Applicative


data Options = Options
    { nodeSocket :: FilePath
    , host :: String
    , port :: Int
    , publicUrl :: Maybe String
    , logLevel :: Severity
    }

parseOptions :: IO Options
parseOptions =
    customExecParser preferences parserInfo
  where
    preferences = prefs showHelpOnEmpty

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parser) $ mempty
    <> progDesc "Ogmios - A JSON-WSP WebSocket adaptor for cardano-node"
    <> header (unwords
        [ "Provides a bridge between cardano-node and WebSocket clients."
        , "Ogmios translates the existing CBOR-based Ouroboros mini-protocols"
        , "into JSON-WSP-based protocols, through WebSocket channels."
        ])
    <> footerDoc (Just $ vsep
        [ string "Additional options (ENV variables):"
        , indent 2 $ string "OGMIOS_NETWORK           Configure target network. (default: \"mainnet\")."
        , hardline
        , string "Revision:"
        , indent 2 $ string $ "git@" <> $(gitRevParseHEAD)
        ])
  where
    parser = Options
        <$> nodeSocketOption
        <*> hostOption
        <*> portOption
        <*> urlOption
        <*> logLevelOption

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

-- | [--public-url=URI]
urlOption :: Parser (Maybe String)
urlOption = optional $ option str $ mempty
    <> long "public-url"
    <> metavar "URI"
    <> help "Public URL, if any, from which Ogmios is accessible when hosted."

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
        , [ replicate 20 '-' ]
        , ("- " <>) . show @Severity <$> [minBound .. maxBound]
        , [ replicate 20 '-' ]
        ]
