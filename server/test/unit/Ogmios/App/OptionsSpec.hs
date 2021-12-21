-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.App.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Data.List
    ( isInfixOf )
import Ogmios.App.Options
    ( Command (..)
    , EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , Options (..)
    , Severity (..)
    , mkSystemStart
    , parseNetworkParameters
    , parseOptions
    , parseOptionsPure
    )
import System.Environment
    ( withArgs )
import Test.Hspec
    ( Expectation
    , Spec
    , context
    , expectationFailure
    , parallel
    , shouldBe
    , shouldSatisfy
    , specify
    )
import Test.Path.Util
    ( getProjectRoot )

spec :: Spec
spec = parallel $ do
    context "parseOptions(Pure)" $ do
        forM_ matrix $ \(args, expect) -> do
            let title = toString $ unwords $ toText <$> args
            specify title $ expect (parseOptionsPure args)

        specify "invalid" $ do
            case parseOptionsPure ["--nope"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"

        specify "test completion" $ do
            case parseOptionsPure ["--node-so\t"] of
                Right{} -> expectationFailure "Expected error but got success."
                Left e -> e `shouldSatisfy` isInfixOf "Invalid option"

    context "parseOptions(IO)" $ do
        specify "--version" $ withArgs ["--version"] parseOptions >>= \case
            Version -> pure ()
            Start{} -> expectationFailure "Expected Version but got Start."

        let args =
                [ "--node-socket", "./node.socket"
                , "--node-config", getConfigFile "testnet"
                ]
        specify (show args) $ withArgs args parseOptions >>= \case
            Start (Identity _) opts -> do
                nodeSocket opts `shouldBe` "./node.socket"
                nodeConfig opts `shouldBe` getConfigFile "testnet"
                serverHost opts `shouldBe` "127.0.0.1"
                serverPort opts `shouldBe` 1337
                connectionTimeout opts `shouldBe` 90
                maxInFlight opts `shouldBe` 1000
                logLevel opts `shouldBe` Info
            Version -> expectationFailure "Expected Start but got Version."

    context "parseNetworkParameters" $ do
        specify "mainnet" $ do
            params <- parseNetworkParameters (getConfigFile "mainnet")
            networkMagic  params `shouldBe` NetworkMagic 764824073
            systemStart   params `shouldBe` mkSystemStart 1506203091
            slotsPerEpoch params `shouldBe` EpochSlots 21600

        specify "testnet" $ do
            params <- parseNetworkParameters (getConfigFile "testnet")
            networkMagic  params `shouldBe` NetworkMagic 1097911063
            systemStart   params `shouldBe` mkSystemStart 1563999616
            slotsPerEpoch params `shouldBe` EpochSlots 21600

        specify "alonzo-white" $ do
            params <- parseNetworkParameters (getConfigFile "alonzo-white")
            networkMagic  params `shouldBe` NetworkMagic 7
            systemStart   params `shouldBe` mkSystemStart 1625593493
            slotsPerEpoch params `shouldBe` EpochSlots 360
  where
    matrix =
        [ ( [], shouldFail )
        , ( [ "--node-socket", "./node.socket" ], shouldFail )
        , ( [ "--node-config", "./node.config" ], shouldFail )

        , ( [ "--node-socket", "/path/to/socket"
            , "--node-config", "./node.config"
            ]
          , shouldSucceed defaultOptions { nodeSocket = "/path/to/socket" }
          )

        , ( [ "--node-socket", "./node.socket"
            , "--node-config", "/path/to/config"
            ]
          , shouldSucceed defaultOptions { nodeConfig = "/path/to/config" }
          )

        , ( defaultArgs ++ [ "--host", "0.0.0.0" ]
          , shouldSucceed defaultOptions { serverHost = "0.0.0.0" }
          )

        , ( defaultArgs ++ [ "--port", "42" ]
          , shouldSucceed defaultOptions { serverPort = 42 }
          )
        , ( defaultArgs ++ [ "--port", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--timeout", "42" ]
          , shouldSucceed defaultOptions { connectionTimeout = 42 }
          )
        , ( defaultArgs ++ [ "--timeout", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--max-in-flight", "42" ]
          , shouldSucceed defaultOptions { maxInFlight = 42 }
          )
        , ( defaultArgs ++ [ "--max-in-flight", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--log-level", "Debug" ]
          , shouldSucceed defaultOptions { logLevel = Debug }
          )
        , ( defaultArgs ++ [ "--log-level", "debug" ]
          , shouldSucceed defaultOptions { logLevel = Debug }
          )
        , ( defaultArgs ++ [ "--log-level", "Info" ]
          , shouldSucceed defaultOptions { logLevel = Info }
          )
        , ( defaultArgs ++ [ "--log-level", "info" ]
          , shouldSucceed defaultOptions { logLevel = Info }
          )
        , ( defaultArgs ++ [ "--log-level", "Notice" ]
          , shouldSucceed defaultOptions { logLevel = Notice }
          )
        , ( defaultArgs ++ [ "--log-level", "notice" ]
          , shouldSucceed defaultOptions { logLevel = Notice }
          )
        , ( defaultArgs ++ [ "--log-level", "Warning" ]
          , shouldSucceed defaultOptions { logLevel = Warning }
          )
        , ( defaultArgs ++ [ "--log-level", "warning" ]
          , shouldSucceed defaultOptions { logLevel = Warning }
          )
        , ( defaultArgs ++ [ "--log-level", "Error" ]
          , shouldSucceed defaultOptions { logLevel = Error }
          )
        , ( defaultArgs ++ [ "--log-level", "error" ]
          , shouldSucceed defaultOptions { logLevel = Error }
          )

        , ( [ "version" ], flip shouldBe $ Right Version )
        , ( [ "-v" ], flip shouldBe $ Right Version )
        , ( [ "--version" ], flip shouldBe $ Right Version )

        , ( [ "--help" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        , ( [ "-h" ]
          , flip shouldSatisfy $ isLeftWith $ \help ->
            help `deepseq` ("Usage:" `isInfixOf` help)
          )
        ]

--
-- Helper
--

defaultOptions :: Options
defaultOptions = parseOptionsPure defaultArgs
    & either (error . toText) (\(Start _ opts) -> opts)

defaultArgs :: [String]
defaultArgs =
    [ "--node-socket", "./node.socket"
    , "--node-config", "./node.config"
    ]

shouldSucceed :: Options -> (Either String (Command Proxy) -> Expectation)
shouldSucceed =
    flip shouldBe . Right . Start Proxy

shouldFail :: Either String (Command Proxy) -> Expectation
shouldFail = flip shouldSatisfy isLeft

isLeftWith :: (err -> Bool) -> Either err result -> Bool
isLeftWith predicate = \case
    Left e -> predicate e
    Right{} -> False

getConfigFile :: String -> FilePath
getConfigFile network =
    $(getProjectRoot) <> "/config/network/" <> network <> "/cardano-node/config.json"
