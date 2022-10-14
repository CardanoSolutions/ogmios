-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Data.List
    ( isInfixOf
    )
import Ogmios.App.Configuration
    ( Configuration (..)
    , EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , mkSystemStart
    )
import Ogmios.Control.MonadLog
    ( Severity (..)
    , TracerDefinition (..)
    , defaultTracers
    )
import Ogmios.Options
    ( Command (..)
    , Tracers (..)
    , parseNetworkParameters
    , parseOptions
    , parseOptionsPure
    )
import System.Environment
    ( withArgs
    )
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
    ( getProjectRoot
    )

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
            HealthCheck{} -> expectationFailure "Expected Version but got HealthCheck."

        let args =
                [ "--node-socket", "./node.socket"
                , "--node-config", getConfigFile "testnet"
                ]
        specify (show args) $ withArgs args parseOptions >>= \case
            Start (Identity _) opts logLevels -> do
                nodeSocket opts `shouldBe` "./node.socket"
                nodeConfig opts `shouldBe` getConfigFile "testnet"
                serverHost opts `shouldBe` "127.0.0.1"
                serverPort opts `shouldBe` 1337
                connectionTimeout opts `shouldBe` 90
                maxInFlight opts `shouldBe` 1000
                logLevels `shouldBe` defaultTracersInfo
            Version -> expectationFailure "Expected Start but got Version."
            HealthCheck{} -> expectationFailure "Expected Start but got HealthCheck."

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

        specify "vasil-dev" $ do
            params <- parseNetworkParameters (getConfigFile "vasil-dev")
            networkMagic  params `shouldBe` NetworkMagic 9
            systemStart   params `shouldBe` mkSystemStart 1659636000
            slotsPerEpoch params `shouldBe` EpochSlots 360

        specify "preview" $ do
            params <- parseNetworkParameters (getConfigFile "preview")
            networkMagic  params `shouldBe` NetworkMagic 2
            systemStart   params `shouldBe` mkSystemStart 1660003200
            slotsPerEpoch params `shouldBe` EpochSlots 4320

        specify "preprod" $ do
            params <- parseNetworkParameters (getConfigFile "preprod")
            networkMagic  params `shouldBe` NetworkMagic 1
            systemStart   params `shouldBe` mkSystemStart 1654041600
            slotsPerEpoch params `shouldBe` EpochSlots 21600
  where
    matrix =
        [ ( [], shouldFail )
        , ( [ "--node-socket", "./node.socket" ], shouldFail )
        , ( [ "--node-config", "./node.config" ], shouldFail )

        , ( [ "--node-socket", "/path/to/socket"
            , "--node-config", "./node.config"
            ]
          , shouldSucceed
                (defaultConfiguration { nodeSocket = "/path/to/socket" })
                defaultTracersInfo
          )

        , ( [ "--node-socket", "./node.socket"
            , "--node-config", "/path/to/config"
            ]
          , shouldSucceed
                (defaultConfiguration { nodeConfig = "/path/to/config" })
                defaultTracersInfo
          )

        , ( defaultArgs ++ [ "--host", "0.0.0.0" ]
          , shouldSucceed
                (defaultConfiguration { serverHost = "0.0.0.0" })
                defaultTracersInfo
          )

        , ( defaultArgs ++ [ "--port", "42" ]
          , shouldSucceed
                (defaultConfiguration { serverPort = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--port", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--timeout", "42" ]
          , shouldSucceed
                (defaultConfiguration { connectionTimeout = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--timeout", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--max-in-flight", "42" ]
          , shouldSucceed
                (defaultConfiguration { maxInFlight = 42 })
                defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--max-in-flight", "#" ]
          , shouldFail
          )

        , ( defaultArgs ++ [ "--log-level", "Debug" ]
          , shouldSucceed defaultConfiguration (defaultTracersDebug)
          )
        , ( defaultArgs ++ [ "--log-level", "debug" ]
          , shouldSucceed defaultConfiguration (defaultTracersDebug)
          )
        , ( defaultArgs ++ [ "--log-level", "Info" ]
          , shouldSucceed defaultConfiguration defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--log-level", "info" ]
          , shouldSucceed defaultConfiguration defaultTracersInfo
          )
        , ( defaultArgs ++ [ "--log-level", "Notice" ]
          , shouldSucceed defaultConfiguration (defaultTracersNotice)
          )
        , ( defaultArgs ++ [ "--log-level", "notice" ]
          , shouldSucceed defaultConfiguration (defaultTracersNotice)
          )
        , ( defaultArgs ++ [ "--log-level", "Warning" ]
          , shouldSucceed defaultConfiguration (defaultTracersWarning)
          )
        , ( defaultArgs ++ [ "--log-level", "warning" ]
          , shouldSucceed defaultConfiguration (defaultTracersWarning)
          )
        , ( defaultArgs ++ [ "--log-level", "Error" ]
          , shouldSucceed defaultConfiguration (defaultTracersError)
          )
        , ( defaultArgs ++ [ "--log-level", "error" ]
          , shouldSucceed defaultConfiguration (defaultTracersError)
          )

        , ( defaultArgs ++ [ "--log-level-health", "Notice" ]
          , shouldSucceed
                defaultConfiguration
                (defaultTracersInfo { tracerHealth = Const (Just Notice) })
          )

        , ( defaultArgs ++
                [ "--log-level-metrics", "Debug"
                , "--log-level-websocket", "Warning"
                ]
          , shouldSucceed
                defaultConfiguration
                (defaultTracersInfo
                    { tracerMetrics = Const (Just Debug)
                    , tracerWebSocket = Const (Just Warning)
                    }
                )
          )

        , ( defaultArgs ++
                [ "--log-level", "Error"
                , "--log-level-health", "Debug"
                ]
          , shouldFail
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

defaultConfiguration :: Configuration
defaultConfiguration = parseOptionsPure defaultArgs
    & either (error . toText) (\(Start _ cfg _) -> cfg)

defaultTracersDebug :: Tracers IO 'MinSeverities
defaultTracersDebug = defaultTracers (Just Debug)

defaultTracersInfo :: Tracers IO 'MinSeverities
defaultTracersInfo = defaultTracers (Just Info)

defaultTracersNotice :: Tracers IO 'MinSeverities
defaultTracersNotice = defaultTracers (Just Notice)

defaultTracersWarning :: Tracers IO 'MinSeverities
defaultTracersWarning = defaultTracers (Just Warning)

defaultTracersError :: Tracers IO 'MinSeverities
defaultTracersError = defaultTracers (Just Error)

defaultArgs :: [String]
defaultArgs =
    [ "--node-socket", "./node.socket"
    , "--node-config", "./node.config"
    ]

shouldSucceed
    :: Configuration
    -> Tracers IO 'MinSeverities
    -> (Either String (Command Proxy) -> Expectation)
shouldSucceed cfg =
    flip shouldBe . Right . Start Proxy cfg

shouldFail :: (Either String (Command Proxy)) -> Expectation
shouldFail = flip shouldSatisfy isLeft

isLeftWith :: (err -> Bool) -> Either err result -> Bool
isLeftWith predicate = \case
    Left e -> predicate e
    Right{} -> False

getConfigFile :: String -> FilePath
getConfigFile network =
    $(getProjectRoot) <> "/config/network/" <> network <> "/cardano-node/config.json"
