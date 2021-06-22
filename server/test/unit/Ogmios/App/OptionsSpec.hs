-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- Used to partially pattern match result of parsing default arguments. Okay-ish
-- because it's test code and, having it fail would be instantly caught.
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ogmios.App.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios.App.Options
    ( Command (..)
    , EpochSlots (..)
    , NetworkMagic (..)
    , NetworkParameters (..)
    , Options (..)
    , Severity (..)
    , mkSystemStart
    , parseNetworkParameters
    , parseOptionsPure
    )
import Test.Hspec
    ( Expectation, Spec, context, parallel, shouldBe, shouldSatisfy, specify )

import qualified Paths_ogmios

spec :: Spec
spec = parallel $ do
    context "parseOptions(Pure)" $ do
        forM_ matrix $ \(args, expect) -> do
            let title = toString $ unwords $ toText <$> args
            specify title $ expect (parseOptionsPure args)

    context "parseNetworkParameters" $ do
        specify "mainnet" $ do
            params <- parseNetworkParameters =<< getConfigFile "mainnet"
            networkMagic  params `shouldBe` NetworkMagic 764824073
            systemStart   params `shouldBe` mkSystemStart 1506203091
            slotsPerEpoch params `shouldBe` EpochSlots 432000

        specify "testnet" $ do
            params <- parseNetworkParameters =<< getConfigFile "testnet"
            networkMagic  params `shouldBe` NetworkMagic 1097911063
            systemStart   params `shouldBe` mkSystemStart 1563999616
            slotsPerEpoch params `shouldBe` EpochSlots 432000
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

        , ( [ "--help" ], flip shouldSatisfy isLeft  )
        , ( [ "-h" ], flip shouldSatisfy isLeft )
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

shouldFail :: (Either String (Command Proxy)) -> Expectation
shouldFail = flip shouldSatisfy isLeft

getConfigFile :: String -> IO FilePath
getConfigFile network =
    "config/network/" <> network <> "/cardano-node/config.json"
    & Paths_ogmios.getDataFileName
