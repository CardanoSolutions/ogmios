-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TemplateHaskell #-}

module Ogmios.App.ConfigurationSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios.App.Configuration
    ( readAlonzoGenesis
    , readByronGenesis
    , readConwayGenesis
    , readShelleyGenesis
    )
import Test.Hspec
    ( Expectation
    , Spec
    , context
    , expectationFailure
    , parallel
    , specify
    )
import Test.Path.Util
    ( getProjectRoot
    )

spec :: Spec
spec = parallel $
    context "Genesis configuration" $
        forM_ networks $ \network -> do
            context network $
                forM_ genesisReaders $ \(name, readGenesis) ->
                    specify ("reads " <> name <> " genesis") $
                        readGenesis (getConfigFile network)

networks :: [String]
networks =
    [ "mainnet"
    , "preprod"
    , "preview"
    ]

genesisReaders :: [(String, FilePath -> Expectation)]
genesisReaders =
    [ ("Byron", void . readByronGenesis)
    , ("Shelley", void . readShelleyGenesis)
    , ("Alonzo", void . readAlonzoGenesis)
    , ("Conway", expectRight <=< readConwayGenesis)
    ]
  where
    expectRight = \case
        Left err -> expectationFailure (toString err)
        Right{} -> pure ()

getConfigFile :: String -> FilePath
getConfigFile network =
    $(getProjectRoot) <> "/config/network/" <> network <> "/cardano-node/config.json"
