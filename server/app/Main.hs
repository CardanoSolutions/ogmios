--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Ogmios.Prelude

import Ogmios
    ( Command (..)
    , application
    , healthCheck
    , newEnvironment
    , parseOptions
    , runWith
    , version
    , withStdoutTracers
    )

main :: IO ()
main = parseOptions >>= \case
    Start (Identity network) opts logLevels -> do
        withStdoutTracers version logLevels $ \tr -> do
            env <- newEnvironment tr network opts
            application tr `runWith` env
    HealthCheck{healthCheckPort} ->
        healthCheck healthCheckPort
    Version -> do
        putTextLn version
