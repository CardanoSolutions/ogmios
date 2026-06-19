--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Ogmios.Prelude

import Control.Exception
    ( AsyncException (..)
    )
import Ogmios
    ( Command (..)
    , InspectCommand (..)
    , application
    , healthCheck
    , inspectTransaction
    , newEnvironment
    , parseOptions
    , runWith
    , version
    , withStdoutTracers
    )
import Ogmios.Control.Exception
    ( MonadCatch (..)
    , MonadThrow (..)
    )

main :: IO ()
main = (parseOptions >>= execute) `catch` \e -> case e of
    UserInterrupt -> exitSuccess
    _             -> throwIO e
  where
    execute = \case
        Start (Identity network) opts logLevels -> do
            withStdoutTracers version logLevels $ \tr -> do
                env <- newEnvironment tr network opts
                application tr `runWith` env
        HealthCheck{healthCheckPort} ->
            healthCheck healthCheckPort
        Inspect InspectTransaction{transaction} ->
            inspectTransaction transaction
        Version -> do
            putTextLn version
