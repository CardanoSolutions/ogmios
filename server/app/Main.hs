--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Ogmios.Prelude

import Ogmios
    ( Command (..)
    , Options (..)
    , application
    , newEnvironment
    , parseOptions
    , runWith
    , withStdoutTracer
    )
import Ogmios.Version
    ( revision, shortVersion, version )

main :: IO ()
main = parseOptions >>= \case
    (_, Version) -> do
        putTextLn version
    (network, Start opts@Options{logLevel}) -> do
        withStdoutTracer "ogmios" (shortVersion, revision) logLevel $ \tr -> do
            env <- newEnvironment tr network opts
            application tr `runWith` env
