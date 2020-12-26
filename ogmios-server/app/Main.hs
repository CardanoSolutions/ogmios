--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Prelude

import Ogmios
    ( Command (..)
    , Options (..)
    , application
    , newEnvironment
    , parseOptions
    , runWith
    , version
    , withStdoutTracer
    )

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = parseOptions >>= \case
    (_, Version) -> do
        TIO.putStrLn version
    (network, Start opts@Options{logLevel}) -> do
        withStdoutTracer "ogmios" logLevel (T.pack . show) $ \tr -> do
            env <- newEnvironment tr network opts
            application tr `runWith` env
