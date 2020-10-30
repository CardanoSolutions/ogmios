--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Prelude

import Cardano.BM.Trace.Extra
    ( withStdoutTracer )
import Data.Text
    ( Text )
import Ogmios.Options
    ( Command (..), Options (..), parseOptions )
import Ogmios.Server
    ( printVersion, runServer )

import qualified Data.Text as T

main :: IO ()
main = parseOptions >>= \case
    (_, Version) -> do
        printVersion
    (env, Start opts@Options{logLevel}) ->
        withStdoutTracer "ogmios" logLevel pretty (runServer env opts)
  where
    pretty :: Show a => a -> Text
    pretty = T.pack . show
