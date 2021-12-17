-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import Prelude
import Test.Hspec
import qualified Test.Hspec.Json.SchemaSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Test.Hspec.Json.Schema" Test.Hspec.Json.SchemaSpec.spec
