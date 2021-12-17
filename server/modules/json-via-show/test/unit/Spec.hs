-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.
module Main where

import qualified Data.Aeson.Via.ShowSpec
import Prelude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.Aeson.Via.Show" Data.Aeson.Via.ShowSpec.spec
