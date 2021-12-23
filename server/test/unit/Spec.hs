-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Main where

import qualified Ogmios.App.ProtocolSpec
import qualified Ogmios.App.Protocol.ChainSyncSpec
import qualified Ogmios.App.Protocol.StateQuerySpec
import qualified Ogmios.Data.HealthSpec
import qualified Ogmios.Data.JsonSpec
import qualified Ogmios.Data.MetricsSpec
import qualified Ogmios.OptionsSpec
import Prelude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe
    "Ogmios.App.Protocol.ChainSync"
    Ogmios.App.Protocol.ChainSyncSpec.spec
  describe "Ogmios.App.Protocol" Ogmios.App.ProtocolSpec.spec
  describe
    "Ogmios.App.Protocol.StateQuery"
    Ogmios.App.Protocol.StateQuerySpec.spec
  describe "Ogmios.Data.Health" Ogmios.Data.HealthSpec.spec
  describe "Ogmios.Data.Json" Ogmios.Data.JsonSpec.spec
  describe "Ogmios.Data.Metrics" Ogmios.Data.MetricsSpec.spec
  describe "Ogmios.Options" Ogmios.OptionsSpec.spec
