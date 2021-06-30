-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.HealthSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios.Data.Health
    ( NetworkSynchronization (..) )
import Test.Hspec
    ( Spec, context, parallel, shouldBe, specify )

import qualified Data.Aeson as Json

spec :: Spec
spec = parallel $ do
    context "NetworkSynchronization" $ do
        let matrix =
                [ ( NetworkSynchronization 1,        "1.00000" )
                , ( NetworkSynchronization 0.1,      "0.10000" )
                , ( NetworkSynchronization 0.01,     "0.01000" )
                , ( NetworkSynchronization 0.001,    "0.00100" )
                , ( NetworkSynchronization 0.0001,   "0.00010" )
                , ( NetworkSynchronization 0.00001,  "0.00001" )
                , ( NetworkSynchronization 0.000001, "0.00000" )
                ]
        context "is encoded as plain decimals, no scientific notation" $ do
            forM_ matrix $ \(networkSync, expectation) ->
                let title = show networkSync <> " -> " <> show expectation in
                specify title $ Json.encode networkSync `shouldBe` expectation
