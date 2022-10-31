-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Instances.Util
    ( eqShowJson
    ) where

import Ogmios.Prelude

import Data.Aeson
    ( ToJSON (..)
    )
import Test.Hspec
    ( Expectation
    , SpecWith
    , shouldBe
    , specify
    )

-- | Non-interesting test meant to tick coverage for Eq, Show and JSON
-- instances, as well as record-fields.
eqShowJson :: (Eq a, Show a, ToJSON a) => String -> a -> (a -> Expectation) -> SpecWith ()
eqShowJson lbl a predicate =
    specify (lbl <> " / " <> show a <> " / " <> show (toEncoding a)) $
        predicate a >> shouldBe a a
