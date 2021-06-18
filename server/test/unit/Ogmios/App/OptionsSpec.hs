-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.App.OptionsSpec
    ( spec
    ) where

import Ogmios.Prelude

import Ogmios.App.Options
    ( NetworkParameters (..) )

import Test.Hspec
    ( Spec, context, parallel )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , elements
    , forAll
    , property
    , (===)
    )

spec :: Spec
spec = parallel $ do
    context "parseNetworkParameters" $ do
        prop "TODO" (property True)
