-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Path.Util
    ( getProjectRoot
    ) where

import Ogmios.Prelude

import Data.FileEmbed
    ( makeRelativeToProject
    )
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    , liftData
    )

getProjectRoot :: Q Exp
getProjectRoot =
    liftData =<< makeRelativeToProject ""
