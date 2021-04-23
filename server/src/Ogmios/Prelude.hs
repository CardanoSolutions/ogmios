--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Prelude
    ( -- * relude, minus STM
      module Relude

      -- * generic-lens commons
    , HasType
    , view
    , typed
    ) where

import Relude hiding
    ( Nat
    , STM
    , TVar
    , atomically
    , newEmptyTMVar
    , newTMVar
    , newTVar
    , putTMVar
    , readTVar
    , takeTMVar
    , tryTakeTMVar
    , writeTVar
    )

import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
