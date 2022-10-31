--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadAsync
    ( MonadAsync (..)
    , AsyncCancelled(..)
    , ExceptionInLinkedThread(..)

    , MonadLink
    , MonadFork
    , MonadThread
    , link
    ) where

import Control.Monad.Class.MonadAsync
    ( AsyncCancelled (..)
    , ExceptionInLinkedThread (..)
    , MonadAsync (..)
    , link
    )
import Control.Monad.Class.MonadFork
    ( MonadFork
    , MonadThread
    )
import Control.Monad.Class.MonadThrow
    ( MonadMask
    )

type MonadLink m = (MonadFork m, MonadMask m)
