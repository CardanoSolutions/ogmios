--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.Exception
    ( -- * Classes
      MonadThrow (..)
    , MonadMask (..)
    , MonadCatch (..)

      -- * Re-exports
    , Exception (..)
    , SomeException (..)
    , IOException

      -- * Useful predicates
    , isAsyncException
    , isDoesNotExistError
    , isTryAgainError
    , isResourceVanishedError
    ) where

import Ogmios.Prelude

import Control.Exception.Safe
    ( IOException
    , isAsyncException
    )
import Control.Monad.Class.MonadThrow
    ( MonadCatch (..)
    , MonadMask (..)
    , MonadThrow (..)
    )
import Data.List
    ( isInfixOf
    )
import System.IO.Error
    ( isDoesNotExistError
    )

isTryAgainError :: IOException -> Bool
isTryAgainError = isInfixOf "resource exhausted" . show

isResourceVanishedError :: IOException -> Bool
isResourceVanishedError = isInfixOf "resource vanished" . show
