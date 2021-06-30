--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadSTM
    ( MonadSTM (..)

    , TQueue
    , newTQueue
    , writeTQueue
    , readTQueue
    , tryReadTQueue

    , TVar
    , newTVar
    , writeTVar
    , readTVar

    , TMVar
    , newTMVar
    , newEmptyTMVar
    , putTMVar
    , takeTMVar
    , tryTakeTMVar
    , withTMVar
    ) where

import Ogmios.Prelude

import Control.Monad.Class.MonadSTM
    ( MonadSTM (..), MonadSTMTx (..), TMVar, TQueue, TVar )
import Control.Monad.Class.MonadThrow
    ( MonadCatch (..), MonadMask (..) )

-- | An exception-safe bracket-style acquisition of a TMVar.
withTMVar :: (MonadSTM m, MonadMask m) => TMVar m a -> (a -> m b) -> m b
withTMVar var action = mask $ \restore -> do
    a <- atomically (takeTMVar var)
    b <- restore (action a) `onException` atomically (putTMVar var a)
    b <$ atomically (putTMVar var a)
