--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE KindSignatures #-}

-- | This module is a simple wrapper around the STM 'TQueue' interface, but it
-- allows deferring the choice of the underlying monad to the caller and, it
-- provides an interface that is convenient to use via -XNamedFieldPuns.
module Control.Concurrent.Queue
    ( Queue(..)
    , newQueue
    ) where

import Prelude

import Control.Concurrent.STM
    ( atomically )
import Control.Concurrent.STM.TQueue
    ( newTQueueIO, readTQueue, tryReadTQueue, writeTQueue )

-- | A simple interface of a (thread-safe) 'Queue'
data Queue a (m :: * -> *) = Queue
    { pop :: m a
        -- ^ Unstash a value. Block until one is available

    , tryPop :: m (Maybe a)
        -- ^ Unstash a value, if any.

    , push :: a -> m ()
        -- ^ Stash a value for later.
    }

-- | Create a new 'Queue' in 'IO'
newQueue :: IO (Queue a IO)
newQueue = do
    resp <- newTQueueIO
    return Queue
        { pop = atomically $ readTQueue resp
        , tryPop = atomically $ tryReadTQueue resp
        , push  = atomically . writeTQueue resp
        }
