--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Control.Manufacture
    (
    -- * Worker
    Worker
        ( Worker
        , await
        , tryAwait
        , yield
        , pass
        )

    -- * Construction
    , newWorker
    , bindWorker
    , feedWorker
    ) where

import Prelude

import Control.Concurrent.STM
    ( atomically )
import Control.Concurrent.STM.TQueue
    ( TQueue, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue )
import Control.Monad
    ( forever )

-- | A worker for the manufacture. Each worker can either operate on the next
-- inputs or pass it on to the next one in the assembly line.
data Worker input output (m :: * -> *) = Worker_
    { _await :: m input
        -- ^ Await for the next input. Block until an input is available.

    , _tryAwait :: m (Maybe input)
        -- ^ Return 'Just input' if there's an input available, nothing
        -- otherwise. Non blocking.

    , _yield :: output -> m ()
        -- ^ Yield a result.

    , _pass  :: input -> m ()
        -- ^ Pass the input onto another component

    , _queue :: TQueue input
        -- ^ Actual input queue of the worker
    }

pattern Worker
    :: m input
    -> m (Maybe input)
    -> (output -> m ())
    -> (input -> m ())
    -> Worker input output m
pattern Worker { await, tryAwait, yield, pass } <-
    Worker_ await tryAwait yield pass _

{-# COMPLETE Worker #-}

newWorker
    :: (output -> IO ())
    -> (input -> IO ())
    -> IO (Worker input output IO)
newWorker _yield _pass = do
    _queue <- newTQueueIO
    pure Worker_
        { _await = atomically $ readTQueue _queue
        , _tryAwait = atomically $ tryReadTQueue _queue
        , _pass
        , _yield
        , _queue
        }

bindWorker
    :: Worker input output IO
    -> IO (Worker input output IO)
bindWorker Worker_{_yield,_queue=next} = do
    _queue <- newTQueueIO
    pure Worker_
        { _await = atomically $ readTQueue _queue
        , _tryAwait = atomically $ tryReadTQueue _queue
        , _pass = atomically . writeTQueue next
        , _yield
        , _queue
        }

feedWorker
    :: IO input
    -> Worker input output IO
    -> IO ()
feedWorker source Worker_{_queue} =
    forever $ source >>= atomically . writeTQueue _queue
