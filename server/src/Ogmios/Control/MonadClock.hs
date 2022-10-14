--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadClock
    ( -- * Class
      MonadClock (..)
    , idle

      -- * Debouncer
    , Debouncer(..)
    , withDebouncer

      -- * Helpers
    , foreverCalmly
    , diffTimeToMilliseconds
    , diffTimeToMicroseconds
    , _1s
    , _5s
    , _10s
    , _30s
    ) where

import Ogmios.Prelude

import Ogmios.Control.MonadAsync
    ( MonadAsync (..)
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , newTMVar
    , putTMVar
    , tryTakeTMVar
    )

import Control.Exception
    ( evaluate
    )
import Control.Monad.Reader
    ( mapReaderT
    )
import Data.Time.Clock
    ( DiffTime
    , NominalDiffTime
    , UTCTime
    , diffUTCTime
    )

import qualified Control.Concurrent as IO
import qualified Data.Time.Clock as IO

-- | A 'Monad' to make time effects explicit.
class Monad m => MonadClock (m :: Type -> Type) where
    getCurrentTime :: m UTCTime
    -- ^ Get the time of the current clock.

    threadDelay :: DiffTime -> m ()
    -- ^ Mark a pause in the current thread

    timed :: m a -> m (a, DiffTime)
    -- ^ Measure the actual time taken by an action.

-- | Do nothing. Just wait.
idle :: MonadClock m => m a
idle = forever $ threadDelay 43200

instance MonadClock IO where
    getCurrentTime = IO.getCurrentTime
    threadDelay = IO.threadDelay . diffTimeToMicroseconds
    timed action = do
        start <- IO.getCurrentTime
        a <- action >>= evaluate
        end <- IO.getCurrentTime
        pure (a, toDiffTime $ diffUTCTime end start)
      where
        toDiffTime :: NominalDiffTime -> DiffTime
        toDiffTime = toEnum . fromEnum

instance MonadClock m => MonadClock (ReaderT env m) where
    getCurrentTime = lift getCurrentTime
    threadDelay = lift . threadDelay
    timed = mapReaderT timed

--
-- Debounce
--

-- | A type holding a debounced action; that is calling the inner 'debounce'
-- multiple times within a given time period will only trigger the actual action
-- once.
newtype Debouncer m = Debouncer
    { debounce :: m () -> m ()
    }

-- | Run an action, but no more than once every chosen interval of time.
withDebouncer
    :: (MonadAsync m, MonadClock m)
    => DiffTime
    -> (Debouncer m -> m a)
    -> m a
withDebouncer delay action = do
    lock <- atomically $ newTMVar ()

    let debouncer = Debouncer $ \io -> atomically (tryTakeTMVar lock) >>= \case
            Nothing -> return ()
            Just () -> io

    let inner = forever $ threadDelay delay *> atomically (putTMVar lock ())

    withAsync inner (\_ -> action debouncer)

--
-- Helpers
--

-- | Like 'forever', but with pauses.
foreverCalmly :: MonadClock m => m a -> m a
foreverCalmly a = do
    let a' = a *> threadDelay _5s *> a' in a'

-- | Convert a 'DiffTime' to ms
diffTimeToMilliseconds :: DiffTime -> Int
diffTimeToMilliseconds = round . (* 1000)

-- | Convert a 'DiffTime' to μs
diffTimeToMicroseconds :: DiffTime -> Int
diffTimeToMicroseconds = (* 1000) . diffTimeToMilliseconds

-- | A delay of exactly 1s
_1s :: DiffTime
_1s = 1

-- | A delay of exactly 5s
_5s :: DiffTime
_5s = 5

-- | A delay of exactly 10s
_10s :: DiffTime
_10s = 10

-- | A delay of exactly 30s
_30s :: DiffTime
_30s = 30
