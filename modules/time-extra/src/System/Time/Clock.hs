--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Portable
module System.Time.Clock
    (
    -- * Re-export
      NominalDiffTime

    -- * Measuring
    , timed
    , nominalDiffTimeToMilliseconds
    , nominalDiffTimeToMicroseconds

    -- * Debouncing
    , Debouncer (..)
    , newDebouncer
    ) where

import Prelude

import Control.Concurrent.Async
    ( async, link )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( newMVar, putMVar, tryTakeMVar )
import Control.Monad
    ( forever )
import Control.Exception
    ( evaluate )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime )

-- | Run an action, evaluate its result, and return the time elapsed.
--
-- @since 1.0.0
timed :: IO a -> IO (a, NominalDiffTime)
timed action = do
    start <- getCurrentTime
    a <- action >>= evaluate
    end <- getCurrentTime
    pure (a, diffUTCTime end start)

-- | Convert a 'NominalDiffTime' to ms
--
-- @since 1.0.0
nominalDiffTimeToMilliseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMilliseconds = round . (* 1000)

-- | Convert a 'NominalDiffTime' to μs
--
-- @since 1.0.0
nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds = (* 1000) . nominalDiffTimeToMilliseconds

-- | A type holding a debounced action; that is calling the inner 'debounce'
-- multiple times within a given time period will only trigger the actual action
-- once.
--
-- @since 1.0.0
newtype Debouncer = Debouncer
    { debounce :: IO () -> IO ()
    }

-- | Run an action, but no more than once every chosen interval of time.
--
-- @since 1.0.0
newDebouncer :: NominalDiffTime -> IO Debouncer
newDebouncer delay = do
    lock <- newMVar ()
    link =<< async (forever $ threadDelay (micro delay) *> putMVar lock ())
    return $ Debouncer $ \action -> tryTakeMVar lock >>= \case
        Nothing -> return ()
        Just () -> action
  where
    micro = fromIntegral . nominalDiffTimeToMicroseconds
