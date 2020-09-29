module System.Time.Clock
    ( NominalDiffTime
    , timed
    , nominalDiffTimeToMilliseconds
    , nominalDiffTimeToMicroseconds
    ) where

import Prelude

import Control.Exception
    ( evaluate )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime )

-- | Run an action, evaluate its result, and return the time elapsed.
timed :: IO a -> IO (a, NominalDiffTime)
timed action = do
    start <- getCurrentTime
    a <- action >>= evaluate
    end <- getCurrentTime
    pure (a, diffUTCTime end start)

nominalDiffTimeToMilliseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMilliseconds = round . (* 1000)

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Integer
nominalDiffTimeToMicroseconds = (* 1000) . nominalDiffTimeToMilliseconds
