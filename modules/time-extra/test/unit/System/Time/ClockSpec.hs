--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module System.Time.ClockSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( replicateM_ )
import Control.Concurrent.MVar
    ( modifyMVar_, readMVar, newMVar )
import Control.Concurrent
    ( threadDelay )
import System.Time.Clock
    ( Debouncer (..)
    , NominalDiffTime
    , newDebouncer
    , nominalDiffTimeToMicroseconds
    , nominalDiffTimeToMilliseconds
    , timed
    )
import Test.Hspec
    ( Spec, context, specify, shouldReturn )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Gen
    , choose
    , counterexample
    , cover
    , forAll
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, monitor, run )

spec :: Spec
spec = do
    context "nominalDiffTimeToXXX" $ do

        prop "X to ms is always smaller than X to μs"
            $ forAll genNominalDiffTime
            $ \t -> t /= 0 ==>
            nominalDiffTimeToMilliseconds t < nominalDiffTimeToMicroseconds t

        prop "0 is stable"
            $ nominalDiffTimeToMilliseconds 0 === nominalDiffTimeToMicroseconds 0

    context "timed" $ do
        prop "can time action with precision"
            $ monadicIO
            $ do
                ((), t) <- run $ timed $ threadDelay _100ms
                let ok = nominalDiffTimeToMilliseconds t `within` (99, 101)
                monitor $ counterexample $ show t
                monitor $ cover 90 ok "OK"

    context "Debouncer" $ do
        specify "does not run an action more than N times per period" $ do
            let period = 1 :: NominalDiffTime
            counter <- newMVar (0 :: Word)
            let action = modifyMVar_ counter (pure . (+1))
            Debouncer{debounce} <- newDebouncer period
            replicateM_ 10 (debounce action)
            readMVar counter `shouldReturn` 1
            threadDelay (fromInteger $ nominalDiffTimeToMicroseconds period)
            debounce action
            readMVar counter `shouldReturn` 2

--
-- Helpers
--

-- | 100ms in micro-seconds
_100ms :: Int
_100ms = 100_000

-- | Test that a value is within the given min/max boundaries
within :: Integer -> (Integer, Integer) -> Bool
within x (inf, sup) = x >= inf && x <= sup

--
-- Arbitrary Generators
--

genNominalDiffTime :: Gen NominalDiffTime
genNominalDiffTime = toEnum . (*1_000_000_000_000) <$> choose (0, 100)
