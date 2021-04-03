-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Prelude

import Criterion
    ( bench, bgroup, env, whnf )
import Criterion.Main
    ( defaultMain )
import Data.ByteString.Bech32
    ( HumanReadablePart (..), encodeBech32 )
import Data.ByteString.Random
    ( random )

import qualified Codec.Binary.Bech32 as Bech32

main :: IO ()
main = defaultMain
    [ env cases $ \ ~(_10, _100, _1000) -> do
        bgroup "encode"
            [ bgroup "10"
                [ bench "bech32" $ whnf bech32 _10
                , bench "fast-bech32" $ whnf fastBech32 _10
                ]
            , bgroup "100"
                [ bench "bech32" $ whnf bech32 _100
                , bench "fast-bech32" $ whnf fastBech32 _100
                ]
            , bgroup "1000"
                [ bench "bech32" $ whnf bech32 _1000
                , bench "fast-bech32" $ whnf fastBech32 _1000
                ]
            ]
    ]
  where
    Right hrp = Bech32.humanReadablePartFromText "bench_"
    bech32 = Bech32.encode hrp . Bech32.dataPartFromBytes

    fastBech32 = encodeBech32 hrp'
    hrp' = HumanReadablePart "bench_"

    cases = (,,)
        <$> random 10
        <*> random 100
        <*> random 1000
