-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main where

import Prelude

import Criterion
    ( bench, bgroup, env, whnf )
import Criterion.Main
    ( defaultMain )
import Data.ByteString.Bech32
    ( pattern HumanReadablePart, encodeBech32 )
import Data.ByteString.Random
    ( random )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString.Base32 as Base32

main :: IO ()
main = defaultMain
    [ env cases $ \ ~(_10, _100, _1000) -> do
        bgroup "encode"
            [ bgroup "10"
                [ bench "base32" $ whnf base32 _10
                , bench "bech32" $ whnf bech32 _10
                , bench "fast-bech32" $ whnf fastBech32 _10
                ]
            , bgroup "100"
                [ bench "base32" $ whnf base32 _100
                , bench "bech32" $ whnf bech32 _100
                , bench "bech32-fast" $ whnf fastBech32 _100
                ]
            , bgroup "1000"
                [ bench "base32" $ whnf base32 _1000
                , bench "bech32" $ whnf bech32 _1000
                , bench "bech32-fast" $ whnf fastBech32 _1000
                ]
            ]
    ]
  where
    base32 = Base32.encodeBase32
    bech32 = let Right hrp = Bech32.humanReadablePartFromText "bench_" in
        Bech32.encode hrp . Bech32.dataPartFromBytes
    fastBech32 = let hrp = HumanReadablePart "bench_" in
        encodeBech32 hrp

    cases = (,,)
        <$> random 10
        <*> random 100
        <*> random 1000
