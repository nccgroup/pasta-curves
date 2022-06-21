{-|
Module      : benchmark.Main (internal)
Description : Target for benchmarking build.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This module provides a basic Main target for benchmarking builds. The suite of
benchmarks will be expanded over time.
-}

{-# LANGUAGE NoImplicitPrelude, Safe #-}

module Main (main) where

import Prelude
import Data.ByteString.UTF8 (fromString)
import Criterion.Main
import PastaCurves
import Criterion.Types (timeLimit, resamples)


benchConfig = defaultConfig {
              timeLimit = 60,
              resamples = 20
           }

-- Benchmarks will be further developed in future versions...
main :: IO ()
main = defaultMainWith benchConfig [
  bgroup "Group 1" [ 
    bench "Hello Pallas 1"  $ whnf hashToPallas (fromString "Hello Pallas 1"), 
    bench "Hello Pallas 2"  $ whnf hashToPallas (fromString "Hello Pallas 2"), 
    bench "Hello Vesta 1"   $ whnf hashToVesta  (fromString "Hello Vesta 1"), 
    bench "Hello Vesta 2"   $ whnf hashToVesta  (fromString "Hello Vesta 2"), 
    bench "Pasta base * -1" $ whnf (pointMul (-1 :: Fq)) (base :: Pallas),
    bench "Pasta base * 2^200 - 1" $ whnf (pointMul (2 ^ (200::Integer) - 1 :: Fq)) (base :: Pallas),
    bench "Pasta base * 2^100 - 1" $ whnf (pointMul (2 ^ (100::Integer) - 1 :: Fq)) (base :: Pallas),
    bench "Vesta base * -1" $ whnf (pointMul (-1 :: Fp)) (base :: Vesta)
    ]
  ]

-- Note: Initial field multiply performance is in line with expected Rust BigInt described at
-- https://research.nccgroup.com/2021/09/10/optimizing-pairing-based-cryptography-montgomery-multiplication-in-assembly/

{- Potential performance improvements to investigate/implement
0. Inline (<- almost zero) and other compiler directives (-O2 <- almost zero)
1. Remove field add/sub MOD by compare then subtract (<- almost zero)
2. Remove field mul MOD by Barrett reduction / Montgomery multiplication
3. Implement explicit pointDouble routine
4. Separate out statics (e.g. in sqrt routines)

Base Case
=========
benchmarking Group 1/Hello Pallas 1
time                 2.213 ms   (2.190 ms .. 2.257 ms)

benchmarking Group 1/Hello Pallas 2
time                 2.437 ms   (2.428 ms .. 2.441 ms)

benchmarking Group 1/Hello Vesta 1
time                 2.458 ms   (2.454 ms .. 2.464 ms)

benchmarking Group 1/Hello Vesta 2
time                 2.378 ms   (2.371 ms .. 2.398 ms)

benchmarking Group 1/Pasta base * -1
time                 92.48 μs   (92.35 μs .. 92.73 μs)

benchmarking Group 1/Pasta base * 2^200 - 1
time                 106.4 μs   (104.4 μs .. 109.1 μs)

benchmarking Group 1/Pasta base * 2^100 - 1
time                 31.51 μs   (31.45 μs .. 31.58 μs)

benchmarking Group 1/Vesta base * -1
time                 92.43 μs   (91.97 μs .. 92.79 μs)
-}