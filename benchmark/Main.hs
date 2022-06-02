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


-- Benchmarks will be further developed in future versions...
main :: IO ()
main = defaultMain [
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
0. Inline and other compiler directives
1. Remove field add/sub MOD by compare then subtract
2. Remove field mul MOD by Barrett reduction / Montgomery mult
3. Implement explicit pointDouble routine
4. Separate out statics (e.g. in sqrt routines)
-}