{-|
Module      : app.Main (internal)
Description : Trivial target for application executable build.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This module provides a trivial Main target for application executable builds.
-}

{-# LANGUAGE NoImplicitPrelude, Safe #-}

module Main (main) where

import Prelude
import PastaCurves

main :: IO ()
main = do
  print "Sample executable for pasta-curves"
  print $ pointMul (2 ^ (200::Integer) - 1 :: Fq) (base :: Pallas)
  -- print exampleFp
  -- print exampleFq
  -- print examplePallasPt
  -- print exampleVestaPt

{- For profiling
  cabal v2-run --enable-profiling exes --  +RTS -p
-}
