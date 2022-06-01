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
  print exampleFp
  print exampleFq
  print examplePallas
  print exampleVesta

