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

