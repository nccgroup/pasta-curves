{-# LANGUAGE NoImplicitPrelude, Trustworthy #-}

module Main (main) where

import Prelude
import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import TestFields (fieldProps, testBadF, testGoodF)
import TestCurves (curveProps, testHashToPallas, testHashToVesta, testPOI, testBadC, testPallasEq)


main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "1_000"
  defaultMain $ testGroup "\nRunning Tests" [fieldProps, testBadF, testGoodF,
    testHashToPallas, testHashToVesta, curveProps, testPOI, testBadC, testPallasEq]
  print "Finished!"
