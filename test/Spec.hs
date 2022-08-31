{-# LANGUAGE NoImplicitPrelude, Trustworthy #-}

module Main (main) where

import Prelude
import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import TestFields (fieldProps, testBadF, testGoodF, testRnd)
import TestCurves (curveProps, testHashToPallas, testHashToVesta, testPOI, testBadC, testPallasEq, testRndPV)


main :: IO ()
main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "1_000"
  defaultMain $ testGroup "\nRunning Tests" [fieldProps, testBadF, testGoodF, testRnd,
    testHashToPallas, testHashToVesta, curveProps, testPOI, testBadC, testPallasEq, testRndPV]
  print "Finished!"
