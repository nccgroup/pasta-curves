{-# LANGUAGE Trustworthy, NoImplicitPrelude #-}

module Main (main) where

import Prelude (IO, print, ($))
--import System.Environment (setEnv)
import Test.Tasty (defaultMain, testGroup)
import TestFields --(fieldProps)
--(fieldProps)
import TestCurves (curveProps, testPOI, testHashToPallas, testHashToVesta, testBadC)


main :: IO ()
main = do
  -- setEnv "TASTY_QUICKCHECK_TESTS" "1_000"
  defaultMain $ testGroup "\nRunning Tests" [fieldProps, testBadF, testGoodF,
    testHashToPallas, testHashToVesta, curveProps, testPOI, testBadC]
  print "wogga!"
