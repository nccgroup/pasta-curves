{-# LANGUAGE NoImplicitPrelude, Safe #-}

module Main (main) where

import Prelude
import Data.ByteString.UTF8 (fromString)
import Criterion.Main
import PastaCurves


main :: IO ()
-- main = do
--   print "Benchmarks for pasta-curves"
--   print exampleFp
--   print exampleFq
--   print examplePallas
--   print exampleVesta

-- Benchmarks will be further developed in future versions...

main = defaultMain [
  bgroup "Group 1" [ 
    bench "Hello Pallas 1"  $ whnf hashToPallas (fromString "Hello Pallas 1"), 
    bench "Hello Pallas 2"  $ whnf hashToPallas (fromString "Hello Pallas 2"), 
    bench "Hello Vesta 1"   $ whnf hashToVesta  (fromString "Hello Vesta 1"), 
    bench "Hello Vesta 2"   $ whnf hashToVesta  (fromString "Hello Vesta 1"), 
    bench "Pasta base * -1" $ whnf (pointMul (-1 :: Fq)) (base :: Pallas),
    bench "Vesta base * -1" $ whnf (pointMul (-1 :: Fp)) (base :: Vesta)
    ]
  ]