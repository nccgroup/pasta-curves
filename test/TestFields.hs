-- Test fields...

{-# LANGUAGE DataKinds, FlexibleInstances, NoImplicitPrelude, OverloadedStrings, Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TestFields (fieldProps, testBadF, testGoodF) where

import Prelude hiding (sqrt)
import Data.ByteString (pack)
import Data.Maybe (fromJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), choose, testProperty)

import PastaCurves
import Test.Tasty.HUnit (testCase, assertBool)
import Data.Word (Word8)


instance Arbitrary Fp where
   arbitrary = fromInteger <$> choose (0, pallasPrime - 1)

instance Arbitrary Fq where
   arbitrary = fromInteger <$> choose (0, vestaPrime - 1)


fieldProps :: TestTree
fieldProps = testGroup "Testing Field properties via QuickCheck" [
  testProperty "Fp arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fp),
  testProperty "Fp inv0"   $ \a -> a * inv0 a == (1 :: Fp),
  testProperty "Fp sqrt"   $ \a -> fromJust (sqrt (a*a)) ^ (2 :: Integer) == (a ^ (2 :: Integer) :: Fp),
  testProperty "Fp isSqr"  $ \a -> isSqr (a*a :: Fp),
  testProperty "Fp serdes" $ \a -> fromBytesF (toBytesF a) == Just (a :: Fp),
  testProperty "Fp shiftR1" $ \a -> shiftR1 a == fromInteger (toI (a :: Fp) `div` 2),

  testProperty "Fq arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fq),
  testProperty "Fq inv0"   $ \a -> a * inv0 a == (1 :: Fq),
  testProperty "Fq sqrt"   $ \a -> fromJust (sqrt (a*a)) ^ (2 :: Integer) == (a ^ (2 :: Integer)  :: Fq),
  testProperty "Fq isSqr"  $ \a -> isSqr (a*a :: Fq),
  testProperty "Fq serdes" $ \a -> fromBytesF (toBytesF a) == Just (a :: Fq),
  testProperty "Fq shiftR1" $ \a -> shiftR1 a == fromInteger (toI (a :: Fq) `div` 2)
  ]


testBadF :: TestTree
testBadF = testCase "testBadF" $
  do
    let tooFewBytes = pack [0]
    let act1 = fromBytesF tooFewBytes :: Maybe Fq
    assertBool "bad too short bytes" (isNothing act1)
    let tooManyBytes = pack $ replicate 33 0
    let act2 = fromBytesF tooManyBytes :: Maybe Fq
    assertBool "bad too many bytes" (isNothing act2)
    let tooLargeValue = pack $ (0x41 :: Word8) : replicate 31 0
    let act3 = fromBytesF tooLargeValue :: Maybe Fq
    assertBool "bad too large value" (isNothing act3)

testGoodF :: TestTree
testGoodF = testCase "testGoodF" $
  do
    assertBool "sgn0 -1" (sgn0 (negate (1 :: Fq)) == 0)
    assertBool "sgn0 1" (sgn0 (1 :: Fq) == 1)
    -- hash2Field tested as part of hash2Curve

