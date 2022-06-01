-- Test curves...

{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestCurves (curveProps, testPOI, testHashToPallas, testHashToVesta, testBadC) where

import Prelude hiding (exp)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString (pack)
import Data.Maybe (fromJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), testProperty)
import Test.Tasty.HUnit (assertBool, testCase)
import TestFields ()
import PastaCurves
import Curves (Point(Projective))
import Data.Word (Word8)


instance Arbitrary Pallas where
  arbitrary = do
    scalar <- arbitrary
    return $ pointMul (scalar :: Fq) (base :: Pallas)  


instance Arbitrary Vesta where
  arbitrary = do
    scalar <- arbitrary
    return $ pointMul (scalar :: Fp) (base :: Vesta)  


curveProps :: TestTree
curveProps = testGroup "Testing Curve properties via QuickCheck" [

  testProperty "Pallas point add/mul" $ 
    \x y -> pointAdd (pointMul (x :: Fq) base) (pointMul y base) == pointMul (x+y) (base::Pallas),
  testProperty "Pallas point add symm" $
    \x y -> pointAdd x y == pointAdd y (x :: Pallas),
  testProperty "Pallas ser->deser" $
    \x -> fromJust (fromBytesC (toBytesC x)) == (x :: Pallas),

  testProperty "Vesta point add/mul" $ 
    \x y -> pointAdd (pointMul (x :: Fp) base) (pointMul y base) == pointMul (x+y) (base :: Vesta),
  testProperty "Vesta point add symm" $
    \x y -> pointAdd x y == pointAdd y (x :: Vesta),
  testProperty "Vesta ser->deser" $
    \x -> fromJust (fromBytesC (toBytesC x)) == (x :: Vesta)
  ] 


testPOI :: TestTree
testPOI = testCase "poi decode" $
  do
    let poiBytes = pack [0]
    let orderLessOne = (0::Fq) - 1
    let act = pointAdd (pointMul orderLessOne base) base :: Pallas
    let actBytes = toBytesC act
    assertBool "bad pairing mul" (poiBytes == actBytes)
    assertBool "point point eq" (act == (neutral :: Pallas))


testBadC :: TestTree
testBadC = testCase "bad decode" $ do
  let tooFewBytes = pack [0, 0]
  let act1 = fromBytesC tooFewBytes :: Maybe Pallas
  assertBool "bad too short bytes" (isNothing act1)
  let tooManyBytes = pack $ replicate 34 0
  let act2 = fromBytesC tooManyBytes :: Maybe Pallas
  assertBool "bad too many bytes" (isNothing act2)
  let tooLargeValue = pack $ (0x02 :: Word8) : (0x41 :: Word8) : replicate 31 0
  let act3 = fromBytesC tooLargeValue :: Maybe Pallas
  assertBool "bad too large value" (isNothing act3)
  let notOnCurve = pack $ (0x02 :: Word8) : replicate 32 0
  let act4 = fromBytesC notOnCurve :: Maybe Pallas
  assertBool "bad not on curve" (isNothing act4)
  let baseBytes = pack $ ((0x02 :: Word8) : replicate 31 0) ++ [0x01 :: Word8]
  let act5 = fromBytesC baseBytes :: Maybe Pallas
  assertBool "bad is base" $ fromJust act5 == (base :: Pallas)
  let notBaseBytes = pack $ ((0x03 :: Word8) : replicate 31 0) ++ [0x01 :: Word8]
  let act6 = fromBytesC notBaseBytes :: Maybe Pallas
  assertBool "bad not base" $ fromJust act6 /= (base :: Pallas)


testHashToPallas :: TestTree
testHashToPallas = testCase "testHashToPallas" $ assertBool "Failed testHashToPallas" helper
  where
    actual = hashToPallas (fromString "Trans rights now!")  -- String from zcash test vector line 147 (link below)
    -- See https://github.com/zcash/pasta_curves/blob/21fd9e2c1bbd2d049bfe95588d77cb884e9f93ab/src/pallas.rs#L150-L158
    z = 0x1d48103df8fcbb70d1809c1806c95651dd884a559fec0549658537ce9d94bed9 :: Fp
    x = 0x36a6e3a9c50b7b6540cb002c977c82f37f8a875fb51eb35327ee1452e6ce7947 * inv0 (z ^ (2::Integer))
    y = 0x01da3b4403d73252f2d7e9c19bc23dc6a080f2d02f8262fca4f7e3d756ac6a7c * inv0 (z ^ (3::Integer))
    expected = Projective x y 1 :: Pallas    
    helper = actual == expected


testHashToVesta :: TestTree
testHashToVesta = testCase "testHashToVesta" $ assertBool "Failed testHashToVesta" helper
  where
    actual = hashToVesta (fromString "hello")  -- String from zcash test vector line 147 (link below)
    -- See https://github.com/zcash/pasta_curves/blob/21fd9e2c1bbd2d049bfe95588d77cb884e9f93ab/src/vesta.rs#L63-L71
    z = 0x1b58d4aa4d68c3f4d9916b77c79ff9911597a27f2ee46244e98eb9615172d2ad :: Fq
    x = 0x12763505036e0e1a6684b7a7d8d5afb7378cc2b191a95e34f44824a06fcbd08e * inv0 (z ^ (2::Integer))
    y = 0x0256eafc0188b79bfa7c4b2b393893ddc298e90da500fa4a9aee17c2ea4240e6 * inv0 (z ^ (3::Integer))
    expected = Projective x y 1 :: Vesta
    helper = actual == expected