{-# LANGUAGE DataKinds, FlexibleInstances, NoImplicitPrelude, OverloadedStrings, Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module TestFields (fieldProps) where

import Prelude hiding (sqrt)
import Control.Monad (replicateM)
import Data.ByteString (ByteString, pack)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), choose, testProperty)

import PastaCurves


instance Arbitrary Fp where
   arbitrary = do
     fromInteger <$> choose (0, pallasPrime - 1)


instance Arbitrary Fq where
   arbitrary = do
     fromInteger <$> choose (0, vestaPrime - 1)

type Serdes = ByteString

instance Arbitrary Serdes where
  arbitrary = pack <$> replicateM 32 arbitrary


fieldProps :: TestTree
fieldProps = testGroup "Testing Field properties via QuickCheck" [
  testProperty "Fp arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fp),
  testProperty "Fp inv0"   $ \a -> a * inv0 a == (1 :: Fp),
  testProperty "Fp sqrt"   $ \a -> fromJust (sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer) :: Fp),
  testProperty "Fp serdes" $ \a -> fromJust (fromBytesF  (toBytesF (_fromBytesF a ::
       Fp))) == (_fromBytesF a :: Fp),

  testProperty "Fq arith"  $ \a b c -> a*(b-c) - a*b + a*c == (0 :: Fq),
  testProperty "Fq inv0"   $ \a -> a * inv0 a == (1 :: Fq),
  testProperty "Fq sqrt"   $ \a -> fromJust (sqrt (a*a))^(2 :: Integer) == (a^(2 :: Integer)  :: Fq),
  testProperty "Fq serdes" $ \a -> fromJust (fromBytesF  (toBytesF (_fromBytesF a ::
       Fq))) == (_fromBytesF a :: Fq)
  ]
