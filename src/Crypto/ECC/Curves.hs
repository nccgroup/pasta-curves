{-|
Module      : Crypto.PastaCurves.Curves (internal)
Description : Supports the instantiation of parameterized prime-order elliptic curves.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This internal module provides an elliptic curve (multi-use) template from arbitrary
parameters for a curve of odd order, along with a variety of supporting functionality 
such as point addition, multiplication, negation, equality, serialization and 
deserialization. The algorithms are NOT constant time. Safety and simplicity are the 
top priorities; the curve order must be prime (and so affine curve point y-cord != 0).
-}

{-# LANGUAGE CPP, DataKinds, DerivingStrategies, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, Safe, ScopedTypeVariables #-}

module Curves (Curve(..), CurvePt(..), Point(..)) where

import Prelude hiding (drop, length, sqrt)
import Control.Monad (mfilter)
import Data.ByteString (ByteString, cons, drop, index, length, pack)
import Data.Maybe (fromJust)
import Data.Typeable (Proxy (Proxy))
import GHC.TypeLits (Nat, KnownNat, natVal)
import Fields (Field (..))


-- | The `Point` type incorporates type literals @a@ and @b@ of an elliptic curve in the
-- short Weierstrass normal form. It also incorporates @baseX@ and @baseY@ coordinates
-- for the base type. A point with different literals is considered a different type, so
-- cannot be inadvertently mixed. *The curve order must be prime, and `_y` cannot be zero*.
data Point (a :: Nat) (b :: Nat) (baseX :: Nat) (baseY :: Nat) f = 
  Projective {_x :: f, _y :: f, _z :: f} deriving stock (Show) -- (x * inv0 z, y * inv0 z)
            

-- CPP macro 'helpers' to extract the curve parameters from `Point a b baseX baseY f`
#define A natVal (Proxy :: Proxy a)
#define B natVal (Proxy :: Proxy b)
#define BASE_X natVal (Proxy :: Proxy baseX)
#define BASE_Y natVal (Proxy :: Proxy baseY)


-- Calculate equality for projective points
instance (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) =>
  Eq (Point a b baseX baseY f) where
    
  -- x1/z1 == x2/z2 -> x1*z2/(x2*z1) == 1 -> same for y -> x1*z2/(x2*z1) == y1*z2/(y2*z1)
  -- All (neutral) points at infinity are equal.  
  (==) (Projective x1 y1 z1) (Projective x2 y2 z2) = 
    (x1 * z2 == x2 * z1) && (y1 * z2 == y2 * z1)


-- | The `CurvePt` class provides the bulk of the functionality related to operations
-- involving points on the elliptic curve. It supports both the Pallas and Vesta curve
-- point types, as well as any other curves (using the arbitrary curve parameters). The
-- curve order must be prime.
class CurvePt a where

  -- | Returns the (constant) base point.
  base :: a
  
  -- | The `fromBytesC` function deserializes a compressed point from a ByteString. An 
  -- invalid ByteString will return @Nothing@.
  fromBytesC :: ByteString -> Maybe a

  -- | The `isOnCurve` function validates whether the point is on the curve. It is 
  -- already utilized within `fromBytesC` deserialization, within hash-to-curve (for
  -- redundant safety) and within `toBytesC` serialization.
  isOnCurve :: a -> Bool

  -- | The `negatePt` function negates a point.
  negatePt :: a -> a

  -- | Returns the (constant) neutral point.
  neutral :: a

  -- | The `pointAdd` function adds two curve points on the same elliptic curve.
  pointAdd :: a -> a -> a

  -- | Adds two curve points on the same elliptic curve and return the result in affined coordinates.
  pointAddAffined :: a -> a -> a

  -- | The `toBytesC` function serializes a point to a (compressed) @ByteStream@.
  toBytesC :: a -> ByteString


instance (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) =>
  CurvePt (Point a b baseX baseY f) where

  -- Construct the base point directly from the type literals.
  base = Projective (fromInteger $ BASE_X) (fromInteger $ BASE_Y) 1


  -- Deserialize a ByteString into a point on the elliptic curve based on section 2.3.4
  -- of https://www.secg.org/sec1-v2.pdf. Only compressed points are supported.
  fromBytesC bytes
    -- If the ByteString is a single zero byte, the Just neutral point is returned
    | length bytes == 1 && index bytes 0 == 0 = Just neutral
    -- If the ByteString is correct length with an acceptable leading byte, attempt decode
    | length bytes == corLen && (index bytes 0 == 0x2 || index bytes 0 == 0x03) = result
        where
          -- correct length is the correct length of the field element plus 1
          corLen = 1 + length (toBytesF (fromInteger (A) :: f))
          -- drop the leading byte then deserialize the x-coordinate
          x = fromBytesF (drop 1 bytes) :: Maybe f
          -- see what sgn0 we are expecting for the final y-coordinate
          sgn0y = if index bytes 0 == 0x02 then 0 else 1 :: Integer
          -- calculate y squared from deserialized x-coordinate and curve constants
          alpha = (\t -> t ^ (3 :: Integer) + ((fromInteger $ A) :: f) * t + ((fromInteger $ B) :: f)) <$> x
          -- get square root (thus a proposed y-coordinate; note y cannot be zero)
          beta = alpha >>= sqrt
          -- adjust which root is selected for y-coordinate
          y =  (\t -> if sgn0 t == sgn0y then t else negate t) <$> beta
          -- propose a deserialized point (which is on the curve by construction)
          proposed = (Projective <$> x <*> y <*> Just 1) :: Maybe (Point a b baseX baseY f)
          -- re-validate it is on the curve and return; a sqrt fail propagates through Maybes
          result = mfilter isOnCurve proposed
  -- Otherwise we fail (bad length, bad prefix etc) and return Nothing
  fromBytesC _ = Nothing


  -- Validate via projective form of Weierstrass equation.
  isOnCurve (Projective x y z) = z * y ^ (2 :: Integer) == x ^ (3 :: Integer) + 
    fromInteger (A) * x * z ^ (2 :: Integer) + fromInteger (B) * z ^ (3 :: Integer)


  -- Point negation is flipping y-coordinate.
  negatePt (Projective x y z) = Projective x (- y) z


  -- Anything with z=0 is neutral (y cannot be 0)
  neutral = Projective 0 1 0


  -- See https://eprint.iacr.org/2015/1060.pdf page 8; Algorithm 1: Complete, projective 
  -- point addition for arbitrary (odd) prime order short Weierstrass curves 
  -- E/Fq : y^2 = x^3 + ax + b. The code has the intermediate additions 'squashed out'
  pointAdd (Projective x1 y1 z1) (Projective x2 y2 z2) = result
    where
      m0 = x1 * x2
      m1 = y1 * y2
      m2 = z1 * z2
      m3 = (x1 + y1) * (x2 + y2)
      m4 = (x1 + z1) * (x2 + z2)
      m5 = (y1 + z1) * (y2 + z2)
      m6 = ((fromInteger $ A) :: f) * (- m0 - m2 + m4)
      m7 = ((fromInteger $ 3 * B) :: f) * m2
      m8 = (m1 - m6 - m7) * (m1 + m6 + m7)
      m9 = ((fromInteger $ A) :: f) * m2
      m10 = ((fromInteger $ 3 * B) :: f) * (- m0 - m2 + m4)
      m11 = ((fromInteger $ A) :: f) * (m0 - m9)
      m12 = (m0 * 3 + m9) * (m10 + m11)
      m13 = (- m1 - m2 + m5) * (m10 + m11)
      m14 = (- m0 - m1 + m3) * (m1 - m6 - m7)
      m15 = (- m0 - m1 + m3) * (m0 * 3 + m9)
      m16 = (- m1 - m2 + m5) * (m1 + m6 + m7)
      result = Projective (-m13 + m14) (m8 + m12) (m15 + m16) :: Point a b baseX baseY f


  -- Convert the result from `pointAdd` to affined coordinates.
  pointAddAffined (Projective x1 y1 z1) (Projective x2 y2 z2) = result
    where
      (Projective xp yp zp) = pointAdd (Projective x1 y1 z1) (Projective x2 y2 z2) :: Point a b baseX baseY f
      x = xp * inv0 zp
      y = yp * inv0 zp
      result = Projective x y 1 :: Point a b baseX baseY f


  -- Note that point doubling from algorithm 3 on page 10 would require around "13" 
  -- multiplications (not much/enough performance benefit)


  -- Serialize a point on the elliptic curve into a ByteString based on section 2.3.3
  -- of https://www.secg.org/sec1-v2.pdf. Only compressed points are supported.
  --toBytesC (Projective xp yp zp)
  toBytesC pt
    | not $ isOnCurve pt = error "trying to serialize point not on curve" 
    | _z pt == 0 = pack [0]
    | sgn0 y == 0 = cons 0x02 (toBytesF x)
    | otherwise   = cons 0x03 (toBytesF x)
    where  -- recover affine coordinates from original projective coordinates
      x = _x pt * inv0 (_z pt)
      y = _y pt * inv0 (_z pt)


-- | The `Curve` class provides the elliptic point multiplication operation involving
-- one `CurvePt` point on an elliptic curve and another `Field` field element as the
-- scalar operand. It also provides the `mapToCurveSimpleSwu` which is used in the later
-- stages of hashing-to-curve. It supports both the Pallas and Vesta curve point type.
class (CurvePt a, Field b) => Curve a b where

  -- | The `pointMul` function multiplies a field element by a prime-order curve point. 
  -- This, for example, could be a `PastaCurves.Fq` field element scalar with a 
  -- `PastaCurves.Pallas` elliptic curve point (which happens to use `PastaCurves.Fp` 
  -- co-ordinates). 
  pointMul :: b -> a -> a

  -- The `mapToCurveSimpleSwu` is a simplistic implementation of the Simplified 
  -- Shallue-van de Woestijne-Ulas method maps a field element to a curve point.
  -- See https://www.ietf.org/archive/id/draft-irtf-cfrg-hash-to-curve-14.html#name-simplified-shallue-van-de-w
  -- It requires A*B != 0 and a special constant Z (see link).
  mapToCurveSimpleSwu :: b -> b -> a


instance (Field f1, Field f2, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  Curve (Point a b baseX baseY f1) f2 where


  -- Classic double and add algorithm; will add a dedicated point double routine in the future
  pointMul s pt = pointMul' s pt neutral
    where
      pointMul' :: f2 -> Point a b baseX baseY f1 -> Point a b baseX baseY f1 -> Point a b baseX baseY f1
      pointMul' scalar p1 accum
        | scalar == 0 = accum  -- scalar is a field element so cannot go 'below zero'
        | sgn0 scalar /= 0 = pointMul' (shiftR1 scalar) doublePt (pointAdd accum p1)
        | sgn0 scalar == 0 = pointMul' (shiftR1 scalar) doublePt accum
        | otherwise = error "pointMul' pattern match fail (should never happen)"
        where
          doublePt = pointAdd p1 p1

  
  -- Z is Pasta-specific (constant is calculated elsewhere)
  mapToCurveSimpleSwu fu fz = if A * B /= 0 then result else error "Curve params A*B must not be zero"
    where
      u = (fromInteger $ toI fu)  :: f1  -- pesky type conversion 
      z = (fromInteger $ toI fz)  :: f1
      -- See https://www.ietf.org/archive/id/draft-irtf-cfrg-hash-to-curve-14.html#section-6.6.2-7
      tv1 = inv0 (z ^ (2 :: Integer) * u ^ (4 :: Integer) + z * u ^ (2 :: Integer))
      x1a = (fromInteger ((-1) * B) * inv0 (fromInteger (A))) * (1 + tv1)
      x1 = if toI tv1 == 0 then fromInteger (B) * inv0 (z * fromInteger (A))  else x1a 
      gx1 = x1 ^ (3 :: Integer) + fromInteger (A) * x1 + fromInteger (B)
      x2 = z * u ^ (2 :: Integer) * x1
      gx2 = x2 ^ (3 :: Integer) + fromInteger (A) * x2 + fromInteger (B)
      (x, ya) = if isSqr gx1 then (x1, fromJust $ sqrt gx1) else (x2, fromJust $ sqrt gx2)
      y = if sgn0 u /= sgn0 ya then -ya else ya
      result = Projective x y 1 :: Point a b baseX baseY f1
