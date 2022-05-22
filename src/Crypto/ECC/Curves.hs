{-|
Module      : Crypto.PastaCurves.Curves
Description : Supports the instantiation of parameterized elliptic curves.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This internal module provides an elliptic curve (multi-use) template with a arbitrary
paramaters along with a variety of supporting functionality such as point addition, 
multiplication,  negation, serialization and deserialization. The algorithms are NOT 
constant time.
-}

{-# LANGUAGE CPP, DataKinds, DerivingStrategies, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, ScopedTypeVariables, Safe #-}

module Curves (Curve(..), CurvePt(..), Point(..)) where

import Prelude hiding (drop, length, sqrt)
import Data.ByteString (ByteString, cons, drop, index, length, pack)
import Data.Typeable (Proxy (Proxy))
import GHC.TypeLits (Nat, natVal, KnownNat)
import Fields (Field (..))
import Data.Maybe (fromJust)
import Control.Monad (mfilter)


data Point (a::Nat) (b::Nat) (baseX::Nat) (baseY::Nat) f =
            Projective {_px :: f, _py :: f, _pz :: f} -- (x * inv0 z, y * inv0 z)
            | Affine {_ax :: f, _ay :: f}
            | PointAtInfinity deriving stock (Show)


-- CPP macro 'helpers' to extract the curve parameters from `Point a b baseX baseY f`
#define A natVal (Proxy :: Proxy a)
#define B natVal (Proxy :: Proxy b)
#define BASE_X natVal (Proxy :: Proxy baseX)
#define BASE_Y natVal (Proxy :: Proxy baseY)


instance (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) =>
  Eq (Point a b baseX baseY f) where

  (==) (Affine x1 y1) (Affine x2 y2) = (x1 == x2) && (y1 == y2)
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _ = False
  (==) _ PointAtInfinity = False
  (==) pt1 pt2 = toAffine pt1 == toAffine pt2  -- one or both are projective


-- | The `CurvePt` class provides the bulk of the functionality related to operations
-- involving points on an elliptic curve. It supports both the Pallas and Vesta curve
-- point type.
class CurvePt a where

  -- | Returns the (constant) base point
  base :: a
  
  -- | The `fromBytesC` function deserializes a point
  fromBytesC :: ByteString -> Maybe a

  -- | The `isOnCurve` function validates whether the point is on the curve. It is 
  -- already utilized within `toBytesC` deserialization and within hash-to-curve (for
  -- redundant safety).
  isOnCurve :: a -> Bool

  -- | The `negatePt` function negates a point
  negatePt :: a -> a

  -- | Returns the (constant) neutral point
  neutral :: a

  -- | The `pointAdd` function adds two curve points
  pointAdd :: a -> a -> a

  -- | The `toAffine` function converts to an affine representation. Revist: necessary to expose?
  toAffine :: a -> a

  -- | The `toBytesC` function serializes a point
  toBytesC :: a -> ByteString

  -- | The `toProjective` function coverts to a projective representation. Revisit: necessary to expose?
  toProjective :: a -> a


instance (Field f, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) =>
  CurvePt (Point a b baseX baseY f) where

  base = Projective (fromInteger $ BASE_X) (fromInteger $ BASE_Y) 1


  fromBytesC bytes
    | length bytes == 1 && index bytes 0 == 0 = Just PointAtInfinity
    | length bytes == expLen && (index bytes 0 == 0x2 || index bytes 0 == 0x03) = result
        where
         expLen = 1 + length (toBytesF (fromInteger (A) :: f))
         x = fromBytesF (drop 1 bytes) :: Maybe f
         sgn0y = if index bytes 0 == 0x02 then 0 else 1::Integer
         alpha = (\t -> t^(3::Integer) + ((fromInteger $ A)::f) * t + ((fromInteger $ B)::f)) <$> x
         beta = alpha >>= sqrt
         y =  (\t -> if sgn0 t == sgn0y then t else negate t) <$> beta
         proposed = (Affine <$> x <*> y) :: Maybe (Point a b baseX baseY f)
         result = mfilter isOnCurve proposed :: Maybe (Point a b baseX baseY f)
  fromBytesC _ = Nothing


  isOnCurve PointAtInfinity = True
  isOnCurve (Affine x y) = y^(2::Integer) == x^(3::Integer) + fromInteger (A) * x + fromInteger (B)
  isOnCurve pt = isOnCurve $ toAffine pt


  negatePt (Projective x y z) = Projective x (- y) z
  negatePt PointAtInfinity = PointAtInfinity
  negatePt a = negatePt $ toProjective a


  neutral = Projective 0 1 0


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
  pointAdd PointAtInfinity pt2 = pt2
  pointAdd pt1 PointAtInfinity = pt1
  pointAdd pt1 pt2 = pointAdd (toProjective pt1) (toProjective pt2)


  toAffine (Projective _ _ 0) = PointAtInfinity
  toAffine (Projective x y z) = Affine (x * inv0 z) (y * inv0 z)
  toAffine (Affine x y) = Affine x y
  toAffine PointAtInfinity = PointAtInfinity


  -- Compressed; section 2.3.3 on page 10 of https://www.secg.org/sec1-v2.pdf
  toBytesC PointAtInfinity = pack [0]
  toBytesC (Affine x y)
    | sgn0 y == 0 = cons 0x02 (toBytesF x)
    | otherwise   = cons 0x03 (toBytesF x)
  toBytesC pt = toBytesC (toAffine pt)


  toProjective (Projective x y z) = Projective x y z
  toProjective (Affine x y) = Projective x y 1
  toProjective PointAtInfinity = Projective 0 1 0


-- | The `Curve` class provides the elliptic point multiplication operation involving
-- one `CurvePt` point on an elliptic curve and another `Field` field element as the
-- scalar operand. It supports both the Pallas and Vesta curve point type.
class (CurvePt a, Field b) => Curve a b where

  -- | The `pointMul` function multiplies a field element by a curve point. This, for 
  -- example, could be a `PastaCurves.Fq` field element scalar with a 
  -- `PastaCurves.Pallas` elliptic curve point (which happens to use `PastaCurves.Fp` 
  -- co-ordinates). 
  pointMul :: b -> a -> a

  mapToCurveSimpleSwu :: b -> a


instance (Field f1, Field f2, KnownNat a, KnownNat b, KnownNat baseX, KnownNat baseY) => 
  Curve (Point a b baseX baseY f1) f2 where

  -- See https://eprint.iacr.org/2015/1060.pdf page 8; The following has all the additions 'squashed out'
  -- Algorithm 1: Complete, projective point addition for arbitrary prime order short Weierstrass curves E/Fq : y^2 = x^3 + ax + b.
  pointMul s pt = pointMul' s pt neutral
    where
      pointMul' :: f2 -> Point a b baseX baseY f1 -> Point a b baseX baseY f1 -> Point a b baseX baseY f1
      pointMul' scalar p1 accum
        | scalar == 0 = accum
        | sgn0 scalar /= 0 = pointMul' (shiftR1 scalar) doublePt (pointAdd accum p1)
        | sgn0 scalar == 0 = pointMul' (shiftR1 scalar) doublePt accum
        | otherwise = error "pointMul' pattern match fail (should never happen)"
        where
          doublePt = pointAdd p1 p1


  -- mapToCurveSimpleSwu :: f -> Point a b baseX baseY f
  -- z from https://github.com/eschorn1/zero11/blob/master/curves.py#L172 ;; this is Pasta-specific
  mapToCurveSimpleSwu fu = if A * B /= 0 then result else error "curve params A*B must not be zero"
    where
      u = (fromInteger $ toI fu)  :: f1
      z = fromInteger (-13) :: f1
      -- See https://www.ietf.org/archive/id/draft-irtf-cfrg-hash-to-curve-13.html#section-6.6.2-7
      tv1 = inv0 (z^(2::Integer) * u^(4::Integer) + z * u^(2::Integer))
      x1a = (fromInteger ((-1) * B) * inv0 (fromInteger (A))) * (1 + tv1) :: f1
      x1 = if toI tv1 == 0 then fromInteger (B) * inv0 (z * fromInteger (A))  else x1a :: f1 
      gx1 = x1^(3::Integer) + fromInteger (A) * x1 + fromInteger (B) :: f1
      x2 = z * u^(2::Integer) * x1 :: f1
      gx2 = x2^(3::Integer) + fromInteger (A) * x2 + fromInteger (B) :: f1
      (x, ya) = if isSqr gx1 then (x1, fromJust $ sqrt gx1) else (x2, fromJust $ sqrt gx2) :: (f1, f1)
      y = if sgn0 u /= sgn0 ya then -ya else ya :: f1
      result = Projective x y 1 :: Point a b baseX baseY f1
