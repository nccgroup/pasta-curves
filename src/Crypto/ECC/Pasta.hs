{-|
Module      : Crypto.PastaCurves.Pasta (internal)
Description : Pasta-specific instantiation of parameterized curves and fields.
Copyright   : (c) Eric Schorn, 2022
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental
Portability : GHC
SPDX-License-Identifier: MIT

This internal module instantiates the specific curves and fields specific to Pasta. It
also includes `hashToPallas` and `hashToVesta` functionality (which in turn includes
two isogenous curves, mapping functionality, and coefficient vectors). The algorithms 
are NOT constant time; Safety and simplicity are the top priorities.
-}

{-# LANGUAGE DataKinds, NoImplicitPrelude, ScopedTypeVariables, Safe #-}

module Pasta (Fp, Fq, Num(..), Pallas, Vesta, Curve(..), CurvePt(..), Field(..), 
  hashToPallas, hashToVesta, rndPallas, rndVesta, pallasPrime, vestaPrime) where

import Prelude
import Curves (Curve(..), CurvePt(..), Point(..))
import Fields (Fz, Field(..))
import Data.ByteString.UTF8 (ByteString)
import System.Random (RandomGen)


-- | `Fp` is the field element used as a coordinate in the Pallas elliptic curve.
-- It is a type synonym for the internal `Fields.Fz` type, parameterized with the 
-- correct modulus. It is also typically used as a scalar to multiply a Vesta elliptic
-- curve point. Note that pointMul does not enforce specific scalar/point combinations.
type Fp  = Fz 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001


-- | `Pallas` represents a point on the Pallas elliptic curve using `Fp` coordinates.
-- The curve was designed to have the some order as the `Fq` element\'s modulus. It is
-- a type synonym for the internal `Curves.Point` type, parameterized with the curve\s 
-- @a@ and @b@ values and the affine base point as @base_x@ and @base_y@. The underlying
-- point is of type @Point a b base_x base_y field@.
type Pallas = (Point 0 5 1 0x248b4a5cf5ed6c83ac20560f9c8711ab92e13d27d60fb1aa7f5db6c93512d546 Fp)


-- | `Fq` is the field element used as a coordinate in the Vesta elliptic curve.
-- It is a type synonym for the internal `Fields.Fz` type, parameterized with the 
-- correct modulus. It is also typically used as a scalar to multiply a Pallas elliptic 
-- curve point. Note that pointMul does not enforce specific scalar/point combinations.
type Fq = Fz 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001


-- | `Vesta` represents a point on the Vesta elliptic curve using `Fq` coordinates.
-- The curve was designed to have the some order as the `Fp` element\'s modulus.  It is
-- a type synonym for the internal `Curves.Point` type, parameterized with the curve\s 
-- @a@ and @b@ values and the affine base point as @base_x@ and @base_y@.  The underlying
-- point is of type @Point a b base_x base_y field@.
type Vesta  = (Point 0 5 1 0x26bc999156dd5194ec49b1c551768ab375785e7ce00906d13e0361674fd8959f Fq)


-- This is a curve that is isogenous to Pallas, but with a*b != 0; base point params are unused
type IsoPallas = (Point 0x18354a2eb0ea8c9c49be2d7258370742b74134581a27a59f92bb4b0b657a014b 1265 0 0 Fp)


-- This is a curve that is isogenous to Vesta, but with a*b != 0; base point params are unused
type IsoVesta = (Point 0x267f9b2ee592271a81639c4d96f787739673928c7d01b212c515ad7242eaa6b1 1265 0 0 Fq)


-- | The `hashToPallas` function takes an arbitrary `ByteString` and maps it to a valid 
-- point on the Pallas elliptic curve (of unknown relation to the base point).
hashToPallas :: String -> ByteString -> Pallas
hashToPallas domain_separator msg = result 
  where
    (fe0, fe1) = hash2Field msg domain_separator "pallas" :: (Fp, Fp)
    q0 = mapToCurveSimpleSwu fe0 (fromInteger (-13)) :: IsoPallas  -- -13 is Pasta specific magic constant
    q1 = mapToCurveSimpleSwu fe1 (fromInteger (-13)) :: IsoPallas
    (Projective xp yp zp) = pointAdd q0 q1 :: IsoPallas
    x = xp * inv0 zp ;  y = yp * inv0 zp
    xTop = head isoPallasVecs * x ^ (3::Integer) + isoPallasVecs !! 1 * x ^ (2::Integer) + isoPallasVecs !! 2 * x + isoPallasVecs !! 3
    xBot = x ^ (2::Integer) + isoPallasVecs !! 4 * x + isoPallasVecs !! 5
    yTop = isoPallasVecs !! 6 * x ^ (3::Integer) + isoPallasVecs !! 7 * x ^ (2::Integer) + isoPallasVecs !! 8 * x + isoPallasVecs !! 9
    yBot = x ^ (3::Integer) + isoPallasVecs !! 10 * x ^ (2::Integer) + isoPallasVecs !! 11 * x + isoPallasVecs !! 12 
    proposed = Projective (xTop * inv0 xBot) (y * yTop * inv0 yBot) 1 :: Pallas
    result = if isOnCurve proposed then proposed else error "hashed to Pallas non-point"

-- | The `hashToVesta` function takes an arbitrary `ByteString` and maps it to a valid 
-- point on the Vesta elliptic curve (of unknown relation to the base point).
hashToVesta :: ByteString -> Vesta
hashToVesta msg = result 
  where
    (fe0, fe1) = hash2Field msg "z.cash:test" "vesta" :: (Fq, Fq)
    q0 = mapToCurveSimpleSwu fe0 (fromInteger (-13)) :: IsoVesta
    q1 = mapToCurveSimpleSwu fe1 (fromInteger (-13)) :: IsoVesta
    (Projective xp yp zp) = pointAdd q0 q1 :: IsoVesta
    x = xp * inv0 zp ;  y = yp * inv0 zp
    xTop = head isoVestaVecs * x ^ (3::Integer) + isoVestaVecs !! 1 * x ^ (2::Integer) + isoVestaVecs !! 2 * x + isoVestaVecs !! 3
    xBot = x ^ (2::Integer) + isoVestaVecs !! 4 * x + isoVestaVecs !! 5
    yTop = isoVestaVecs !! 6 * x ^ (3::Integer) + isoVestaVecs !! 7 * x ^ (2::Integer) + isoVestaVecs !! 8 * x + isoVestaVecs !! 9
    yBot = x ^ (3::Integer) + isoVestaVecs !! 10 * x ^ (2::Integer) + isoVestaVecs !! 11 * x + isoVestaVecs !! 12 
    proposed = Projective (xTop * inv0 xBot) (y * yTop * inv0 yBot) 1 :: Vesta
    result = if isOnCurve proposed then proposed else error "hashed to Vesta non-point"


-- | The `rndPallas` function returns a random Pallas point when given a StdGen.
rndPallas :: forall g. RandomGen g => g -> (g, Pallas)
rndPallas rndGen = hashToPallas "z.cash:test" . toBytesF <$> (rndF rndGen :: (g, Fq))


-- | The `rndVesta` function returns a random Vests point when given a StdGen.
rndVesta :: forall g. RandomGen g => g -> (g, Vesta)
rndVesta rndGen = hashToVesta . toBytesF <$> (rndF rndGen :: (g, Fp))


-- | The Pallas field modulus https://neuromancer.sk/std/other/Pallas
pallasPrime :: Integer
pallasPrime = 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001


-- | The Vesta field modulus https://neuromancer.sk/std/other/Vesta
vestaPrime :: Integer
vestaPrime = 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001


-- Vectors to map isogenous curve (with a*b != 0) point to Pallas
-- See https://github.com/zcash/pasta_curves/blob/21fd9e2c1bbd2d049bfe95588d77cb884e9f93ab/src/curves.rs#L1017-L1096
isoPallasVecs :: [Fp]
isoPallasVecs = [
  0x0e38e38e38e38e38e38e38e38e38e38e4081775473d8375b775f6034aaaaaaab,
  0x3509afd51872d88e267c7ffa51cf412a0f93b82ee4b994958cf863b02814fb76,
  0x17329b9ec525375398c7d7ac3d98fd13380af066cfeb6d690eb64faef37ea4f7,
  0x1c71c71c71c71c71c71c71c71c71c71c8102eea8e7b06eb6eebec06955555580,
  0x1d572e7ddc099cff5a607fcce0494a799c434ac1c96b6980c47f2ab668bcd71f,
  0x325669becaecd5d11d13bf2a7f22b105b4abf9fb9a1fc81c2aa3af1eae5b6604,
  0x1a12f684bda12f684bda12f684bda12f7642b01ad461bad25ad985b5e38e38e4,
  0x1a84d7ea8c396c47133e3ffd28e7a09507c9dc17725cca4ac67c31d8140a7dbb,
  0x3fb98ff0d2ddcadd303216cce1db9ff11765e924f745937802e2be87d225b234,
  0x025ed097b425ed097b425ed097b425ed0ac03e8e134eb3e493e53ab371c71c4f,
  0x0c02c5bcca0e6b7f0790bfb3506defb65941a3a4a97aa1b35a28279b1d1b42ae,
  0x17033d3c60c68173573b3d7f7d681310d976bbfabbc5661d4d90ab820b12320a,
  0x40000000000000000000000000000000224698fc094cf91b992d30ecfffffde5]


-- Vectors to map isogenous curve (with a*b != 0) point to Vesta
-- Curve isogenous to Vesta, with a*b != 0
-- See https://github.com/zcash/pasta_curves/blob/21fd9e2c1bbd2d049bfe95588d77cb884e9f93ab/src/curves.rs#L1117-L1196
isoVestaVecs :: [Fq]
isoVestaVecs = [
  0x38e38e38e38e38e38e38e38e38e38e390205dd51cfa0961a43cd42c800000001,
  0x1d935247b4473d17acecf10f5f7c09a2216b8861ec72bd5d8b95c6aaf703bcc5,
  0x18760c7f7a9ad20ded7ee4a9cdf78f8fd59d03d23b39cb11aeac67bbeb586a3d,
  0x31c71c71c71c71c71c71c71c71c71c71e1c521a795ac8356fb539a6f0000002b,
  0x0a2de485568125d51454798a5b5c56b2a3ad678129b604d3b7284f7eaf21a2e9,
  0x14735171ee5427780c621de8b91c242a30cd6d53df49d235f169c187d2533465,
  0x12f684bda12f684bda12f684bda12f685601f4709a8adcb36bef1642aaaaaaab,
  0x2ec9a923da239e8bd6767887afbe04d121d910aefb03b31d8bee58e5fb81de63,
  0x19b0d87e16e2578866d1466e9de10e6497a3ca5c24e9ea634986913ab4443034,
  0x1ed097b425ed097b425ed097b425ed098bc32d36fb21a6a38f64842c55555533,
  0x2f44d6c801c1b8bf9e7eb64f890a820c06a767bfc35b5bac58dfecce86b2745e,
  0x3d59f455cafc7668252659ba2b546c7e926847fb9ddd76a1d43d449776f99d2f,
  0x40000000000000000000000000000000224698fc0994a8dd8c46eb20fffffde5]
