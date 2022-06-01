# pasta-curves

[![Stack CI](https://github.com/nccgroup/pasta-curves/actions/workflows/stack.yml/badge.svg)](https://github.com/nccgroup/pasta-curves/actions/workflows/stack.yml)
[![Cabal CI](https://github.com/nccgroup/pasta-curves/actions/workflows/cabal.yml/badge.svg)](https://github.com/nccgroup/pasta-curves/actions/workflows/cabal.yml)
[![Hackage](https://img.shields.io/hackage/v/pasta-curves.svg?logo=haskell)](https://hackage.haskell.org/package/pasta-curves)
[![Stackage Lts](http://stackage.org/package/pasta-curves/badge/lts)](http://stackage.org/lts/package/pasta-curves)
[![Stackage Nightly](http://stackage.org/package/pasta-curves/badge/nightly)](http://stackage.org/nightly/package/pasta-curves)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)


This Haskell library provides the Pasta Curves consisting of: the `Pallas`
curve and its `Fp`  field element, the `Vesta` curve  and its `Fq` field 
element, and a variety of  supporting functionality such as point/element 
arithmetic, serialization, and hash-to-curve. The algorithms are NOT constant
time.

Pallas is y<sup>2</sup> = x<sup>3</sup> + 5 over F<sub>p</sub>(0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001).
The order of the Pallas curve is 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001.


Vesta is y<sup>2</sup> = x<sup>3</sup> + 5 over F<sub>q</sub>(0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001).
The order of the Vesta curve is 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001.

The curves are designed such that the order of one matches the field 
characteristic of the other. For a brief introduction, see the Zcash blog 
titled ["The Pasta Curves for Halo 2 and Beyond"](https://electriccoin.co/blog/the-pasta-curves-for-halo-2-and-beyond/).
The reference Rust implementation (which inspired this implementation) 
can be found at: <https://github.com/zcash/pasta_curves>.

Example usage of this library implementation:

~~~
$ cabal repl

ghci> a = 9 :: Fp

ghci> a*a
0x0000000000000000000000000000000000000000000000000000000000000051

ghci> pointMul a base :: Vesta
Projective {_px = 0x3CDC6A090F2BB3B52714C083929B620FE24ADBCBBD420752108CD7C29E543E5E, 
            _py = 0x08795CD330B3CE5AA63BD2B18DE155AE3C96E8AF9DA2CC742C6BA1464E490161, 
            _pz = 0x1FA26F58F3A641ADFE81775D3D53378D6178B6CCBF14F9BD4AB5F10DEE28D878}
~~~
