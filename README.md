# fast-digits [![Hackage](http://img.shields.io/hackage/v/fast-digits.svg)](https://hackage.haskell.org/package/fast-digits) [![Stackage LTS](http://stackage.org/package/fast-digits/badge/lts)](http://stackage.org/lts/package/fast-digits) [![Stackage Nightly](http://stackage.org/package/fast-digits/badge/nightly)](http://stackage.org/nightly/package/fast-digits)

The fastest Haskell library to split integers into digits.
It is both asymptotically (O(n<sup>1.4</sup>) vs. O(n<sup>2</sup>))
and practically (2x-40x for typical inputs)
faster than [Data.Digits](https://hackage.haskell.org/package/digits).

Here are benchmarks for GHC 8.10:

```
> cabal bench -w ghc-8.10.4
All
  short
    2
      FastDigits:  OK (3.11s)
        12.3 ms ± 701 μs
      Data.Digits: OK (1.41s)
        22.2 ms ± 1.8 ms, 1.81x
    10
      FastDigits:  OK (2.11s)
        4.16 ms ± 369 μs
      Data.Digits: OK (3.74s)
        7.40 ms ± 235 μs, 1.78x
    100000
      FastDigits:  OK (4.89s)
        1.20 ms ±  69 μs
      Data.Digits: OK (3.96s)
        1.95 ms ±  78 μs, 1.63x
    1000000000
      FastDigits:  OK (4.02s)
        985  μs ±  62 μs
      Data.Digits: OK (3.15s)
        1.54 ms ±  70 μs, 1.56x
  medium
    2
      FastDigits:  OK (3.02s)
        1.49 ms ±  66 μs
      Data.Digits: OK (1.42s)
        5.62 ms ± 542 μs, 3.77x
    10
      FastDigits:  OK (2.35s)
        571  μs ±  42 μs
      Data.Digits: OK (1.77s)
        1.76 ms ± 152 μs, 3.07x
    100000
      FastDigits:  OK (3.87s)
        238  μs ±  19 μs
      Data.Digits: OK (3.44s)
        419  μs ±  23 μs, 1.76x
    1000000000
      FastDigits:  OK (3.05s)
        186  μs ±  13 μs
      Data.Digits: OK (4.42s)
        268  μs ±  11 μs, 1.44x
  long
    2
      FastDigits:  OK (3.75s)
        3.60 ms ± 215 μs
      Data.Digits: OK (1.89s)
        125  ms ± 9.6 ms, 34.88x
    10
      FastDigits:  OK (2.30s)
        2.24 ms ± 125 μs
      Data.Digits: OK (2.47s)
        39.0 ms ± 2.0 ms, 17.40x
    100000
      FastDigits:  OK (1.93s)
        1.88 ms ± 139 μs
      Data.Digits: OK (4.52s)
        8.82 ms ± 533 μs, 4.70x
    1000000000
      FastDigits:  OK (1.77s)
        1.71 ms ± 149 μs
      Data.Digits: OK (1.35s)
        5.30 ms ± 482 μs, 3.10x
```
