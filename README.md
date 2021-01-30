# fast-digits [![Hackage](http://img.shields.io/hackage/v/fast-digits.svg)](https://hackage.haskell.org/package/fast-digits) [![Stackage LTS](http://stackage.org/package/fast-digits/badge/lts)](http://stackage.org/lts/package/fast-digits) [![Stackage Nightly](http://stackage.org/package/fast-digits/badge/nightly)](http://stackage.org/nightly/package/fast-digits)

The fastest Haskell library to split integers into digits.
It is both asymptotically (O(n<sup>1.4</sup>) vs. O(n<sup>2</sup>))
and practically (2x-40x for typical inputs)
faster than [Data.Digits](https://hackage.haskell.org/package/digits).

Here are some benchmarks:

```
> cabal bench
shortInt/FastDigits  base 2              mean 6.429 ms  ( +- 465.7 μs  )
shortInt/Data.Digits base 2              mean 50.08 ms  ( +- 1.848 ms  )

shortInt/FastDigits  base 10             mean 4.288 ms  ( +- 217.1 μs  )
shortInt/Data.Digits base 10             mean 15.62 ms  ( +- 540.1 μs  )

shortInt/FastDigits  base 10^5           mean 1.142 ms  ( +- 50.09 μs  )
shortInt/Data.Digits base 10^5           mean 3.962 ms  ( +- 269.1 μs  )

shortInt/FastDigits  base 10^9           mean 963.8 μs  ( +- 46.13 μs  )
shortInt/Data.Digits base 10^9           mean 3.052 ms  ( +- 238.5 μs  )

mediumInt/FastDigits  base 2             mean 1.213 ms  ( +- 185.0 μs  )
mediumInt/Data.Digits base 2             mean 12.41 ms  ( +- 3.417 ms  )

mediumInt/FastDigits  base 10            mean 689.3 μs  ( +- 32.43 μs  )
mediumInt/Data.Digits base 10            mean 3.271 ms  ( +- 137.3 μs  )

mediumInt/FastDigits  base 10^5          mean 220.1 μs  ( +- 11.30 μs  )
mediumInt/Data.Digits base 10^5          mean 711.1 μs  ( +- 67.31 μs  )

mediumInt/FastDigits  base 10^9          mean 156.0 μs  ( +- 9.115 μs  )
mediumInt/Data.Digits base 10^9          mean 407.9 μs  ( +- 19.58 μs  )

longInt/FastDigits  base 2               mean 3.515 ms  ( +- 189.7 μs  )
longInt/Data.Digits base 2               mean 183.2 ms  ( +- 4.564 ms  )

longInt/FastDigits  base 10              mean 2.164 ms  ( +- 134.7 μs  )
longInt/Data.Digits base 10              mean 55.45 ms  ( +- 946.7 μs  )

longInt/FastDigits  base 10^5            mean 1.467 ms  ( +- 60.49 μs  )
longInt/Data.Digits base 10^5            mean 11.09 ms  ( +- 347.0 μs  )

longInt/FastDigits  base 10^9            mean 1.242 ms  ( +- 52.92 μs  )
longInt/Data.Digits base 10^9            mean 6.181 ms  ( +- 226.1 μs  )
```
