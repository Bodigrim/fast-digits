# fast-digits

The fastest Haskell library to split integers into digits.
Usually it is at least 2x faster than [Data.Digits](https://hackage.haskell.org/package/digits-0.2)
and for small bases and huge numbers it may be up to 40 times faster.

Here are some benchmarks:

```
> cabal bench
benchmarking shortInt/FastDigits  base 2
time                 9.327 ms   (9.186 ms .. 9.503 ms)
benchmarking shortInt/Data.Digits base 2
time                 65.39 ms   (55.06 ms .. 77.73 ms)

benchmarking shortInt/FastDigits  base 10
time                 4.974 ms   (4.875 ms .. 5.075 ms)
benchmarking shortInt/Data.Digits base 10
time                 16.67 ms   (16.41 ms .. 16.89 ms)

benchmarking shortInt/FastDigits  base 10^5
time                 1.320 ms   (1.308 ms .. 1.335 ms)
benchmarking shortInt/Data.Digits base 10^5
time                 4.032 ms   (3.963 ms .. 4.099 ms)

benchmarking shortInt/FastDigits  base 10^9
time                 1.073 ms   (1.063 ms .. 1.086 ms)
benchmarking shortInt/Data.Digits base 10^9
time                 3.250 ms   (3.166 ms .. 3.372 ms)

benchmarking mediumInt/FastDigits  base 2
time                 1.470 ms   (1.442 ms .. 1.504 ms)
benchmarking mediumInt/Data.Digits base 2
time                 11.90 ms   (11.76 ms .. 12.04 ms)

benchmarking mediumInt/FastDigits  base 10
time                 806.2 μs   (796.9 μs .. 815.8 μs)
benchmarking mediumInt/Data.Digits base 10
time                 3.716 ms   (3.658 ms .. 3.774 ms)

benchmarking mediumInt/FastDigits  base 10^5
time                 287.5 μs   (285.6 μs .. 290.0 μs)
benchmarking mediumInt/Data.Digits base 10^5
time                 791.8 μs   (782.9 μs .. 804.8 μs)

benchmarking mediumInt/FastDigits  base 10^9
time                 209.3 μs   (207.3 μs .. 211.4 μs)
benchmarking mediumInt/Data.Digits base 10^9
time                 458.8 μs   (450.7 μs .. 468.4 μs)

benchmarking longInt/FastDigits  base 2
time                 4.225 ms   (4.165 ms .. 4.292 ms)
benchmarking longInt/Data.Digits base 2
time                 190.1 ms   (185.0 ms .. 195.2 ms)

benchmarking longInt/FastDigits  base 10
time                 3.499 ms   (3.454 ms .. 3.561 ms)
benchmarking longInt/Data.Digits base 10
time                 56.45 ms   (56.08 ms .. 57.08 ms)

benchmarking longInt/FastDigits  base 10^5
time                 3.832 ms   (3.766 ms .. 3.916 ms)
benchmarking longInt/Data.Digits base 10^5
time                 11.89 ms   (11.57 ms .. 12.22 ms)

benchmarking longInt/FastDigits  base 10^9
time                 3.376 ms   (3.250 ms .. 3.518 ms)
benchmarking longInt/Data.Digits base 10^9
time                 6.286 ms   (6.196 ms .. 6.388 ms)
```
