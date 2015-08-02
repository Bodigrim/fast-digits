# fast-digits

The fastest Haskell library to split integers into digits. 
Usually it is 2x-3x faster than [Data.Digits](https://hackage.haskell.org/package/digits-0.2)
and for small bases and huge numbers it may be up to 35 times faster.

Here are benchmarks:

```
> cabal bench
benchmarking shortInt/FastDigits  base 2   
time                 17.46 ms   (16.99 ms .. 18.13 ms)
benchmarking shortInt/Data.Digits base 2   
time                 50.16 ms   (49.45 ms .. 51.00 ms)

benchmarking shortInt/FastDigits  base 10  
time                 5.560 ms   (5.459 ms .. 5.638 ms)
benchmarking shortInt/Data.Digits base 10  
time                 16.03 ms   (15.10 ms .. 16.91 ms)

benchmarking shortInt/FastDigits  base 10^5
time                 1.530 ms   (1.511 ms .. 1.547 ms)
benchmarking shortInt/Data.Digits base 10^5
time                 4.197 ms   (3.939 ms .. 4.485 ms)

benchmarking shortInt/FastDigits  base 10^9
time                 1.245 ms   (1.227 ms .. 1.261 ms)
benchmarking shortInt/Data.Digits base 10^9
time                 3.041 ms   (3.002 ms .. 3.083 ms)

benchmarking mediumInt/FastDigits  base 2   
time                 2.793 ms   (2.775 ms .. 2.810 ms)
benchmarking mediumInt/Data.Digits base 2   
time                 12.87 ms   (12.78 ms .. 12.98 ms)

benchmarking mediumInt/FastDigits  base 10  
time                 1.047 ms   (1.034 ms .. 1.060 ms)
benchmarking mediumInt/Data.Digits base 10  
time                 3.647 ms   (3.603 ms .. 3.688 ms)

benchmarking mediumInt/FastDigits  base 10^5
time                 487.5 μs   (465.5 μs .. 519.3 μs)
benchmarking mediumInt/Data.Digits base 10^5
time                 838.1 μs   (800.0 μs .. 874.5 μs)

benchmarking mediumInt/FastDigits  base 10^9
time                 367.5 μs   (360.3 μs .. 375.6 μs)
benchmarking mediumInt/Data.Digits base 10^9
time                 477.1 μs   (471.4 μs .. 483.7 μs)

benchmarking longInt/FastDigits  base 2   
time                 5.267 ms   (5.207 ms .. 5.328 ms)
benchmarking longInt/Data.Digits base 2   
time                 185.2 ms   (182.1 ms .. 189.9 ms)

benchmarking longInt/FastDigits  base 10  
time                 3.781 ms   (3.726 ms .. 3.833 ms)
benchmarking longInt/Data.Digits base 10  
time                 54.46 ms   (53.49 ms .. 55.25 ms)

benchmarking longInt/FastDigits  base 10^5
time                 3.813 ms   (3.750 ms .. 3.872 ms)
benchmarking longInt/Data.Digits base 10^5
time                 10.95 ms   (10.78 ms .. 11.10 ms)

benchmarking longInt/FastDigits  base 10^9
time                 3.136 ms   (3.076 ms .. 3.183 ms)
benchmarking longInt/Data.Digits base 10^9
time                 6.056 ms   (5.981 ms .. 6.128 ms)
```
