{-# LANGUAGE CPP #-}

module Main (main) where

import Prelude hiding (Foldable(..))
#ifdef MIN_VERSION_digits
import qualified Data.Digits as D (digitsRev)
#endif
import Data.FastDigits (digits, undigits)
import Data.Foldable
import Test.Tasty.Bench (Benchmark, Benchmarkable, defaultMain, bench, bgroup, nf)
#ifdef MIN_VERSION_digits
import Test.Tasty.Bench (bcompare)
#endif

#ifdef MIN_VERSION_digits
digitsD :: Int -> Integer -> [Int]
digitsD base n = map fromInteger $ D.digitsRev (toInteger base) n
#endif

intNs :: Int -> Int -> [Int]
intNs from len = [from, step .. maxBound]
  where
    step = maxBound `div` len

integerN :: Int -> Int -> Integer
integerN from len = undigits (maxBound :: Int) (intNs from len)

benchShort :: (Integer -> [Int]) -> Benchmarkable
benchShort f = flip nf 10000 $
  \len -> foldl' (+) 0 $ concatMap (f . toInteger)
    [1 :: Int, maxBound `quot` len .. maxBound]
{-# INLINE benchShort #-}

benchMedium :: (Integer -> [Int]) -> Benchmarkable
benchMedium f = flip nf 100 $
  \len -> foldl' (+) 0 $ concatMap (f . flip integerN 10) [1 :: Int .. len]
{-# INLINE benchMedium #-}

benchLong :: (Integer -> [Int]) -> Benchmarkable
benchLong f = flip nf 1000 $
  \len -> foldl' (+) 0 $ f $ integerN 1 len
{-# INLINE benchLong #-}

benchBase :: ((Integer -> [Int]) -> Benchmarkable) -> String -> Int -> Benchmark
#ifdef MIN_VERSION_digits
benchBase b groupName base = bgroup (show base)
  [ bench "FastDigits" (b (digits base))
  , bcompare ("$NF == \"FastDigits\" && $(NF-1) == \"" ++ show base ++ "\" && $(NF-2) == \"" ++ groupName ++ "\"")
  $ bench "Data.Digits" (b (digitsD base))
  ]
#else
benchBase b _ base = bench (show base) (b (digits base))
#endif
{-# INLINE benchBase #-}

benchSmth :: String -> ((Integer -> [Int]) -> Benchmarkable) -> Benchmark
benchSmth name b = bgroup name $
  [ benchBase b name 2
  , benchBase b name 10
  , benchBase b name 100000
  , benchBase b name 1000000000
  ]
{-# INLINE benchSmth #-}

main :: IO ()
main = defaultMain
  [ benchSmth "short"  benchShort
  , benchSmth "medium" benchMedium
  , benchSmth "long"   benchLong
  ]
