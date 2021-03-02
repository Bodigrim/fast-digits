{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef MIN_VERSION_digits
import qualified Data.Digits as D (digitsRev)
#endif
import Data.FastDigits (digits, undigits)
import Data.List (foldl')
import Test.Tasty.Bench (Benchmark, defaultMain, bench, bgroup, nf)

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

integerNs :: Int -> Int -> [Integer]
integerNs llen len = map (\i -> integerN i len) [1..llen]

benchShort :: String -> (Integer -> [Int]) -> Benchmark
benchShort name f = bench name $ flip nf 10000 $
  \len -> foldl' (+) 0 $ concatMap (f . toInteger)
    [1 :: Int, maxBound `quot` len .. maxBound]
{-# INLINE benchShort #-}

benchMedium :: String -> (Integer -> [Int]) -> Benchmark
benchMedium name f = bench name $ flip nf 100 $
  \len -> foldl' (+) 0 $ concatMap (f . flip integerN 10) [1 :: Int .. len]
{-# INLINE benchMedium #-}

benchLong :: String -> (Integer -> [Int]) -> Benchmark
benchLong name f = bench name $ flip nf 1000 $
  \len -> foldl' (+) 0 $ f $ integerN 1 len
{-# INLINE benchLong #-}

benchBase :: (String -> (Integer -> [Int]) -> Benchmark) -> Int -> Benchmark
#ifdef MIN_VERSION_digits
benchBase b base = bgroup (show base)
  [ b "FastDigits" (digits base)
  , b "Digits" (digitsD base)
  ]
#else
benchBase b base = b (show base) (digits base)
#endif
{-# INLINE benchBase #-}

benchSmth :: String -> (String -> (Integer -> [Int]) -> Benchmark) -> Benchmark
benchSmth name b = bgroup name $
  [ benchBase b 2
  , benchBase b 10
  , benchBase b (10^5)
  , benchBase b (10^9)
  ]
{-# INLINE benchSmth #-}

main :: IO ()
main = defaultMain
  [ benchSmth "short"  benchShort
  , benchSmth "medium" benchMedium
  , benchSmth "long"   benchLong
  ]
