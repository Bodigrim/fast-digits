{-|
Module      : Data.FastDigits
Description : The fast library for integer-to-digits conversion.
Copyright   : (c) Andrew Lelechenko, 2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental
Portability : POSIX

Convert an integer to digits and back.
Usually this library is twice as fast as "Data.Digits".
For small bases and long numbers it may be up to 30 times faster.
-}

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -O2                      #-}
{-# OPTIONS_GHC -optc-O3                 #-}

module Data.FastDigits
  ( digits
  , undigits
  ) where

import GHC.Exts
import GHC.Integer

ti :: Integral a => a -> Integer
ti = toInteger

fi :: Num a => Integer -> a
fi = fromInteger

-- | First argument should be below maxBound :: Int.
digitsInteger :: Integer -> Integer -> [Int]
digitsInteger base = f
  where
    f 0 = []
    f n = let (# q, r #) = n `quotRemInteger` base in fi r : f q

digitsInt :: Int# -> Int -> [Int]
digitsInt base (I# m) = f m
  where
    f :: Int# -> [Int]
    f 0# = []
    f n = let (# q, r #) = n `quotRemInt#` base in I# r : f q

-- | For a given base and expected length of list of digits
--   return the list of digits and padding till expected length.
digitsIntL :: Int# -> Int# -> Int -> (# [Int], Int# #)
digitsIntL base power (I# m) = f m
  where
    f :: Int# -> (# [Int], Int# #)
    f 0# = (# [], power #)
    f n = (# I# r : fq, lq -# 1# #)
      where
        (# q, r #) = n `quotRemInt#` base
        (# fq, lq #) = f q

-- | For a given base, power and precalculated base^power
--   take an integer and return the list of its digits.
digitsInteger' :: Int# -> Int# -> Integer -> Integer -> [Int]
digitsInteger' base power poweredBase = f
  where
    f :: Integer -> [Int]
    f n = case  n `quotRemInteger` poweredBase of
      (# 0, _ #) -> digitsInt base (fi n)
      (# q, r #) -> fr ++ replicate (I# lr) 0 ++ f q
                    where
                      (# fr, lr #) = digitsIntL base power (fi r)

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower :: Int# -> (# Int#, Int# #)
selectPower base = -- if I# poweredBase' > 0
    --then
    (# power', poweredBase' #)
    --else (# power,  poweredBase  #)
  where
    !(I# maxB) = maxBound

    power = double2Int# (logDouble# (int2Double# base) /## logDouble# (int2Double# maxB))
    power' = power +# 1#

    -- !(I# poweredBase) = (I# base) ^ (I# power)
    !(I# poweredBase') = (I# base) ^ (I# power')

-- | Return digits of a non-negative integer in reverse order. E. g.,
--
--   > digits 10 123 = [3, 2, 1]
--   > digits 10 0   = []
--
-- Throw an error if number is negative or base is below 2.
digits
  :: Int     -- ^ The base to use
  -> Integer -- ^ The integer to convert
  -> [Int]
digits base@(I# base') n
  | base < 2  = error "Base must be > 1"
  | n < 0     = error "Number must be non-negative"
  | n < ti (maxBound :: Int) = digitsInt base' (fi n)
  | otherwise = if I# power == 1
                  then digitsInteger (ti base) n
                  else digitsInteger' base' power (ti $ I# poweredBase) n
  where
    (# power, poweredBase #) = selectPower base'

-- | Return an integer, built from given digits in reverse order.
--   Condition 0 <= digit < base is not checked.
undigits :: (Integral a, Integral b)
         => a       -- ^ The base to use
         -> [b]     -- ^ The list of digits to convert
         -> Integer
undigits base' = foldr (\d acc -> acc * base + ti d) 0
  where
    base = ti base'
{-# SPECIALIZE undigits :: Word -> [Word] -> Integer #-}
{-# SPECIALIZE undigits :: Int -> [Int] -> Integer #-}
{-# SPECIALIZE undigits :: Integer -> [Integer] -> Integer #-}
