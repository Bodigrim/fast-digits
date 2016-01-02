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
  ( digitsUnsigned
  , digits
  , undigits
  ) where

import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import GHC.Natural
import Unsafe.Coerce

ti :: Integral a => a -> Integer
ti = toInteger

digitsNatural :: GmpLimb# -> BigNat -> [Word]
digitsNatural base = f
  where
    f n = if n == zeroBigNat
      then []
      else let (# q, r #) = n `quotRemBigNatWord` base in W# r : f q

digitsWord :: Word# -> Word# -> [Word]
digitsWord base = f
  where
    f :: Word# -> [Word]
    f 0## = []
    f n = let (# q, r #) = n `quotRemWord#` base in W# r : f q

-- | For a given base and expected length of list of digits
--   return the list of digits and padding till expected length.
digitsWordL :: Word# -> Word# -> Word# -> (# [Word], Word# #)
digitsWordL base power = f
  where
    f :: Word# -> (# [Word], Word# #)
    f 0## = (# [], power #)
    f n = (# W# r : fq, lq `minusWord#` 1## #)
      where
        (# q, r #) = n `quotRemWord#` base
        (# fq, lq #) = f q

-- | For a given base, power and precalculated base^power
--   take an integer and return the list of its digits.
digitsNatural' :: Word# -> Word# -> Word# -> BigNat -> [Word]
digitsNatural' base power poweredBase = f
  where
    f :: BigNat -> [Word]
    f n = let (# q, r #) = n `quotRemBigNatWord` poweredBase in
      if q == zeroBigNat
        then digitsWord base r
        else let (# fr, lr #) = digitsWordL base power r in
          fr ++ replicate (I# (word2Int# lr)) 0 ++ f q

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower :: Word# -> (# Word#, Word# #)
selectPower base = (# power, poweredBase #)
  where
    !(W# maxB) = maxBound
    power = int2Word# (integerLogBase# (wordToInteger base) (wordToInteger maxB))
    !(W# poweredBase) = (W# base) ^ (W# power)

-- | Return digits of a non-negative integer in reverse order. E. g.,
--
--   > digits 10 123 = [3, 2, 1]
--   > digits 10 0   = []
--
-- Throw an error if number is negative or base is below 2.
digitsUnsigned
  :: Word     -- ^ The base to use
  -> Natural -- ^ The integer to convert
  -> [Word]
digitsUnsigned (W# base) (NatS# n) = digitsWord base n
digitsUnsigned (W# base) (NatJ# n) = case power of
  1## -> digitsNatural base n
  _   -> digitsNatural' base power poweredBase n
  where
    (# power, poweredBase #) = selectPower base

digits
  :: Int     -- ^ The base to use
  -> Integer -- ^ The integer to convert
  -> [Int]
digits base n
  | base < 2  = error "Base must be > 1"
  | n < 0     = error "Number must be non-negative"
  | otherwise = unsafeCoerce
              $ digitsUnsigned (unsafeCoerce base) (unsafeCoerce n)

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
