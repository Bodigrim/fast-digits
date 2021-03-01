{-|
Module      : Data.FastDigits
Description : Integer-to-digits conversion.
Copyright   : (c) Andrew Lelechenko, 2015-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental

Convert an integer to digits and back.
This library is both asymptotically (O(n^1.4) vs. O(n^2))
and practically (2x-40x for typical inputs)
faster than "Data.Digits".
-}

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.FastDigits
  ( digits
  , undigits
  , digitsUnsigned
  ) where

import Data.Bits
import GHC.Exts
import GHC.Integer.GMP.Internals
import GHC.Natural
import Unsafe.Coerce
import Data.FastDigits.Internal

digitsNatural :: GmpLimb# -> BigNat -> [Word]
digitsNatural base = f
  where
    f n
      | isZeroBigNat n = []
      | otherwise      = let !(# q, r #) = n `quotRemBigNatWord` base in
                         W# r : f q

digitsWord :: Word# -> Word# -> [Word]
digitsWord 2## = g
  where
    g :: Word# -> [Word]
    g 0## = []
    g n   = W# (n `and#` 1##) : g (n `uncheckedShiftRL#` 1#)
digitsWord 10##
  | finiteBitSize (0 :: Word) == 64
  = f
  where
    f :: Word# -> [Word]
    f 0## = []
    f n   = let !(# hi, _ #) = n `timesWord2#` 14757395258967641293## in
            let q = hi `shiftRL#` 3# in
            let r = n `minusWord#` (q `timesWord#` 10##) in
            W# r : f q
digitsWord base = f
  where
    f :: Word# -> [Word]
    f 0## = []
    f n   = let !(# q, r #) = n `quotRemWord#` base in
            W# r : f q

-- | For a given base and expected length of list of digits
--   return the list of digits and padding till expected length.
digitsWordL :: Word# -> Word# -> Word# -> (# [Word], Word# #)
digitsWordL 2## power = g
  where
    g :: Word# -> (# [Word], Word# #)
    g 0## = (# [], power #)
    g n   = (# W# (n `and#` 1##) : fq, lq `minusWord#` 1## #)
      where
        !(# fq, lq #) = g (n `uncheckedShiftRL#` 1#)
digitsWordL 10## power
  | finiteBitSize (0 :: Word) == 64
  = f
  where
    f :: Word# -> (# [Word], Word# #)
    f 0## = (# [], power #)
    f n   = (# W# r : fq, lq `minusWord#` 1## #)
      where
        !(# hi, _ #) = n `timesWord2#` 14757395258967641293##
        q = hi `shiftRL#` 3#
        r = n `minusWord#` (q `timesWord#` 10##)
        !(# fq, lq #) = f q
digitsWordL base power = f
  where
    f :: Word# -> (# [Word], Word# #)
    f 0## = (# [], power #)
    f n   = (# W# r : fq, lq `minusWord#` 1## #)
      where
        !(#  q,  r #) = n `quotRemWord#` base
        !(# fq, lq #) = f q

-- | For a given base, power and precalculated base^power
--   take an integer and return the list of its digits.
digitsNatural' :: Word# -> Word# -> Word# -> BigNat -> [Word]
digitsNatural' base power poweredBase = f
  where
    f :: BigNat -> [Word]
    f n = let !(# q, r #) = n `quotRemBigNatWord` poweredBase in
      if isZeroBigNat q
        then digitsWord base r
        else let !(# fr, lr #) = digitsWordL base power r in
          fr ++ replicate (I# (unsafeCoerce# lr)) 0 ++ f q

padUpTo :: Int -> [Word] -> [Word]
padUpTo !n [] = replicate n 0
padUpTo !n (x : xs) = x : padUpTo (n - 1) xs

-- | Return digits of a non-negative number in reverse order.
digitsUnsigned
  :: Word    -- ^ Precondition that base is ≥2 is not checked
  -> Natural
  -> [Word]
digitsUnsigned (W# base) (NatS# n) = digitsWord base n
digitsUnsigned (W# base) (NatJ# n)
  | halfSize <- sizeofBigNat# n `iShiftRL#` 1#
  , isTrue# (halfSize ># 128#)
  = let pow = I# (word2Int# power *# halfSize) in
    let (nHi, nLo) = NatJ# n `quotRem` (NatS# poweredBase ^ (I# halfSize)) in
    padUpTo pow (digitsUnsigned (W# base) nLo) ++ digitsUnsigned (W# base) nHi
  | otherwise
  = case power of
    1## -> digitsNatural base n
    _   -> digitsNatural' base power poweredBase n
  where
    !(# power, poweredBase #) = selectPower base

-- | Return digits of a non-negative number in reverse order.
--   Throw an error if number is negative or base is below 2.
--
--   > digits 10 123 = [3, 2, 1]
--   > digits 10 0   = []
digits
  :: Int     -- ^ The base to use
  -> Integer -- ^ The number to convert
  -> [Int]   -- ^ Digits in reverse order
digits base n
  | base < 2  = error "Base must be > 1"
  | n < 0     = error "Number must be non-negative"
  | otherwise = unsafeCoerce
              $ digitsUnsigned (unsafeCoerce base) (unsafeCoerce n)

-- | Return an integer, built from given digits in reverse order.
--   Condition 0 ≤ digit < base is not checked.
undigits :: (Integral a, Integral b)
         => a       -- ^ The base to use
         -> [b]     -- ^ The list of digits to convert
         -> Integer
undigits base' = foldr (\d acc -> acc * base + toInteger d) 0
  where
    base = toInteger base'
{-# SPECIALIZE undigits :: Word    -> [Word]    -> Integer #-}
{-# SPECIALIZE undigits :: Int     -> [Int]     -> Integer #-}
{-# SPECIALIZE undigits :: Integer -> [Integer] -> Integer #-}
