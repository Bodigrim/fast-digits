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

{-# OPTIONS_GHC -fno-warn-type-defaults	 #-}
{-# OPTIONS_GHC -O2	 #-}

module Data.FastDigits
	( digits
	, undigits
	) where

import Control.Arrow

ti :: Integral a => a -> Integer
ti = toInteger

fi :: Num a => Integer -> a
fi = fromInteger


digitsInteger :: Integer -> Integer -> [Int]
digitsInteger base = f
	where
		f 0 = []
		f n = let (q, r) = n `quotRem` base in fi r : f q

digitsInt :: Int -> Int -> [Int]
digitsInt base = f
	where
		f 0 = []
		f n = let (q, r) = n `quotRem` base in r : f q


digitsInteger' :: Int -> Int -> Integer -> Integer -> [Int]
digitsInteger' power base poweredBase = fst . f
	where
		f :: Integer -> ([Int], Int)
		f n = case n `quotRem` poweredBase of
			(0, _) -> (id &&& length) (digitsInt base $ fi n)
			(q, r) -> (fr ++ replicate (power - lr) 0 ++ fq, lq)
								where
									(fq, lq) = f q
									(fr, lr) = f r

selectPower :: Int -> (Int, Int)
selectPower base = if poweredBase > 0
		then (power, poweredBase)
		else (power - 1, base ^ (power - 1))
	where
		power :: Int
		power = floor $ logBase (fi $ ti base) (fi $ ti $ (maxBound :: Int))
		poweredBase :: Int
		poweredBase = base ^ power

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
digits base n
	| base < 2  = error "Base must be > 1"
	| n < 0     = error "Number must be non-negative"
	| n < ti (maxBound :: Int) = digitsInt base (fi n)
	| otherwise = if power == 1
									then digitsInteger (ti base) n
									else digitsInteger' power base (ti poweredBase) n
	where
		(power, poweredBase) = selectPower base

-- | Return an integer, built from given digits in reverse order.
--   Condition 0 <= digit < base is not checked.
undigits
	:: Int      -- ^ The base to use
	-> [Int]    -- ^ The list of digits to convert
	-> Integer
undigits base' = foldr (\d acc -> acc * base + ti d) 0
	where
		base = ti base'
