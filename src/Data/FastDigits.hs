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

digits :: Int -> Integer -> [Int]
digits base n
	| base < 2  = error "Base must be > 1"
	| n < 0     = error "Number must be non-negative"
	| n < ti (maxBound :: Int) = digitsInt base (fi n)
	| otherwise = if power == 1
									then digitsInteger (ti base) n
									else digitsInteger' power base (ti poweredBase) n
	where
		(power, poweredBase) = selectPower base


undigits :: Int -> [Int] -> Integer
undigits base' = foldr (\d acc -> acc * base + ti d) 0
	where
		base = ti base'
