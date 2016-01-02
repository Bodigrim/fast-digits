{-# LANGUAGE ViewPatterns  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC hiding (Positive, NonNegative)

import Test.SmallCheck.Series

import qualified Data.Digits as D

import Data.FastDigits

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary =
    (Positive . abs) `fmap` (arbitrary `suchThat` (> 0))

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary =
    (NonNegative . abs) `fmap` (arbitrary `suchThat` (>= 0))


digitsD :: Int -> Integer -> [Int]
digitsD base n = map fromInteger $ D.digitsRev (toInteger base) n

undigitsD :: Int -> [Int] -> Integer
undigitsD base ns = D.unDigits (toInteger base) (map toInteger ns)

digits10 :: Integer -> [Int]
digits10 = reverse . map (read . (:[])) . show

largeInteger :: [Positive Int] -> Integer
largeInteger ns = read $ take 1000 $ '0' : concatMap (\(Positive n) -> show n) ns

-- undigits base . digits base == id
qProperty1 :: Positive Int -> [Positive Int] -> QC.Property
qProperty1 (Positive base) (largeInteger -> n) = base > 1 QC.==>
  undigits base (digits base n) == n

sProperty1 :: Int -> Integer -> Bool
sProperty1 base n = base <= 1 || n < 0 ||
  undigits base (digits base n) == n

-- digits == digitsD
qProperty2 :: Positive Int -> [Positive Int] -> QC.Property
qProperty2 (Positive base) (largeInteger -> n) = base > 1 && n > 0QC.==>
  digits base n == digitsD base n

sProperty2 :: Int -> Integer -> Bool
sProperty2 base n = base <= 1 || n < 0 ||
  digits base n == digitsD base n

-- digits 10 == digits10
qProperty3 :: [Positive Int] -> QC.Property
qProperty3 (largeInteger -> n) = n > 0 QC.==>
  digits 10 n == digits10 n

sProperty3 :: Integer -> Bool
sProperty3 n = n <= 0 ||
  digits 10 n == digits10 n

-- All digits are between 0 and base - 1
qProperty4 :: Positive Int -> [Positive Int] -> QC.Property
qProperty4 (Positive base) (largeInteger -> n) = base > 1 QC.==>
  all (\d -> d >= 0 && d < base) (digits base n)

sProperty4 :: Int -> Integer -> Bool
sProperty4 base n = base <= 1 || n < 0 ||
  all (\d -> d >= 0 && d < base) (digits base n)

-- Last digit is not 0
qProperty5 :: Positive Int -> [Positive Int] -> QC.Property
qProperty5 (Positive base) (largeInteger -> n) = base > 1 && n > 0 QC.==>
  ((/= 0) $ last $ digits base n)

sProperty5 :: Int -> Integer -> Bool
sProperty5 base n = base <= 1 || n <= 0 ||
  ((/= 0) $ last $ digits base n)

-- digits 2 == digitsD 2
qProperty6 :: [Positive Int] -> Bool
qProperty6 (largeInteger -> n) =
  digits 2 n == digitsD 2 n

sProperty6 :: Integer -> Bool
sProperty6 n = n < 0 ||
  digits 2 n == digitsD 2 n

-- digits 2 == digitsD 2 on integers of special form
qProperty7 :: NonNegative Int -> NonNegative Int -> Bool
qProperty7 (NonNegative a) (NonNegative b) =
  digits 2 n == digitsD 2 n
  where
    n = toInteger a * toInteger (maxBound :: Int) + toInteger b

sProperty7 :: Int -> Int -> Bool
sProperty7 a b = a < 0 || b < 0 ||
  digits 2 n == digitsD 2 n
  where
    n = toInteger a * toInteger (maxBound :: Int) + toInteger b

testSuite :: TestTree
testSuite = testGroup "digits"
  [ SC.testProperty "S undigits base . digits base == id" sProperty1
  , QC.testProperty "Q undigits base . digits base == id" qProperty1
  , SC.testProperty "S digits == digitsD" sProperty2
  , QC.testProperty "Q digits == digitsD" qProperty2
  , SC.testProperty "S digits 10 == digits10" sProperty3
  , QC.testProperty "Q digits 10 == digits10" qProperty3
  , SC.testProperty "S All digits are between 0 and base - 1" sProperty4
  , QC.testProperty "Q All digits are between 0 and base - 1" qProperty4
  , SC.testProperty "S Last digit is not 0" sProperty5
  , QC.testProperty "Q Last digit is not 0" qProperty5
  , SC.testProperty "S digits 2 == digitsD 2" sProperty6
  , QC.testProperty "Q digits 2 == digitsD 2" qProperty6
  , SC.testProperty "S digits 2 == digitsD 2 on integers of special form" sProperty7
  , QC.testProperty "Q digits 2 == digitsD 2 on integers of special form" qProperty7
  ]

main :: IO ()
main = defaultMain testSuite
