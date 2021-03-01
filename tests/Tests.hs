{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns  #-}

module Main where

import Test.SmallCheck.Series as SC
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

#ifdef MIN_VERSION_digits
import qualified Data.Digits as D
#endif

import Data.FastDigits
import Data.FastDigits.Internal

#ifdef MIN_VERSION_digits
digitsD :: Int -> Integer -> [Int]
digitsD base n = map fromInteger $ D.digitsRev (toInteger base) n

undigitsD :: Int -> [Int] -> Integer
undigitsD base ns = D.unDigits (toInteger base) (map toInteger ns)
#endif

digits10 :: Integer -> [Int]
digits10 = reverse . map (read . (:[])) . show

largeInteger :: [QC.Positive Int] -> QC.NonNegative Integer
largeInteger ns = QC.NonNegative $ read $ take 1000 $ '0' : concatMap (\(QC.Positive n) -> show n) ns

-- undigits base . digits base == id
qProperty1 :: QC.Positive Int -> [QC.Positive Int] -> QC.Property
qProperty1 (QC.Positive base) (largeInteger -> QC.NonNegative n) = base /= 1 QC.==>
  undigits base (digits base n) === n

sProperty1 :: SC.Positive Int -> SC.NonNegative Integer -> Bool
sProperty1 (SC.Positive base) (SC.NonNegative n) = base == 1 ||
  undigits base (digits base n) == n

#ifdef MIN_VERSION_digits
-- digits == digitsD
qProperty2 :: QC.Positive Int -> [QC.Positive Int] -> QC.Property
qProperty2 (QC.Positive base) (largeInteger -> QC.NonNegative n) = base /= 1 QC.==>
  digits base n === digitsD base n

sProperty2 :: SC.Positive Int -> SC.NonNegative Integer -> Bool
sProperty2 (SC.Positive base) (SC.NonNegative n) = base == 1 ||
  digits base n == digitsD base n
#endif

-- digits 10 == digits10
qProperty3 :: [QC.Positive Int] -> QC.Property
qProperty3 (largeInteger -> QC.NonNegative n) = n /= 0 QC.==>
  digits 10 n === digits10 n

sProperty3 :: SC.Positive Integer -> Bool
sProperty3 (SC.Positive n) =
  digits 10 n == digits10 n

-- All digits are between 0 and base - 1
qProperty4 :: QC.Positive Int -> [QC.Positive Int] -> QC.Property
qProperty4 (QC.Positive base) (largeInteger -> QC.NonNegative n) = base /= 1 QC.==>
  all (\d -> d >= 0 && d < base) (digits base n)

sProperty4 :: SC.Positive Int -> SC.NonNegative Integer -> Bool
sProperty4 (SC.Positive base) (SC.NonNegative n) = base == 1 ||
  all (\d -> d >= 0 && d < base) (digits base n)

-- Last digit is not 0
qProperty5 :: QC.Positive Int -> [QC.Positive Int] -> QC.Property
qProperty5 (QC.Positive base) (largeInteger -> QC.NonNegative n) = base /= 1 && n /= 0 QC.==>
  ((/= 0) $ last $ digits base n)

sProperty5 :: SC.Positive Int -> SC.Positive Integer -> Bool
sProperty5 (SC.Positive base) (SC.Positive n) = base == 1 ||
  ((/= 0) $ last $ digits base n)

#ifdef MIN_VERSION_digits
-- digits 2 == digitsD 2
qProperty6 :: [QC.Positive Int] -> QC.Property
qProperty6 (largeInteger -> QC.NonNegative n) =
  digits 2 n === digitsD 2 n

sProperty6 :: SC.NonNegative Integer -> Bool
sProperty6 (SC.NonNegative n) =
  digits 2 n == digitsD 2 n

-- digits 2 == digitsD 2 on integers of special form
qProperty7 :: QC.NonNegative Int -> QC.NonNegative Int -> QC.Property
qProperty7 (QC.NonNegative a) (QC.NonNegative b) =
  digits 2 n === digitsD 2 n
  where
    n = toInteger a * toInteger (maxBound :: Int) + toInteger b

sProperty7 :: SC.NonNegative Int -> SC.NonNegative Int -> Bool
sProperty7 (SC.NonNegative a) (SC.NonNegative b) =
  digits 2 n == digitsD 2 n
  where
    n = toInteger a * toInteger (maxBound :: Int) + toInteger b
#endif

qProperty8 :: QC.Positive Int -> QC.Property
qProperty8 (QC.Positive base') = base /= 1 QC.==>
  base ^ power == poweredBase && poweredBase > maxBound `div` base
  where
    base = fromIntegral $ toInteger base'
    (power, poweredBase) = selectPower' base

sProperty8 :: SC.Positive Int -> Bool
sProperty8 (SC.Positive base') = base == 1 ||
  base ^ power == poweredBase && poweredBase > maxBound `div` base
  where
    base = fromIntegral $ toInteger base'
    (power, poweredBase) = selectPower' base

testSuite :: TestTree
testSuite = testGroup "digits"
  [ SC.testProperty "S undigits base . digits base == id" sProperty1
  , QC.testProperty "Q undigits base . digits base == id" qProperty1
#ifdef MIN_VERSION_digits
  , SC.testProperty "S digits == digitsD" sProperty2
  , QC.testProperty "Q digits == digitsD" qProperty2
#endif
  , SC.testProperty "S digits 10 == digits10" sProperty3
  , QC.testProperty "Q digits 10 == digits10" qProperty3
  , SC.testProperty "S All digits are between 0 and base - 1" sProperty4
  , QC.testProperty "Q All digits are between 0 and base - 1" qProperty4
  , SC.testProperty "S Last digit is not 0" sProperty5
  , QC.testProperty "Q Last digit is not 0" qProperty5
#ifdef MIN_VERSION_digits
  , SC.testProperty "S digits 2 == digitsD 2" sProperty6
  , QC.testProperty "Q digits 2 == digitsD 2" qProperty6
  , SC.testProperty "S digits 2 == digitsD 2 on integers of special form" sProperty7
  , QC.testProperty "Q digits 2 == digitsD 2 on integers of special form" qProperty7
#endif
  , SC.testProperty "S selectPower is correct" sProperty8
  , QC.testProperty "Q selectPower is correct" qProperty8
  ]

main :: IO ()
main = defaultMain testSuite
