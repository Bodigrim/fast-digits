module Main where

import Criterion.Main

import qualified Data.Digits as D

import Data.FastDigits

digitsD :: Int -> Integer -> [Int]
digitsD base n = map fromInteger $ D.digitsRev (toInteger base) n



intNs :: Int -> Int -> [Int]
intNs from len = [from, step .. maxBound]
  where
    step = maxBound `div` len

integerN :: Int -> Int -> Integer
integerN from len = undigits (maxBound :: Int) (intNs from len)

integerNs :: Int -> Int -> [Integer]
integerNs llen len = map (\i -> integerN i len) [1..llen]

main = defaultMain [
  bgroup "shortInt"  [ bench "FastDigits  base 2    on shortInt" $ nf (map $ digits  2      . toInteger) (intNs 1 10000)
                     , bench "Data.Digits base 2    on shortInt" $ nf (map $ digitsD 2      . toInteger) (intNs 1 10000)
                     , bench "FastDigits  base 10   on shortInt" $ nf (map $ digits  10     . toInteger) (intNs 1 10000)
                     , bench "Data.Digits base 10   on shortInt" $ nf (map $ digitsD 10     . toInteger) (intNs 1 10000)
                     , bench "FastDigits  base 10^5 on shortInt" $ nf (map $ digits  (10^5) . toInteger) (intNs 1 10000)
                     , bench "Data.Digits base 10^5 on shortInt" $ nf (map $ digitsD (10^5) . toInteger) (intNs 1 10000)
                     , bench "FastDigits  base 10^9 on shortInt" $ nf (map $ digits  (10^9) . toInteger) (intNs 1 10000)
                     , bench "Data.Digits base 10^9 on shortInt" $ nf (map $ digitsD (10^9) . toInteger) (intNs 1 10000)
                     ],
  bgroup "mediumInt" [ bench "FastDigits  base 2    on mediumInt" $  nf (map $ digits  2     ) (integerNs 100 10)
                     , bench "Data.Digits base 2    on mediumInt" $  nf (map $ digitsD 2     ) (integerNs 100 10)
                     , bench "FastDigits  base 10   on mediumInt" $  nf (map $ digits  10    ) (integerNs 100 10)
                     , bench "Data.Digits base 10   on mediumInt" $  nf (map $ digitsD 10    ) (integerNs 100 10)
                     , bench "FastDigits  base 10^5 on mediumInt" $  nf (map $ digits  (10^5)) (integerNs 100 10)
                     , bench "Data.Digits base 10^5 on mediumInt" $  nf (map $ digitsD (10^5)) (integerNs 100 10)
                     , bench "FastDigits  base 10^9 on mediumInt" $  nf (map $ digits  (10^9)) (integerNs 100 10)
                     , bench "Data.Digits base 10^9 on mediumInt" $  nf (map $ digitsD (10^9)) (integerNs 100 10)
                     ],
  bgroup "longInt"   [ bench "FastDigits  base 2    on longInt" $  nf (digits  2     ) (integerN 1 1000)
                     , bench "Data.Digits base 2    on longInt" $  nf (digitsD 2     ) (integerN 1 1000)
                     , bench "FastDigits  base 10   on longInt" $  nf (digits  10    ) (integerN 1 1000)
                     , bench "Data.Digits base 10   on longInt" $  nf (digitsD 10    ) (integerN 1 1000)
                     , bench "FastDigits  base 10^5 on longInt" $  nf (digits  (10^5)) (integerN 1 1000)
                     , bench "Data.Digits base 10^5 on longInt" $  nf (digitsD (10^5)) (integerN 1 1000)
                     , bench "FastDigits  base 10^9 on longInt" $  nf (digits  (10^9)) (integerN 1 1000)
                     , bench "Data.Digits base 10^9 on longInt" $  nf (digitsD (10^9)) (integerN 1 1000)
                     ]
  ]
