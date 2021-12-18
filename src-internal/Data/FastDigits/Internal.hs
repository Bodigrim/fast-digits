{-|
Module      : Data.FastDigits.Internal
Copyright   : (c) Andrew Lelechenko, 2015-2016
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

-}

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.FastDigits.Internal
  ( selectPower
  , selectPower'
  ) where

import Data.Bits (finiteBitSize)
import GHC.Exts (Word#, Word(..), timesWord2#, plusWord#, timesWord#)

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower :: Word# -> (# Word#, Word# #)
selectPower 2##
  | finiteBitSize (0 :: Word) == 64
  = (# 63##, 9223372036854775808## #)
selectPower 10##
  | finiteBitSize (0 :: Word) == 64
  = (# 19##, 10000000000000000000## #)

selectPower base = go base
  where
    go pw = case timesWord2# pw pw of
        (# 0##, pw2 #)
#if MIN_VERSION_base(4,10,0)
          -> let !(# n, pw2n #) = go pw2 in
#else
          -> let (# n, pw2n #) = go pw2 in
#endif
            case timesWord2# pw pw2n of
              (# 0##, pw2n1 #) -> (#n `timesWord#` 2## `plusWord#` 1##, pw2n1 #)
              _ -> (# n `timesWord#` 2##, pw2n #)
        _           -> (# 1##, pw #)

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower' :: Word -> (Word, Word)
selectPower' (W# base) = (W# power, W# poweredBase)
  where
    !(# power, poweredBase #) = selectPower base
