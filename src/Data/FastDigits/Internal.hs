{-|
Module      : Data.FastDigits.Internal
Copyright   : (c) Andrew Lelechenko, 2015
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com
Stability   : experimental

-}

{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-type-defaults  #-}
{-# OPTIONS_GHC -O2                      #-}
{-# OPTIONS_GHC -optc-O3                 #-}

module Data.FastDigits.Internal
  ( selectPower
  , selectPower'
  ) where

import GHC.Exts

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower :: Word# -> (# Word#, Word# #)
#if WORD_SIZE_IN_BITS == 31
selectPower 2## = (# 31##, 2147483648## #)
#elif WORD_SIZE_IN_BITS == 32
selectPower 2## = (# 31##, 2147483648## #)
#else
selectPower 2## = (# 63##, 9223372036854775808## #)
#endif

selectPower base = go base
  where
    go pw = case timesWord2# pw pw of
        (# 0##, pw2 #)
          -> let (# n, pw2n #) = go pw2 in
            case timesWord2# pw pw2n of
              (# 0##, pw2n1 #) -> (#n `timesWord#` 2## `plusWord#` 1##, pw2n1 #)
              _ -> (# n `timesWord#` 2##, pw2n #)
        _           -> (# 1##, pw #)

-- | Take an integer base and return (pow, base^pow),
--   where base^pow <= maxBound and pow is as large as possible.
selectPower' :: Word -> (Word, Word)
selectPower' (W# base) = (W# power, W# poweredBase)
  where
    (# power, poweredBase #) = selectPower base
