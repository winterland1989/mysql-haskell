{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}

#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Int.Int24
-- Copyright   :  (c) The University of Glasgow 1997-2002
-- License     :  see src/Data/LICENSE
--
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- The 24 bit integral datatype, 'Int24'.
--
-----------------------------------------------------------------------------

module Data.Int.Int24 (
  -- * Int24 type
  Int24(..)
  -- * Internal helper
  , narrow24Int#
  ) where

import           Data.Bits
import           Data.Data
import           Data.Maybe
import           Data.Word.Word24
import           Foreign.Storable

import           GHC.Arr
import           GHC.Base
import           GHC.Enum
import           GHC.Int.Compat
import           GHC.Integer (smallInteger, integerToInt)
import           GHC.Num hiding (integerToInt)
import           GHC.Ptr
import           GHC.Read
import           GHC.Real
import           GHC.Show
import           GHC.Word.Compat

import           Control.DeepSeq

#if !MIN_VERSION_base(4,8,0)
import           Data.Typeable
#endif

------------------------------------------------------------------------

-- Int24 is represented in the same way as Int. Operations may assume
-- and must ensure that it holds only values from its logical range.

-- | 24-bit signed integer type
data Int24 = I24# Int# deriving (Eq, Ord)

#if !MIN_VERSION_base(4,8,0)
deriving instance Typeable Int24
#endif

instance NFData Int24  where rnf !_ = ()

int24Type :: DataType
int24Type = mkIntType "Data.Word.Int24.Int24"

instance Data Int24 where
  toConstr x = mkIntegralConstr int24Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Int24."
  dataTypeOf _ = int24Type

-- the narrowings are primops in GHC; I don't have that luxury.
-- if the 24th bit (from right) is on, the value is negative, so
-- fill the uppermost bits with 1s.  Otherwise clear them to 0s.
narrow24Int# :: Int# -> Int#
narrow24Int# x# = if isTrue# ((x'# `and#` mask#) `eqWord#` mask#)
    then word2Int# (x'# `or#`  int2Word# m1#)
    else word2Int# (x'# `and#` int2Word# m2#)
  where
    !x'#   = int2Word# x#
    !mask# = int2Word# 0x00800000#
    !(I# m1#) = -8388608
    !(I# m2#) = 16777215

instance Show Int24 where
    showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Int24 where
    (I24# x#) + (I24# y#)  = I24# (narrow24Int# (x# +# y#))
    (I24# x#) - (I24# y#)  = I24# (narrow24Int# (x# -# y#))
    (I24# x#) * (I24# y#)  = I24# (narrow24Int# (x# *# y#))
    negate (I24# x#)       = I24# (narrow24Int# (negateInt# x#))
    abs x | x >= 0         = x
          | otherwise      = negate x
    signum x | x > 0       = 1
    signum 0               = 0
    signum _               = -1
    fromInteger i          = I24# (narrow24Int# (integerToInt i))

instance Real Int24 where
    toRational x = toInteger x % 1

instance Enum Int24 where
    succ x
        | x /= maxBound = x + 1
        | otherwise     = succError "Int24"
    pred x
        | x /= minBound = x - 1
        | otherwise     = predError "Int24"
    toEnum i@(I# i#)
        | i >= fromIntegral (minBound::Int24) && i <= fromIntegral (maxBound::Int24)
                        = I24# i#
        | otherwise     = toEnumError "Int24" i (minBound::Int24, maxBound::Int24)
    fromEnum (I24# x#)  = I# x#
    enumFrom            = boundedEnumFrom
    enumFromThen        = boundedEnumFromThen

instance Integral Int24 where
    quot    x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `quotInt#` y#))
    rem     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `remInt#` y#))
    div     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `divInt#` y#))
    mod     x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = I24# (narrow24Int# (x# `modInt#` y#))
    quotRem x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = (I24# (narrow24Int# (x# `quotInt#` y#)),
                                        I24# (narrow24Int# (x# `remInt#` y#)))
    divMod  x@(I24# x#) y@(I24# y#)
        | y == 0                     = divZeroError
        | x == minBound && y == (-1) = overflowError
        | otherwise                  = (I24# (narrow24Int# (x# `divInt#` y#)),
                                        I24# (narrow24Int# (x# `modInt#` y#)))
    toInteger (I24# x#)              = smallInteger x#

instance Bounded Int24 where
    minBound = -0x800000
    maxBound =  0x7FFFFF

instance Ix Int24 where
    range (m,n)         = [m..n]
    unsafeIndex (m,_) i = fromIntegral i - fromIntegral m
    inRange (m,n) i     = m <= i && i <= n

instance Read Int24 where
    readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Int24 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (I24# x#) .&.   (I24# y#)  = I24# (word2Int# (int2Word# x# `and#` int2Word# y#))
    (I24# x#) .|.   (I24# y#)  = I24# (word2Int# (int2Word# x# `or#`  int2Word# y#))
    (I24# x#) `xor` (I24# y#)  = I24# (word2Int# (int2Word# x# `xor#` int2Word# y#))
    complement (I24# x#)       = I24# (word2Int# (not# (int2Word# x#)))
    (I24# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = I24# (narrow24Int# (x# `iShiftL#` i#))
        | otherwise            = I24# (x# `iShiftRA#` negateInt# i#)
    (I24# x#) `shiftL`       (I# i#) = I24# (narrow24Int# (x# `iShiftL#` i#))
    (I24# x#) `unsafeShiftL` (I# i#) = I24# (narrow24Int# (x# `uncheckedIShiftL#` i#))
    (I24# x#) `shiftR`       (I# i#) = I24# (x# `iShiftRA#` i#)
    (I24# x#) `unsafeShiftR` (I# i#) = I24# (x# `uncheckedIShiftRA#` i#)
    (I24# x#) `rotate` i
        | isTrue# (i'# ==# 0#) = I24# x#
        | otherwise = I24# (narrow24Int# (word2Int# ((x'# `uncheckedShiftL#` i'#) `or#`
                                         (x'# `uncheckedShiftRL#` (24# -# i'#)))))
      where
        !x'# = narrow24Word# (int2Word# x#)
        !(I# i'#) = i `mod` 24
    bitSizeMaybe i             = Just (finiteBitSize i)
    bitSize                    = finiteBitSize
    isSigned _                 = True
    popCount (I24# x#)         = I# (word2Int# (popCnt24# (int2Word# x#)))
    bit                        = bitDefault
    testBit                    = testBitDefault

instance FiniteBits Int24 where
    finiteBitSize _ = 24
#if MIN_VERSION_base(4,8,0)
    countLeadingZeros  (I24# x#) = I# (word2Int# (clz24# (int2Word# x#)))
    countTrailingZeros (I24# x#) = I# (word2Int# (ctz24# (int2Word# x#)))
#endif

{-# RULES
"fromIntegral/Word8->Int24"   fromIntegral = \(W8# x#) -> I24# (word2Int# x#)
"fromIntegral/Word16->Int24"  fromIntegral = \(W16# x#) -> I24# (word2Int# x#)
"fromIntegral/Int8->Int24"    fromIntegral = \(I8# x#) -> I24# x#
"fromIntegral/Int16->Int24"   fromIntegral = \(I16# x#) -> I24# x#
"fromIntegral/Int24->Int24"   fromIntegral = id :: Int24 -> Int24
"fromIntegral/a->Int24"       fromIntegral = \x -> case fromIntegral x of I# x# -> I24# (narrow24Int# x#)
"fromIntegral/Int24->a"       fromIntegral = \(I24# x#) -> fromIntegral (I# x#)
  #-}

{-# RULES
"properFraction/Float->(Int24,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int24) n, y :: Float) }
"truncate/Float->Int24"
    truncate = (fromIntegral :: Int -> Int24) . (truncate :: Float -> Int)
"floor/Float->Int24"
    floor    = (fromIntegral :: Int -> Int24) . (floor :: Float -> Int)
"ceiling/Float->Int24"
    ceiling  = (fromIntegral :: Int -> Int24) . (ceiling :: Float -> Int)
"round/Float->Int24"
    round    = (fromIntegral :: Int -> Int24) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Int24,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Int24) n, y :: Double) }
"truncate/Double->Int24"
    truncate = (fromIntegral :: Int -> Int24) . (truncate :: Double -> Int)
"floor/Double->Int24"
    floor    = (fromIntegral :: Int -> Int24) . (floor :: Double -> Int)
"ceiling/Double->Int24"
    ceiling  = (fromIntegral :: Int -> Int24) . (ceiling :: Double -> Int)
"round/Double->Int24"
    round    = (fromIntegral :: Int -> Int24) . (round  :: Double -> Int)
  #-}

instance Storable Int24 where
  sizeOf _ = 3
  alignment _ = 3
  peek p = fmap fromIntegral $ peek ((castPtr p) :: Ptr Word24)
  poke p v = poke (castPtr p :: Ptr Word24) (fromIntegral v)
