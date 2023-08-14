{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}

#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

-- |
-- Module      : Data.Word.Word24
-- License     : see  src/Data/LICENSE
-- Stability   : experimental
-- Portability : non-portable (GHC Extensions)

-- Provide a 24-bit unsigned integral type: 'Word24', analagous to Word8,
-- Word16, etc.
--

module Data.Word.Word24 (
  -- * Word24 type
    Word24(..)
  , byteSwap24
  , byteSwap24#
  -- * Internal helpers
  , narrow24Word#
#if MIN_VERSION_base(4,8,0)
  , clz24#
  , ctz24#
#endif
  , popCnt24#
  )

where

import           Data.Bits
import           Data.Data
import           Data.Maybe
import           Foreign.Storable

import           GHC.Arr
import           GHC.Base
import           GHC.Enum
import           GHC.Num hiding (integerToWord)
import           GHC.Ptr
import           GHC.Read
import           GHC.Real
import           GHC.Show
import           GHC.Word.Compat
import           GHC.Integer (smallInteger, integerToWord)

import           Control.DeepSeq

#if !MIN_VERSION_base(4,8,0)
import           Data.Typeable
#endif

------------------------------------------------------------------------

-- Word24 is represented in the same way as Word.  Operations may assume and
-- must ensure that it holds only values in its logical range.

-- | 24-bit unsigned integer type
--
data Word24 = W24# Word# deriving (Eq, Ord)

#if !MIN_VERSION_base(4,8,0)
deriving instance Typeable Word24
#endif

instance NFData Word24 where rnf !_ = ()

word24Type :: DataType
word24Type = mkIntType "Data.Word.Word24.Word24"

instance Data Word24 where
  toConstr x = mkIntegralConstr word24Type x
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c
                                 ++ " is not of type Word24."
  dataTypeOf _ = word24Type

-- | narrowings represented as primop 'and#' in GHC.
narrow24Word# :: Word# -> Word#
narrow24Word# = and# 0xFFFFFF##

#if MIN_VERSION_base(4,8,0)
-- | count leading zeros
--
clz24# :: Word# -> Word#
clz24# w# = clz32# (narrow24Word# w#) `minusWord#` 8##

-- | count trailing zeros
--
ctz24# :: Word# -> Word#
ctz24# w# = ctz# (w# `or#` 0x1000000##)
#endif

-- | the number of set bits
--
popCnt24# :: Word# -> Word#
popCnt24# w# = popCnt# (narrow24Word# w#)

instance Show Word24 where
  showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Num Word24 where
  (W24# x#) + (W24# y#) = W24# (narrow24Word# (x# `plusWord#` y#))
  (W24# x#) - (W24# y#) = W24# (narrow24Word# (x# `minusWord#` y#))
  (W24# x#) * (W24# y#) = W24# (narrow24Word# (x# `timesWord#` y#))
  negate (W24# x#)      = W24# (narrow24Word# (int2Word# (negateInt# (word2Int# x#))))
  abs x                 = x
  signum 0              = 0
  signum _              = 1
  fromInteger i         = W24# (narrow24Word# (integerToWord i))

instance Real Word24 where
  toRational x = toInteger x % 1

instance Enum Word24 where
  succ x
    | x /= maxBound  = x + 1
    | otherwise      = succError "Word24"
  pred x
    | x /= minBound  = x - 1
    | otherwise      = predError "Word24"
  toEnum i@(I# i#)
    | i >= 0 && i <= fromIntegral (maxBound :: Word24)
                     = W24# (int2Word# i#)
    | otherwise      = toEnumError "Word24" i (minBound::Word24, maxBound::Word24)
  fromEnum (W24# x#) = I# (word2Int# x#)
  enumFrom           = boundedEnumFrom
  enumFromThen       = boundedEnumFromThen

instance Integral Word24 where
  quot (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  rem (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `remWord#` y#)
    | otherwise              = divZeroError
  div (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `quotWord#` y#)
    | otherwise              = divZeroError
  mod (W24# x#) y@(W24# y#)
    | y /= 0                 = W24# (x# `remWord#` y#)
    | otherwise              = divZeroError
  quotRem (W24# x#) y@(W24# y#)
    | y /= 0                 = (W24# (x# `quotWord#` y#), W24# (x# `remWord#` y#))
    | otherwise              = divZeroError
  divMod (W24# x#) y@(W24# y#)
    | y /= 0                 = (W24# (x# `quotWord#` y#), W24# (x# `remWord#` y#))
    | otherwise              = divZeroError
  toInteger (W24# x#)        = smallInteger (word2Int# x#)

instance Bounded Word24 where
  minBound = 0
  maxBound = 0xFFFFFF

instance Ix Word24 where
  range (m,n)         = [m..n]
  unsafeIndex (m,_) i = fromIntegral (i - m)
  inRange (m,n) i     = m <= i && i <= n

instance Read Word24 where
  readsPrec p s = [(fromIntegral (x::Int), r) | (x, r) <- readsPrec p s]

instance Bits Word24 where
    {-# INLINE shift #-}
    {-# INLINE bit #-}
    {-# INLINE testBit #-}

    (W24# x#) .&.   (W24# y#)  = W24# (x# `and#` y#)
    (W24# x#) .|.   (W24# y#)  = W24# (x# `or#`  y#)
    (W24# x#) `xor` (W24# y#)  = W24# (x# `xor#` y#)
    complement (W24# x#)       = W24# (x# `xor#` mb#) where !(W24# mb#) = maxBound
    (W24# x#) `shift` (I# i#)
        | isTrue# (i# >=# 0#)  = W24# (narrow24Word# (x# `shiftL#` i#))
        | otherwise            = W24# (x# `shiftRL#` negateInt# i#)
    (W24# x#) `shiftL` (I# i#)       = W24# (narrow24Word# (x# `shiftL#` i#))
    (W24# x#) `unsafeShiftL` (I# i#) =
        W24# (narrow24Word# (x# `uncheckedShiftL#` i#))
    (W24# x#) `shiftR`       (I# i#) = W24# (x# `shiftRL#` i#)
    (W24# x#) `unsafeShiftR` (I# i#) = W24# (x# `uncheckedShiftRL#` i#)
    (W24# x#) `rotate`       i
        | isTrue# (i'# ==# 0#) = W24# x#
        | otherwise  = W24# (narrow24Word# ((x# `uncheckedShiftL#` i'#) `or#`
                                            (x# `uncheckedShiftRL#` (24# -# i'#))))
      where
        !(I# i'#) = i `mod` 24
    bitSizeMaybe i            = Just (finiteBitSize i)
    bitSize                   = finiteBitSize
    isSigned _                = False
    popCount (W24# x#)        = I# (word2Int# (popCnt24# x#))
    bit                       = bitDefault
    testBit                   = testBitDefault

instance FiniteBits Word24 where
    finiteBitSize _ = 24
#if MIN_VERSION_base(4,8,0)
    countLeadingZeros  (W24# x#) = I# (word2Int# (clz24# x#))
    countTrailingZeros (W24# x#) = I# (word2Int# (ctz24# x#))
#endif

-- | Swap bytes in 'Word24'.
--
byteSwap24 :: Word24 -> Word24
byteSwap24 (W24# w#) = W24# (byteSwap24# w#)

byteSwap24# :: Word# -> Word#
byteSwap24# w# = let byte0 = uncheckedShiftL# (and# w# 0x0000ff##) 16#
                     byte1 = and# w# 0x00ff00##
                     byte2 = uncheckedShiftRL# (and# w# 0xff0000##) 16#
                 in byte0 `or#` byte1 `or#` byte2

{-# RULES
"fromIntegral/Word8->Word24"    fromIntegral = \(W8# x#) -> W24# x#
"fromIntegral/Word16->Word24"   fromIntegral = \(W16# x#) -> W24# x#
"fromIntegral/Word24->Word24"   fromIntegral = id :: Word24 -> Word24
"fromIntegral/Word24->Integer"  fromIntegral = toInteger :: Word24 -> Integer
"fromIntegral/a->Word24"        fromIntegral = \x -> case fromIntegral x of W# x# -> W24# (narrow24Word# x#)
"fromIntegral/Word24->a"        fromIntegral = \(W24# x#) -> fromIntegral (W# x#)
  #-}

{-# RULES
"properFraction/Float->(Word24,Float)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word24) n, y :: Float) }
"truncate/Float->Word24"
    truncate = (fromIntegral :: Int -> Word24) . (truncate :: Float -> Int)
"floor/Float->Word24"
    floor    = (fromIntegral :: Int -> Word24) . (floor :: Float -> Int)
"ceiling/Float->Word24"
    ceiling  = (fromIntegral :: Int -> Word24) . (ceiling :: Float -> Int)
"round/Float->Word24"
    round    = (fromIntegral :: Int -> Word24) . (round  :: Float -> Int)
  #-}

{-# RULES
"properFraction/Double->(Word24,Double)"
    properFraction = \x ->
                      case properFraction x of {
                        (n, y) -> ((fromIntegral :: Int -> Word24) n, y :: Double) }
"truncate/Double->Word24"
    truncate = (fromIntegral :: Int -> Word24) . (truncate :: Double -> Int)
"floor/Double->Word24"
    floor    = (fromIntegral :: Int -> Word24) . (floor :: Double -> Int)
"ceiling/Double->Word24"
    ceiling  = (fromIntegral :: Int -> Word24) . (ceiling :: Double -> Int)
"round/Double->Word24"
    round    = (fromIntegral :: Int -> Word24) . (round  :: Double -> Int)
  #-}

readWord24OffPtr :: Ptr Word24 -> IO Word24
readWord24OffPtr p = do
  let p' = castPtr p :: Ptr Word8
  w1 <- peekElemOff p' 0
  w2 <- peekElemOff p' 1
  w3 <- peekElemOff p' 2
  let w1' = (fromIntegral :: (Word8 -> Word24)) w1
      w2' = (fromIntegral :: (Word8 -> Word24)) w2
      w3' = (fromIntegral :: (Word8 -> Word24)) w3
      w = w1' .|. (w2' `shiftL` 8) .|. (w3' `shiftL` 16)
  return w

writeWord24ToPtr :: Ptr Word24 -> Word24 -> IO ()
writeWord24ToPtr p v = do
    let w1 = fromIntegral (v .&. 0x0000FF) :: Word8
        w2 = fromIntegral ((v .&. 0x00FF00) `shiftR` 8) :: Word8
        w3 = fromIntegral ((v .&. 0xFF0000) `shiftR` 16) :: Word8
    pokeByteOff p 0 w1
    pokeByteOff p 1 w2
    pokeByteOff p 2 w3

instance Storable Word24 where
  sizeOf _    = 3
  alignment _ = 3
  peek        = readWord24OffPtr
  poke        = writeWord24ToPtr

