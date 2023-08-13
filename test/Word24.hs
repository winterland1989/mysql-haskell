{-# LANGUAGE CPP #-}

module Word24(tests) where

import Prelude as P

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck hiding ((.&.))

import QCUtils
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24
import Data.Bits
import Foreign.Storable
import GHC.Real

-- ----------------------------------------
-- Word24 Properties

prop_addIdent a = a - a == 0
  where types = a :: Word24

prop_multIdent a = a * 1 == a
  where types = a :: Word24

prop_unsigned a = a >= 0
  where types = a :: Word24

prop_smaller a = (fromIntegral ((fromIntegral a) :: Word24) :: Word16) == a

prop_show a = show a == show (fromIntegral a :: Word24)
  where types = a :: Word16

prop_read a = a == (read . show) a
  where types = a :: Word24

prop_adder a b = (a < ub) && (b < ub) ==>
                 fromIntegral (a + b) ==
                 ((fromIntegral a + fromIntegral b) :: Word24)
  where types = (a :: Word16, b :: Word16)
        ub :: Word16
        ub = maxBound `div` 2

prop_negate a = a == negate (negate a)
  where types = a :: Word24

prop_abs a = a == abs a
  where types = a :: Word24

prop_signum a = if a == 0 then signum a == a else signum a == 1
  where types = a :: Word24

prop_real a = let r = toRational a in numerator r == fromIntegral a
  where types = a :: Word24

-- Word24 Enum properties
prop_enum1 a = a < maxBound ==> succ a == a + 1
  where types = a :: Word24

prop_enum2 a = a > minBound ==> pred a == a - 1
  where types = a :: Word24

prop_enum3 a = let a' = abs a in
               toEnum a' == fromIntegral a'
  where types = a :: Int

prop_enum4 a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]
  where types = a :: Word24

prop_enum5 a b = let b' = fromIntegral b in
                 enumFromTo a (a + b') ==
                 map fromIntegral (enumFromTo (fromIntegral a :: Integer)
                                             (fromIntegral (a + b') :: Integer))
  where types = (a :: Word24, b :: Word8)

prop_enum6 a b = take 2 (enumFromThen a b) == [a,b]
  where types = (a :: Word24, b :: Word24)

-- Word24 Integral properties
prop_quot a b =
  quot a b == fromIntegral (quot (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_rem a b =
  rem a b == fromIntegral (rem (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_div a b =
  div a b == fromIntegral (div (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_mod a b =
  mod a b == fromIntegral (mod (fromIntegral a :: Word32) (fromIntegral b :: Word32))
  where types = a :: Word24

prop_quotrem a b = let (j, k) = quotRem a b in
  a == (b * j) + k
  where types = (a :: Word24, b :: Word24)

prop_divmod a b =
  divMod a b == (div a b, mod a b)
  where types = (a :: Word24, b :: Word24)

-- binary Word properties
prop_and a b = (a .&. b) == fromIntegral ( (fromIntegral a :: Word24) .&. (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_or a b = (a .|. b) == fromIntegral ( (fromIntegral a :: Word24) .|. (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_xor a b = (a `xor` b) == fromIntegral ( (fromIntegral a :: Word24) `xor` (fromIntegral b :: Word24))
  where types = (a :: Word16, b :: Word16)

prop_xor_ident a b = (a `xor` b) `xor` b == a
  where types = (a :: Word24, b :: Word24)

prop_shiftL a = a `shiftL` 1 == a * 2
  where types = a :: Word24

prop_shiftR a = a < maxBound `div` 2 ==>
  (a * 2) `shift` (-1) == a
  where types = a :: Word24

prop_shiftR2 a n = n >= 0 ==> a `shiftR` n == a `shift` (negate n)
  where types = a :: Word24

prop_shiftL_ident a = a `shiftL` 0 == a
  where types = a :: Word24

prop_rotate a b = (a `rotate` b) `rotate` (negate b) == a
  where types = (a :: Word24, b :: Int)

prop_comp a = complement (complement a) == a
  where types = a :: Word24

prop_byteSwap a = byteSwap24 (byteSwap24 a) == a
  where types = a :: Word24

#if MIN_VERSION_base(4,8,0)
prop_clz a = countLeadingZeros a == countLeadingZeros' a
  where
    countLeadingZeros' :: Word24 -> Int
    countLeadingZeros' x = (w-1) - go (w-1)
      where
        go i | i < 0       = i -- no bit set
             | testBit x i = i
             | otherwise   = go (i-1)

        w = finiteBitSize x

prop_ctz a = countTrailingZeros a == countTrailingZeros' a
  where
    countTrailingZeros' :: Word24 -> Int
    countTrailingZeros' x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)
        w = finiteBitSize x
#endif

prop_bit_ident q (NonNegative j) = testBit (bit j `asTypeOf` q) j == (j < 24)

prop_popCount s t a = if a >= 0
  then popCount (a `asTypeOf` s) == popCount (fromIntegral a `asTypeOf` t)
  else
    bitSize s - popCount (a `asTypeOf` s) == bitSize t - popCount (fromIntegral a `asTypeOf` t)

-- Word Storable properties
prop_sizeOf a = sizeOf a == 3
  where types = a :: Word24

prop_align a = alignment a == 3
  where types = a :: Word24



-- ----------------------------------------
-- Int24 Properties

prop_addIdentI a = a - a == 0
  where types = a :: Int24

prop_multIdentI a = a * 1 == a
  where types = a :: Int24

prop_smallerI a = (fromIntegral ((fromIntegral a) :: Int24) :: Int16) == a

prop_showI a = show a == show (fromIntegral a :: Int24)
  where types = a :: Int16

prop_readI a = a == (read . show) a
  where types = a :: Int24

prop_adderI a b = ((fromIntegral a + fromIntegral b) :: Int) ==
                  fromIntegral ((fromIntegral a + fromIntegral b) :: Int24)
  where types = (a :: Int16, b :: Int16)

prop_negateI a = a == negate (negate a)
  where types = a :: Int24

prop_absI a = if a >= 0 then a == abs a else a == negate (abs a)
  where types = a :: Int24

prop_signumI a = signum a == fromIntegral (signum (fromIntegral a :: Int))
  where types = a :: Int24

prop_realI a = let r = toRational a in numerator r == fromIntegral a
  where types = a :: Int24

-- Int24 Enum Properties
prop_enum1I a = a < maxBound ==> succ a == a + 1
  where types = a :: Int24

prop_enum2I a = a > minBound ==> pred a == a - 1
  where types = a :: Int24

prop_enum3I a = let a' = abs a in
               toEnum a' == fromIntegral a'
  where types = a :: Int

prop_enum4I a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]
  where types = a :: Int24

prop_enum5I a b = let b' = fromIntegral b in
                 enumFromTo a (a + b') ==
                 map fromIntegral (enumFromTo (fromIntegral a :: Integer)
                                             (fromIntegral (a + b') :: Integer))
  where types = (a :: Int24, b :: Word8)

prop_enum6I a b = take 2 (enumFromThen a b) == [a,b]
  where types = (a :: Int24, b :: Int24)

-- Int24 Integral properties
prop_quotI a b =
  quot a b == fromIntegral (quot (fromIntegral a :: Int32) (fromIntegral b :: Int32))
  where types = a :: Int24

prop_remI a b =
  rem a b == fromIntegral (rem (fromIntegral a :: Int32) (fromIntegral b :: Int32))
  where types = a :: Int24

prop_divI a b =
  div a b == fromIntegral (div (fromIntegral a :: Int32) (fromIntegral b :: Int32))
  where types = a :: Int24

prop_modI a b =
  mod a b == fromIntegral (mod (fromIntegral a :: Int32) (fromIntegral b :: Int32))
  where types = a :: Int24

prop_quotremI a b = let (j, k) = quotRem a b in
  a == (b * j) + k
  where types = (a :: Int24, b :: Int24)

prop_divmodI a b =
  divMod a b == (div a b, mod a b)
  where types = (a :: Int24, b :: Int24)


-- binary Int properties
prop_andI a b = (a .&. b) == fromIntegral ( (fromIntegral a :: Int24) .&. (fromIntegral b :: Int24))
  where types = (a :: Int16, b :: Int16)

prop_orI a b = (a .|. b) == fromIntegral ( (fromIntegral a :: Int24) .|. (fromIntegral b :: Int24))
  where types = (a :: Int16, b :: Int16)

prop_xorI a b = (a `xor` b) == fromIntegral ( (fromIntegral a :: Int24) `xor` (fromIntegral b :: Int24))
  where types = (a :: Int16, b :: Int16)

prop_xor_identI a b = (a `xor` b) `xor` b == a
  where types = (a :: Int24, b :: Int24)

prop_shiftLI a = a `shiftL` 1 == a * 2
  where types = a :: Int24

prop_shiftL_identI a = a `shiftL` 0 == a
  where types = a :: Int24

prop_shiftRI a = (a < maxBound `div` 2) && (a > minBound `div` 2) ==>
  (a * 2) `shift` (-1) == a
  where types = a :: Int24

prop_shiftR2I a n = n >= 0 ==> a `shiftR` n == a `shift` (negate n)
  where types = a :: Int24

prop_rotateI a b = (a `rotate` b) `rotate` (negate b) == a
  where types = (a :: Int24, b :: Int)

prop_compI a = complement (complement a) == a
  where types = a :: Int24

#if MIN_VERSION_base(4,8,0)
prop_clzI a = countLeadingZeros a == countLeadingZeros' a
  where
    countLeadingZeros' :: Int24 -> Int
    countLeadingZeros' x = (w-1) - go (w-1)
      where
        go i | i < 0       = i -- no bit set
             | testBit x i = i
             | otherwise   = go (i-1)

        w = finiteBitSize x

prop_ctzI a = countTrailingZeros a == countTrailingZeros' a
  where
    countTrailingZeros' :: Int24 -> Int
    countTrailingZeros' x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)
        w = finiteBitSize x
#endif

-- Int Storable properties
prop_sizeOfI a = sizeOf a == 3
  where types = a :: Int24

prop_alignI a = alignment a == 3
  where types = a :: Int24


-- ----------------------------------------
-- tests
tests = [
 testGroup "Word24"
  [ testGroup "basic" [
    testProperty "add. identity" prop_addIdent
    ,testProperty "mult. identity" prop_multIdent
    ,testProperty "unsigned" prop_unsigned
    ,testProperty "Word16/Word24 conversion" prop_smaller
    ,testProperty "Show" prop_show
    ,testProperty "Read" prop_read
    ,testProperty "addition" prop_adder
    ,testProperty "negate identity" prop_negate
    ,testProperty "absolute value" prop_abs
    ,testProperty "signum" prop_signum
    ,testProperty "Real identity" prop_real
    ]
  ,testGroup "Integral instance" [
    testProperty  "quot" prop_quot
    ,testProperty "rem" prop_rem
    ,testProperty "div" prop_div
    ,testProperty "mod" prop_mod
    ,testProperty "quotRem" prop_quotrem
    ,testProperty "divmod" prop_divmod
    ]
  ,testGroup "Enum instance" [
    testProperty "enum succ" prop_enum1
    ,testProperty "enum pred" prop_enum2
    ,testProperty "toEnum" prop_enum3
    ,testProperty "enumFrom " prop_enum4
    ,testProperty "enumFromTo" prop_enum5
    ,testProperty "enumFromThen" prop_enum6
    ]
  ,testGroup "Bits instance" [
    testProperty "binary and" prop_and
    ,testProperty "binary or" prop_or
    ,testProperty "binary xor" prop_xor
    ,testProperty "binary xor identity" prop_xor_ident
    ,testProperty "binary shiftL" prop_shiftL
    ,testProperty "binary shiftL identity" prop_shiftL_ident
    ,testProperty "binary shift right" prop_shiftR
    ,testProperty "binary shiftR" prop_shiftR2
    ,testProperty "binary rotate" prop_rotate
    ,testProperty "binary complement" prop_comp
    ,testProperty "binary byteSwap24" prop_byteSwap
#if MIN_VERSION_base(4,8,0)
    ,testProperty "binary countLeadingZeros" prop_clz
    ,testProperty "binary countTrailingZeros" prop_ctz
    ,testProperty "binary countTrailingZeros 0" (prop_ctz (0::Word24))
#endif
    ,testProperty "bit/testBit" (prop_bit_ident (0::Word24))
    ,testProperty "popCount"    (prop_popCount (0::Word24) (0::Word))
    ]
  ,testGroup "Storable instance" [
    testProperty  "sizeOf Word24" prop_sizeOf
    ,testProperty "aligntment" prop_align
    ]
  ]
 ,testGroup "Int24"
  [testGroup "basic" [
    testProperty "add. identity" prop_addIdentI
    ,testProperty "mult. identity" prop_multIdentI
    ,testProperty "Int16/Int24 conversion" prop_smallerI
    ,testProperty "Show" prop_showI
    ,testProperty "Read" prop_readI
    ,testProperty "addition" prop_adderI
    ,testProperty "negate identity" prop_negateI
    ,testProperty "absolute value" prop_absI
    ,testProperty "signum" prop_signumI
    ,testProperty "Real identity" prop_realI
    ]
  ,testGroup "Integral instance" [
    testProperty  "quot" prop_quotI
    ,testProperty "rem" prop_remI
    ,testProperty "div" prop_divI
    ,testProperty "mod" prop_modI
    ,testProperty "quotRem" prop_quotremI
    ,testProperty "divmod" prop_divmodI
    ]
  ,testGroup "Enum instance" [
    testProperty "enum succ" prop_enum1I
    ,testProperty "enum pred" prop_enum2I
    ,testProperty "toEnum" prop_enum3I
    ,testProperty "enumFrom " prop_enum4I
    ,testProperty "enumFromTo" prop_enum5I
    ,testProperty "enumFromThen" prop_enum6I
    ]
  ,testGroup "Bits instance" [
    testProperty "binary and" prop_andI
    ,testProperty "binary or" prop_orI
    ,testProperty "binary xor" prop_xorI
    ,testProperty "binary xor identity" prop_xor_identI
    ,testProperty "binary shiftL" prop_shiftLI
    ,testProperty "binary shiftL identity" prop_shiftL_identI
    ,testProperty "binary shift right" prop_shiftRI
    ,testProperty "binary shiftR" prop_shiftR2I
    ,testProperty "binary rotate" prop_rotateI
    ,testProperty "binary complement" prop_compI
#if MIN_VERSION_base(4,8,0)
    ,testProperty "binary countLeadingZeros" prop_clzI
    ,testProperty "binary countTrailingZeros" prop_ctzI
    ,testProperty "binary countTrailingZeros 0" (prop_ctzI (0::Int24))
#endif
    ,testProperty "bit/testBit" (prop_bit_ident (0::Int24))
    ,testProperty "popCount"    (prop_popCount (0::Int24) (0::Int))
    ]
  ,testGroup "Storable instance" [
    testProperty  "sizeOf Int24" prop_sizeOfI
    ,testProperty "aligntment" prop_alignI
    ]
  ]
 ]

