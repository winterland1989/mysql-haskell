module Word24(tests) where

import Prelude as P

import Test.Tasty
import           Test.Tasty.QuickCheck    (testProperty)

import Test.QuickCheck hiding ((.&.))

import Orphans()
import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24
import Data.Bits
import Foreign.Storable
import GHC.Real

prop_enum3 :: Int -> Bool
prop_addIdent :: Word24 -> Bool
prop_multIdent :: Word24 -> Bool
prop_unsigned :: Word24 -> Bool
prop_smaller :: Word16 -> Bool
prop_show :: Word16 -> Bool
prop_read :: Word24 -> Bool
prop_adder :: Word16 -> Word16 -> Property
prop_negate :: Word24 -> Bool
prop_abs :: Word24 -> Bool
prop_signum :: Word24 -> Bool
prop_real :: Word24 -> Bool
prop_enum1 :: Word24 -> Property
prop_enum2 :: Word24 -> Property
prop_enum4 :: Word24 -> Property
prop_enum5 :: Word24 -> Word8 -> Bool
prop_enum6 :: Word24 -> Word24 -> Bool
prop_quot :: Word24 -> Word24 -> Property
prop_rem :: Word24 -> Word24 -> Property
prop_div :: Word24 -> Word24 -> Property
prop_mod :: Word24 -> Word24 -> Property
prop_quotrem :: Word24 -> Word24 -> Property
prop_divmod :: Word24 -> Word24 -> Property
prop_and :: Word16 -> Word16 -> Bool
prop_or :: Word16 -> Word16 -> Bool
prop_xor :: Word16 -> Word16 -> Bool
prop_xor_ident :: Word24 -> Word24 -> Bool
prop_shiftL :: Word24 -> Bool
prop_shiftR :: Word24 -> Property
prop_shiftR2 :: Word24 -> Int -> Property
prop_shiftL_ident :: Word24 -> Bool
prop_rotate :: Word24 -> Int -> Bool
prop_comp :: Word24 -> Bool
prop_byteSwap :: Word24 -> Bool
prop_clz :: Word24 -> Bool
prop_ctz :: Word24 -> Bool
prop_bit_ident :: Bits a => a -> NonNegative Int -> Bool
prop_sizeOf :: Word24 -> Bool
prop_align :: Word24 -> Bool
prop_addIdentI :: Int24 -> Bool
prop_multIdentI :: Int24 -> Bool
prop_smallerI :: Int16 -> Bool
prop_showI :: Int16 -> Bool
prop_readI :: Int24 -> Bool
prop_adderI :: Int16 -> Int16 -> Bool
prop_negateI :: Int24 -> Bool
prop_absI :: Int24 -> Bool
prop_signumI :: Int24 -> Bool
prop_realI :: Int24 -> Bool
prop_enum1I :: Int24 -> Property
prop_enum2I :: Int24 -> Property
prop_enum3I :: Int -> Bool
prop_enum4I :: Int24 -> Property
prop_enum5I :: Int24 -> Word8 -> Bool
prop_enum6I :: Int24 -> Int24 -> Bool
-- | Int24 division throws for a zero divisor and, like 'Int' quot, for
-- minBound divided by -1 (Data.Int.Int24 raises overflowError there).
int24DivisionDefined :: Int24 -> Int24 -> Bool
int24DivisionDefined a b = b /= 0 && not (a == minBound && b == (-1))

prop_quotI :: Int24 -> Int24 -> Property
prop_remI :: Int24 -> Int24 -> Property
prop_divI :: Int24 -> Int24 -> Property
prop_modI :: Int24 -> Int24 -> Property

-- ----------------------------------------
-- Word24 Properties

prop_addIdent a = a - a == 0

prop_multIdent a = a * 1 == a

prop_unsigned a = a >= 0

prop_smaller a = (fromIntegral ((fromIntegral a) :: Word24) :: Word16) == a

prop_show a = show a == show (fromIntegral a :: Word24)

prop_read a = a == (read . show) a

prop_adder a b = (a < ub) && (b < ub) ==>
                 fromIntegral (a + b) ==
                 ((fromIntegral a + fromIntegral b) :: Word24)
     where
        ub :: Word16
        ub = maxBound `div` 2

prop_negate a = a == negate (negate a)

prop_abs a = a == abs a

prop_signum a = if a == 0 then signum a == a else signum a == 1

prop_real a = let r = toRational a in numerator r == fromIntegral a

-- Word24 Enum properties
prop_enum1 a = a < maxBound ==> succ a == a + 1

prop_enum2 a = a > minBound ==> pred a == a - 1

prop_enum3 a = let a' = abs a in
               toEnum a' == fromIntegral @_ @Integer a'

prop_enum4 a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]

prop_enum5 a b = let b' = fromIntegral b in
                 enumFromTo a (a + b') ==
                 map fromIntegral (enumFromTo (fromIntegral a :: Integer)
                                             (fromIntegral (a + b') :: Integer))

prop_enum6 a b = take 2 (enumFromThen a b) == [a,b]

-- Word24 Integral properties
prop_quot a b = b /= 0 ==>
  quot a b == fromIntegral (quot (fromIntegral a :: Word32) (fromIntegral b :: Word32))

prop_rem a b = b /= 0 ==>
  rem a b == fromIntegral (rem (fromIntegral a :: Word32) (fromIntegral b :: Word32))

prop_div a b = b /= 0 ==>
  div a b == fromIntegral (div (fromIntegral a :: Word32) (fromIntegral b :: Word32))

prop_mod a b = b /= 0 ==>
  mod a b == fromIntegral (mod (fromIntegral a :: Word32) (fromIntegral b :: Word32))

prop_quotrem a b = b /= 0 ==> let (j, k) = quotRem a b in
  a == (b * j) + k

prop_divmod a b = b /= 0 ==>
  divMod a b == (div a b, mod a b)

-- binary Word properties
prop_and a b = (a .&. b) == fromIntegral ( (fromIntegral a :: Word24) .&. (fromIntegral b :: Word24))

prop_or a b = (a .|. b) == fromIntegral ( (fromIntegral a :: Word24) .|. (fromIntegral b :: Word24))

prop_xor a b = (a `xor` b) == fromIntegral ( (fromIntegral a :: Word24) `xor` (fromIntegral b :: Word24))

prop_xor_ident a b = (a `xor` b) `xor` b == a

prop_shiftL a = a `shiftL` 1 == a * 2

prop_shiftR a = a < maxBound `div` 2 ==>
  (a * 2) `shift` (-1) == a

prop_shiftR2 a n = n >= 0 ==> a `shiftR` n == a `shift` (negate n)

prop_shiftL_ident a = a `shiftL` 0 == a

prop_rotate a b = (a `rotate` b) `rotate` (negate b) == a

prop_comp a = complement (complement a) == a

prop_byteSwap a = byteSwap24 (byteSwap24 a) == a

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

prop_bit_ident q (NonNegative j) = testBit (bit j `asTypeOf` q) j == (j < 24)

prop_popCount :: (Integral a1, Num a2, FiniteBits a1,
                        FiniteBits a2) =>
                       a1 -> a2 -> a1 -> Bool
prop_popCount s t a = if a >= 0
  then popCount (a `asTypeOf` s) == popCount (fromIntegral a `asTypeOf` t)
  else
    finiteBitSize s - popCount (a `asTypeOf` s) == finiteBitSize t - popCount (fromIntegral a `asTypeOf` t)

-- Word Storable properties
prop_sizeOf a = sizeOf a == 3

prop_align a = alignment a == 3



-- ----------------------------------------
-- Int24 Properties

prop_addIdentI a = a - a == 0

prop_multIdentI a = a * 1 == a

prop_smallerI a = (fromIntegral ((fromIntegral a) :: Int24) :: Int16) == a

prop_showI a = show a == show (fromIntegral a :: Int24)

prop_readI a = a == (read . show) a

prop_adderI a b = ((fromIntegral a + fromIntegral b) :: Int) ==
                  fromIntegral ((fromIntegral a + fromIntegral b) :: Int24)

prop_negateI a = a == negate (negate a)

prop_absI a = if a >= 0 then a == abs a else a == negate (abs a)

prop_signumI a = signum a == fromIntegral (signum (fromIntegral a :: Int))

prop_realI a = let r = toRational a in numerator r == fromIntegral a

-- Int24 Enum Properties
prop_enum1I a = a < maxBound ==> succ a == a + 1

prop_enum2I a = a > minBound ==> pred a == a - 1

prop_enum3I a = let a' = abs a in
               toEnum a' == fromIntegral @_ @Integer a'

prop_enum4I a = a < maxBound ==> take 2 (enumFrom a) == [a, a+1]

prop_enum5I a b = let b' = fromIntegral b in
                 enumFromTo a (a + b') ==
                 map fromIntegral (enumFromTo (fromIntegral a :: Integer)
                                             (fromIntegral (a + b') :: Integer))

prop_enum6I a b = take 2 (enumFromThen a b) == [a,b]

-- Int24 Integral properties
prop_quotI a b = int24DivisionDefined a b ==>
  quot a b == fromIntegral (quot (fromIntegral a :: Int32) (fromIntegral b :: Int32))

prop_remI a b = int24DivisionDefined a b ==>
  rem a b == fromIntegral (rem (fromIntegral a :: Int32) (fromIntegral b :: Int32))

prop_divI a b = int24DivisionDefined a b ==>
  div a b == fromIntegral (div (fromIntegral a :: Int32) (fromIntegral b :: Int32))

prop_modI a b = int24DivisionDefined a b ==>
  mod a b == fromIntegral (mod (fromIntegral a :: Int32) (fromIntegral b :: Int32))

prop_quotremI :: Int24 -> Int24 -> Property
prop_quotremI a b = int24DivisionDefined a b ==> let (j, k) = quotRem a b in
  a == (b * j) + k

prop_divmodI :: Int24 -> Int24 -> Property
prop_divmodI a b = int24DivisionDefined a b ==>
  divMod a b == (div a b, mod a b)


-- binary Int properties
prop_andI :: Int16 -> Int16 -> Bool
prop_andI a b = (a .&. b) == fromIntegral ( (fromIntegral a :: Int24) .&. (fromIntegral b :: Int24))

prop_orI :: Int16 -> Int16 -> Bool
prop_orI a b = (a .|. b) == fromIntegral ( (fromIntegral a :: Int24) .|. (fromIntegral b :: Int24))

prop_xorI :: Int16 -> Int16 -> Bool
prop_xorI a b = (a `xor` b) == fromIntegral ( (fromIntegral a :: Int24) `xor` (fromIntegral b :: Int24))

prop_xor_identI :: Int24 -> Int24 -> Bool
prop_xor_identI a b = (a `xor` b) `xor` b == a

prop_shiftLI :: Int24 -> Bool
prop_shiftLI a = a `shiftL` 1 == a * 2

prop_shiftL_identI :: Int24 -> Bool
prop_shiftL_identI a = a `shiftL` 0 == a

prop_shiftRI :: Int24 -> Property
prop_shiftRI a = (a < maxBound `div` 2) && (a > minBound `div` 2) ==>
  (a * 2) `shift` (-1) == a

prop_shiftR2I :: Int24 -> Int -> Property
prop_shiftR2I a n = n >= 0 ==> a `shiftR` n == a `shift` (negate n)

prop_rotateI :: Int24 -> Int -> Bool
prop_rotateI a b = (a `rotate` b) `rotate` (negate b) == a

prop_compI :: Int24 -> Bool
prop_compI a = complement (complement a) == a

prop_clzI :: Int24 -> Bool
prop_clzI a = countLeadingZeros a == countLeadingZeros' a
  where
    countLeadingZeros' :: Int24 -> Int
    countLeadingZeros' x = (w-1) - go (w-1)
      where
        go i | i < 0       = i -- no bit set
             | testBit x i = i
             | otherwise   = go (i-1)

        w = finiteBitSize x

prop_ctzI :: Int24 -> Bool
prop_ctzI a = countTrailingZeros a == countTrailingZeros' a
  where
    countTrailingZeros' :: Int24 -> Int
    countTrailingZeros' x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)
        w = finiteBitSize x

-- Int Storable properties
prop_sizeOfI :: Int24 -> Bool
prop_sizeOfI a = sizeOf a == 3

prop_alignI :: Int24 -> Bool
prop_alignI a = alignment a == 3


-- ----------------------------------------
-- tests
tests :: [TestTree]
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
    ,testProperty "binary countLeadingZeros" prop_clz
    ,testProperty "binary countTrailingZeros" prop_ctz
    ,testProperty "binary countTrailingZeros 0" (prop_ctz (0::Word24))
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
    ,testProperty "binary countLeadingZeros" prop_clzI
    ,testProperty "binary countTrailingZeros" prop_ctzI
    ,testProperty "binary countTrailingZeros 0" (prop_ctzI (0::Int24))
    ,testProperty "bit/testBit" (prop_bit_ident (0::Int24))
    ,testProperty "popCount"    (prop_popCount (0::Int24) (0::Int))
    ]
  ,testGroup "Storable instance" [
    testProperty  "sizeOf Int24" prop_sizeOfI
    ,testProperty "aligntment" prop_alignI
    ]
  ]
 ]

