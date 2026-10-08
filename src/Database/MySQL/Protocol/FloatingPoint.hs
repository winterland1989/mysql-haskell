{-|
Module      : Database.MySQL.Protocol.FloatingPoint
Description : FLOAT and DOUBLE text, correctly rounded
Copyright   : (c) Jappie Klooster, 2026
License     : BSD3
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

The text protocol sends FLOAT and DOUBLE values as decimal text, large ones in
exponent form such as @1e20@. 'readDouble' and 'readFloat' give the nearest
value, as 'read' does, and are fast for the short numbers MySQL usually writes.
See Note [Correctly rounded floating point text].
-}
module Database.MySQL.Protocol.FloatingPoint
  ( readDouble
  , readFloat
  ) where

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lex.Fractional as LexFrac
import           Data.Scientific                (Scientific, toRealFloat)
import           Data.Word                      (Word64, Word8)
import qualified Unwitch.Convert.Word64         as Word64
import qualified Unwitch.Convert.Word8          as Word8

{- Note [Correctly rounded floating point text]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scaling decimal digits by @10 ^^ e@ in floating point rounds more than once.
bytestring-lexing's readExponential missed the nearest Double for 83% of
20,000 shown doubles, and its readDecimal, which also stops at an @e@ and so
read @1e20@ as 1, missed 2 of 243 plain decimals. Reading the text exactly as a
'Scientific' and rounding once with 'toRealFloat' missed none, but cost about
14,000 instructions per DOUBLE field.

Clinger's fast path avoids that cost: when the significant digits, read as an
integer, convert to a Double exactly (at most 2^53, so up to 15 digits always
do) and the power of ten is at most 22 either way, that power is exact as
well, so one multiplication or division rounds once and gives the nearest
Double. A Float takes integers up to 2^24 and powers up to 10. MySQL writes
most values within these bounds; the rest go the exact way.

The integer is accumulated from at most 19 digits, so it cannot wrap around a
'Word64'. A number with more digits has a 19-digit prefix of at least 10^18,
which no Double holds exactly, so it always goes the exact way.
-}

-- | The nearest 'Double' to the decimal number at the start of the bytes:
-- @-?digits(.digits)?([eE][+-]?digits)?@. Bytes after it are ignored.
readDouble :: ByteString -> Maybe Double
readDouble bytes = do
    decimal <- scanDecimal bytes
    if  | abs (decimalExponent decimal) <= 22 ->
            -- Right only when the significand is exact in a Double (at most
            -- 2^53); see Note [Correctly rounded floating point text].
            case Word64.toDouble (decimalSignificand decimal) of
                Right exactDigits -> Just (signed decimal (scaleExactly exactDigits (decimalExponent decimal)))
                Left _            -> exactly bytes
        | otherwise -> exactly bytes

-- | 'readDouble' for 'Float'.
readFloat :: ByteString -> Maybe Float
readFloat bytes = do
    decimal <- scanDecimal bytes
    if  | abs (decimalExponent decimal) <= 10 ->
            -- Right only when the significand is exact in a Float (at most 2^24).
            case Word64.toFloat (decimalSignificand decimal) of
                Right exactDigits -> Just (signed decimal (scaleExactly exactDigits (decimalExponent decimal)))
                Left _            -> exactly bytes
        | otherwise -> exactly bytes

-- | The significand times @10 ^ powerOfTen@, with one rounding: the caller
-- guarantees that the significand and the power of ten are both exact.
scaleExactly :: RealFloat a => a -> Int -> a
scaleExactly exactDigits powerOfTen =
    if powerOfTen >= 0
    then exactDigits * 10 ^ powerOfTen
    else exactDigits / 10 ^ negate powerOfTen

signed :: Num a => DecimalText -> a -> a
signed decimal = signed' (decimalSign decimal)

signed' :: Num a => Sign -> a -> a
signed' sign magnitude = case sign of
    Positive -> magnitude
    Negative -> negate magnitude

-- | The text read exactly as a 'Scientific', then rounded once.
exactly :: RealFloat a => ByteString -> Maybe a
exactly bytes = toRealFloat <$> exactScientific bytes

exactScientific :: ByteString -> Maybe Scientific
exactScientific bytes = fst <$> LexFrac.readSigned LexFrac.readExponential bytes

data Sign = Positive | Negative

-- | A decimal number in text, as far as the fast path needs it.
data DecimalText = DecimalText
    { decimalSign        :: !Sign
    , decimalSignificand :: !Word64
      -- ^ the first 19 digits from the first non-zero one on, without the
      -- point, as one integer; see Note [Correctly rounded floating point text]
    , decimalExponent    :: !Int
      -- ^ the power of ten the significand is scaled by; 'maxBound' when the
      -- written exponent has more digits than the fast path takes
    }

-- | The number at the start of the bytes, or 'Nothing' when they do not
-- start with one.
scanDecimal :: ByteString -> Maybe DecimalText
scanDecimal bytes =
    let (sign, afterSign) = splitSign bytes
        (integerPart, afterInteger) = B.span isDigit afterSign
        (fractionPart, afterFraction) = splitFraction afterInteger
        digits = integerPart <> fractionPart
        significant = B.dropWhile (== 48) digits  -- '0'
    in if B.null integerPart
       then Nothing
       else Just DecimalText
           { decimalSign        = sign
           , decimalSignificand = B.foldl' appendDigit 0 (B.take 19 significant)
           , decimalExponent    = writtenExponent afterFraction `minusDigits` B.length fractionPart
           }

-- | The exponent after @e@ or @E@, 0 when there is none; 'maxBound' when it
-- has more than 6 digits, which no FLOAT or DOUBLE needs.
writtenExponent :: ByteString -> Int
writtenExponent bytes = case B.uncons bytes of
    Just (marker, rest) ->
        if marker == 101 || marker == 69          -- 'e', 'E'
        then exponentDigits rest
        else 0
    Nothing -> 0

exponentDigits :: ByteString -> Int
exponentDigits bytes =
    let (sign, afterSign) = splitSign bytes
        digits = B.takeWhile isDigit afterSign
    in if  | B.null digits       -> 0
           | B.length digits > 6 -> maxBound
           | otherwise           -> signed' sign (B.foldl' appendDigitInt 0 digits)

-- | A leading @-@ or @+@, and the bytes after it.
splitSign :: ByteString -> (Sign, ByteString)
splitSign bytes = case B.uncons bytes of
    Nothing -> (Positive, bytes)
    Just (byte, rest) ->
        if  | byte == 45 -> (Negative, rest)  -- '-'
            | byte == 43 -> (Positive, rest)  -- '+'
            | otherwise  -> (Positive, bytes)

-- | The digits after a decimal point, and the bytes after them; a point
-- without digits after it is not part of the number.
splitFraction :: ByteString -> (ByteString, ByteString)
splitFraction bytes = case B.uncons bytes of
    Nothing -> (B.empty, bytes)
    Just (byte, rest) ->
        let (fraction, remaining) = B.span isDigit rest
        in if byte == 46 && not (B.null fraction)  -- '.'
           then (fraction, remaining)
           else (B.empty, bytes)

-- | Keeps 'maxBound', the mark of an exponent too long to read, as it is.
minusDigits :: Int -> Int -> Int
minusDigits written fractionDigits =
    if written == maxBound then maxBound else written - fractionDigits

isDigit :: Word8 -> Bool
isDigit byte = byte >= 48 && byte <= 57

appendDigit :: Word64 -> Word8 -> Word64
appendDigit accumulated digit = accumulated * 10 + Word8.toWord64 (digit - 48)

appendDigitInt :: Int -> Word8 -> Int
appendDigitInt accumulated digit = accumulated * 10 + Word8.toInt (digit - 48)
