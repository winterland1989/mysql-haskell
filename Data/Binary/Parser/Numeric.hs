{-# LANGUAGE CPP          #-}
-- |
-- Module      :  Data.Binary.Parser.Numeric
-- Copyright   :  Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for numeric values.
--
module Data.Binary.Parser.Numeric where

import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get.Internal
import qualified Data.Binary.Parser.Word8     as W
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lex.Integral as LexInt
import           Data.Int
import           Data.Scientific              (Scientific (..))
import qualified Data.Scientific              as Sci
import           Data.Word

#define  MINUS    45
#define  PLUS     43
#define  LITTLE_E 101
#define  BIG_E    69
#define  DOT      46

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
--
hexadecimal :: (Integral a, Bits a) => Get a
hexadecimal = do
    bs <- W.takeWhile1 W.isHexDigit
    case LexInt.readHexadecimal bs of
        Just (x, _) -> return x
        Nothing -> fail "hexadecimal: impossible"
{-# SPECIALISE hexadecimal :: Get Int #-}
{-# SPECIALISE hexadecimal :: Get Int8 #-}
{-# SPECIALISE hexadecimal :: Get Int16 #-}
{-# SPECIALISE hexadecimal :: Get Int32 #-}
{-# SPECIALISE hexadecimal :: Get Int64 #-}
{-# SPECIALISE hexadecimal :: Get Integer #-}
{-# SPECIALISE hexadecimal :: Get Word #-}
{-# SPECIALISE hexadecimal :: Get Word8 #-}
{-# SPECIALISE hexadecimal :: Get Word16 #-}
{-# SPECIALISE hexadecimal :: Get Word32 #-}
{-# SPECIALISE hexadecimal :: Get Word64 #-}

-- | Parse and decode an unsigned decimal number.
--
decimal :: Integral a => Get a
decimal = do
    bs <- W.takeWhile1 W.isDigit
    return $! LexInt.readDecimal_ bs
{-# SPECIALISE decimal :: Get Int #-}
{-# SPECIALISE decimal :: Get Int8 #-}
{-# SPECIALISE decimal :: Get Int16 #-}
{-# SPECIALISE decimal :: Get Int32 #-}
{-# SPECIALISE decimal :: Get Int64 #-}
{-# SPECIALISE decimal :: Get Integer #-}
{-# SPECIALISE decimal :: Get Word #-}
{-# SPECIALISE decimal :: Get Word8 #-}
{-# SPECIALISE decimal :: Get Word16 #-}
{-# SPECIALISE decimal :: Get Word32 #-}
{-# SPECIALISE decimal :: Get Word64 #-}

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
--
signed :: Num a => Get a -> Get a
signed p = do
    w <- W.peek
    if w == MINUS
        then W.skipN 1 >> negate <$> p
        else if w == PLUS then W.skipN 1 >> p else p
{-# SPECIALISE signed :: Get Int -> Get Int #-}
{-# SPECIALISE signed :: Get Int8 -> Get Int8 #-}
{-# SPECIALISE signed :: Get Int16 -> Get Int16 #-}
{-# SPECIALISE signed :: Get Int32 -> Get Int32 #-}
{-# SPECIALISE signed :: Get Int64 -> Get Int64 #-}
{-# SPECIALISE signed :: Get Integer -> Get Integer #-}

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
--
rational :: Fractional a => Get a
rational = scientifically realToFrac
{-# SPECIALIZE rational :: Get Double #-}
{-# SPECIALIZE rational :: Get Float #-}
{-# SPECIALIZE rational :: Get Rational #-}
{-# SPECIALIZE rational :: Get Scientific #-}

-- | Parse a rational number and round to 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at least one decimal digit.  The syntax similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ or
-- @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples with behaviour identical to 'read':
--
-- >parseOnly double "3"     == Right ("",1,3.0)
-- >parseOnly double "3.1"   == Right ("",3,3.1)
-- >parseOnly double "3e4"   == Right ("",3,30000.0)
-- >parseOnly double "3.1e4" == Right ("",5,31000.0)
--
-- >parseOnly double ".3"    == Left (".3",0,"takeWhile1")
-- >parseOnly double "e3"    == Left ("e3",0,"takeWhile1")
--
-- Examples of differences from 'read':
--
-- >parseOnly double "3.foo" == Right (".foo",1,3.0)
-- >parseOnly double "3e"    == Right ("e",1,3.0)
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
--
double :: Get Double
double = scientifically Sci.toRealFloat

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientific :: Get Scientific
scientific = scientifically id

-- | Parse a scientific number and convert to result using a user supply function.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
scientifically :: (Scientific -> a) -> Get a
scientifically h = do
    sign <- W.peek
    when (sign == PLUS || sign == MINUS) (W.skipN 1)
    intPart <- decimal
    sci <- (do fracDigits <- W.word8 DOT >> W.takeWhile1 W.isDigit
               let e' = B.length fracDigits
                   intPart' = intPart * (10 ^ e')
                   fracPart = LexInt.readDecimal_ fracDigits
               parseE (intPart' + fracPart) e'
           ) <|> (parseE intPart 0)

    if sign /= MINUS then return $! h sci else return $! h (negate sci)
  where
    parseE c e =
        (do _ <- W.satisfy (\w -> w ==  LITTLE_E || w == BIG_E)
            (Sci.scientific c . (subtract e) <$> signed decimal)) <|> return (Sci.scientific c (negate e))
    {-# INLINE parseE #-}
{-# INLINE scientifically #-}
