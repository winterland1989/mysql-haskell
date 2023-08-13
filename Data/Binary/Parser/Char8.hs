-- |
-- Module      :  Data.Binary.Parser.Char8
-- Copyright   :  Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- This module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or
-- ISO-8859-15.  It /does not/ make any attempt to deal with character
-- encodings, multibyte characters, or wide characters.  In
-- particular, all attempts to use characters above code point U+00FF
-- will give wrong answers.
--
-- Code points below U+0100 are simply translated to and from their
-- numeric values, so e.g. the code point U+00A4 becomes the byte
-- @0xA4@ (which is the Euro symbol in ISO-8859-15, but the generic
-- currency sign in ISO-8859-1).  Haskell 'Char' values above U+00FF
-- are truncated, so e.g. U+1D6B7 is truncated to the byte @0xB7@.

module Data.Binary.Parser.Char8 where

import           Control.Applicative
import qualified Data.Binary.Get          as BG
import           Data.Binary.Get.Internal
import qualified Data.Binary.Parser.Word8 as W
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Unsafe   as B
import           Prelude                  hiding (takeWhile)

--------------------------------------------------------------------------------

-- | Match any char, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Get (Maybe Char)
peekMaybe = fmap w2c <$> W.peekMaybe
{-# INLINE peekMaybe #-}

-- | Match any char, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: Get Char
peek = w2c <$> W.peek
{-# INLINE peek #-}

-- | The parser @satisfy p@ succeeds for any char for which the
-- predicate @p@ returns 'True'. Returns the char that is actually
-- parsed.
--
satisfy :: (Char -> Bool) -> Get Char
satisfy p = w2c <$> W.satisfy (p . w2c)
{-# INLINE satisfy #-}

-- | The parser @satisfyWith f p@ transforms a char, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed char that was parsed.
--
satisfyWith :: (Char -> a) -> (a -> Bool) -> Get a
satisfyWith f = W.satisfyWith (f . w2c)
{-# INLINE satisfyWith #-}

-- | Match a specific character.
--
char :: Char -> Get ()
char c = W.word8 (c2w c)
{-# INLINE char #-}

-- | Match any character.
--
anyChar :: Get Char
anyChar = w2c <$> BG.getWord8
{-# INLINE anyChar #-}

-- | The parser @skipChar p@ succeeds for any char for which the predicate @p@ returns 'True'.
--
skipChar :: (Char -> Bool) -> Get ()
skipChar p = W.skipWord8 (p . w2c)
{-# INLINE skipChar #-}

--------------------------------------------------------------------------------

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Char -> Bool) -> Get ByteString
takeTill p = W.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Char -> Bool) -> Get ByteString
takeWhile p = W.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

-- Similar to 'takeWhile', but requires the predicate to succeed on at least one char
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Char -> Bool) -> Get ByteString
takeWhile1 p = W.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Char -> Bool) -> Get ()
skipWhile p = W.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Satisfy a literal string but ignoring case.
--
stringCI :: ByteString -> Get ByteString
stringCI bs = do
    let l = B.length bs
    ensureN l
    bs' <- B.unsafeTake l <$> get
    if B.map toLower bs' == B.map toLower bs
    then put (B.unsafeDrop l bs') >> return bs'
    else fail "stringCI"
  where
    toLower w | w >= 65 && w <= 90 = w + 32
              | otherwise          = w
{-# INLINE stringCI #-}

--------------------------------------------------------------------------------

-- | Fast predicate for matching ASCII space characters.
--
-- /Note/: This predicate only gives correct answers for the ASCII
-- encoding.  For instance, it does not recognise U+00A0 (non-breaking
-- space) as a space character, even though it is a valid ISO-8859-15
-- byte. For a Unicode-aware and only slightly slower predicate,
-- use 'Data.Char.isSpace'
--
isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- | Decimal digit predicate.
--
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | Hex digit predicate.
--
isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
{-# INLINE isHexDigit #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
--
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
{-# INLINE isHorizontalSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
--
isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\r' || c == '\n'
{-# INLINE isEndOfLine #-}
