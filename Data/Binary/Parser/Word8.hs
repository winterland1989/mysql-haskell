{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
-- |
-- Module      :  Data.Binary.Parser.Word8
-- Copyright   :  Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'B.ByteString' strings.
--
module Data.Binary.Parser.Word8 where

import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Get.Internal
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Unsafe   as B
import           Data.Word
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (minusPtr, plusPtr)
import qualified Foreign.Storable         as Storable (Storable (peek))
import           Prelude                  hiding (takeWhile)

#if MIN_VERSION_bytestring(0,10,6)
import           Data.ByteString.Internal (accursedUnutterablePerformIO)
#else
import           Data.ByteString.Internal (inlinePerformIO)

{-# INLINE accursedUnutterablePerformIO #-}
-- | You must be truly desperate to come to me for help.
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO = inlinePerformIO
#endif

--------------------------------------------------------------------------------

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
peekMaybe :: Get (Maybe Word8)
peekMaybe = do
    e <- isEmpty
    if e then return Nothing
         else Just <$> peek
{-# INLINE peekMaybe #-}

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
--
peek :: Get Word8
peek = do
    ensureN 1
    bs <- get
    return (B.unsafeHead bs)
{-# INLINE peek #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
--
satisfy :: (Word8 -> Bool) -> Get Word8
satisfy p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if p w then put (B.unsafeTail bs) >> return w
           else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
--
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Get a
satisfyWith f p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
        r = f w
    if p r then put (B.unsafeTail bs) >> return r
           else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- | Match a specific byte.
--
word8 :: Word8 -> Get ()
word8 c = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if c == w then put (B.unsafeTail bs)
              else fail "word8"
{-# INLINE word8 #-}

-- | Match any byte.
--
anyWord8 :: Get Word8
anyWord8 = getWord8
{-# INLINE anyWord8 #-}

-- | The parser @skipWord8 p@ succeeds for any byte for which the predicate @p@ returns 'True'.
--
skipWord8 :: (Word8 -> Bool) -> Get ()
skipWord8 p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if p w then put (B.unsafeTail bs)
              else fail "skip"
{-# INLINE skipWord8 #-}

--------------------------------------------------------------------------------

-- | This is a faster version of 'skip' for small N (smaller than chunk size).
--
skipN :: Int -> Get ()
skipN n = do
    bs <- get
    let l = B.length bs
    if l > n then put (B.unsafeDrop n bs)
             else skip n
{-# INLINE skipN #-}

-- | Consume input as long as the predicate returns 'False' or reach the end of input,
-- and return the consumed input.
--
takeTill :: (Word8 -> Bool) -> Get ByteString
takeTill p = do
    bs <- get
    let (want, rest) = B.break p bs
    put rest
    if B.null rest then B.concat . reverse <$> go [want] else return want
  where
    go acc = do
        e <- isEmpty -- isEmpty will draw input here
        if e
        then return acc
        else do
            bs <- get
            let (want, rest) = B.break p bs
                acc' = want : acc
            put rest
            if B.null rest then go acc' else return acc'
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True' or reach the end of input,
-- and return the consumed input.
--
takeWhile :: (Word8 -> Bool) -> Get ByteString
takeWhile p = do
    bs <- get
    let (want, rest) = B.span p bs
    put rest
    if B.null rest then B.concat . reverse <$> go [want] else return want
  where
    go acc = do
        e <- isEmpty
        if e
        then return acc
        else do
            bs <- get
            let (want, rest) = B.span p bs
                acc' = want : acc
            put rest
            if B.null rest then go acc' else return acc'
{-# INLINE takeWhile #-}

-- | Similar to 'takeWhile', but requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or reach the end of input
--
takeWhile1 :: (Word8 -> Bool) -> Get ByteString
takeWhile1 p = do
    bs <- takeWhile p
    if B.null bs then fail "takeWhile1" else return bs
{-# INLINE takeWhile1 #-}

-- | Skip past input for as long as the predicate returns 'True'.
--
skipWhile :: (Word8 -> Bool) -> Get ()
skipWhile p = do
    bs <- get
    let rest = B.dropWhile p bs
    put rest
    when (B.null rest) go
  where
    go = do
        e <- isEmpty
        unless e $ do
            bs <- get
            let rest = B.dropWhile p bs
            put rest
            when (B.null rest) go
{-# INLINE skipWhile #-}

-- | Skip over white space using 'isSpace'.
--
skipSpaces :: Get ()
skipSpaces = skipWhile isSpace
{-# INLINE skipSpaces #-}

-- | @string s@ parses a sequence of bytes that identically match @s@.
--
string :: ByteString -> Get ()
string bs = do
    let l = B.length bs
    bs' <- get
    if l <= B.length bs'              -- current chunk is enough
    then if B.unsafeTake l bs' == bs
        then put (B.unsafeDrop l bs')
        else fail "string"
    else do
        ensureN l
        bs'' <- get
        if B.unsafeTake l bs'' == bs
        then put (B.unsafeDrop l bs'')
        else fail "string"
{-# INLINE string #-}

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
scan :: s -> (s -> Word8 -> Maybe s) -> Get ByteString
scan s0 consume = withInputChunks s0 consume' B.concat (return . B.concat)
  where
    consume' s1 (PS fp off len) = accursedUnutterablePerformIO $
        withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
            go fp off start end start s1
    go fp off start end ptr !s
        | ptr < end = do
            w <- Storable.peek ptr
            case consume s w of
                Just s' -> go fp off start end (ptr `plusPtr` 1) s'
                _       -> do
                    let !len1 = ptr `minusPtr` start
                        !off2 = off + len1
                        !len2 = end `minusPtr` ptr
                    return (Right (PS fp off len1, PS fp off2 len2))
        | otherwise = return (Left s)
{-# INLINE scan #-}

-- | Similar to 'scan', but working on 'ByteString' chunks, The predicate
-- consumes a 'ByteString' chunk and transforms a state argument,
-- and each transformed state is passed to successive invocations of
-- the predicate on each chunk of the input until one chunk got splited to
-- @Right (ByteString, ByteString)@ or the input ends.
--
scanChunks :: s -> Consume s -> Get ByteString
scanChunks s consume = withInputChunks s consume B.concat (return . B.concat)
{-# INLINE scanChunks #-}

--------------------------------------------------------------------------------

-- | Fast 'Word8' predicate for matching ASCII space characters
--
-- >isSpace w = w == 32 || w - 9 <= 4
--
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w - 9 <= 4
{-# INLINE isSpace #-}

-- | Decimal digit predicate.
--
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Hex digit predicate.
--
isHexDigit :: Word8 -> Bool
isHexDigit w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
{-# INLINE isHexDigit #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
--
isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == 32 || w == 9
{-# INLINE isHorizontalSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
--
isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10
{-# INLINE isEndOfLine #-}

--------------------------------------------------------------------------------

-- | Match either a single newline byte @\'\\n\'@, or a carriage
-- return followed by a newline byte @\"\\r\\n\"@.
endOfLine :: Get ()
endOfLine = do
    w <- getWord8
    case w of
        10 -> return ()
        13 -> word8 10
        _  -> fail "endOfLine"
{-# INLINE endOfLine #-}
