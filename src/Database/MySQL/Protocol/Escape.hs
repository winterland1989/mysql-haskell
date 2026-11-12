{-# LANGUAGE BangPatterns #-}

{-|
Module      : Database.MySQL.Protocol.Escape
Description : Pure haskell mysql escape
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Escaping for the body of a single-quoted string literal, the form
'Database.MySQL.Protocol.MySQLValue.putTextField' renders text and bytes in.

A quote is doubled. A backslash, a double quote and NUL get a backslash in
front of them, and backspace, tab, newline, carriage return and Control-Z
(0x1A) are written as @\\b@, @\\t@, @\\n@, @\\r@ and @\\Z@; every other
character is kept as it is. In the default @sql_mode@ the server reads the
literal back as the original value, and the result cannot end a double-quoted
literal either, though a quote reads as two there.

Under @NO_BACKSLASH_ESCAPES@ the literal still ends at its closing quote, but
a backslash is an ordinary character, so the server stores every backslash
written: a backslash is stored twice, and a double quote or one of those
control characters is stored after a backslash. That changes the stored
value; it cannot end the literal.

Not safe on a connection whose character set lets a multibyte character end
in the backslash byte, 0x5C: gbk, big5, sjis, cp932 and gb18030. There the
escaping backslash can become the second byte of a character, and the
backslash in the data then escapes a quote, so a parameter can end its
literal, with @SET NAMES gbk@ on MySQL 8.0.45 and MariaDB 11.4.9 alike. UTF-8,
the default, and latin1 never put 0x5C inside a character. On those other
character sets, use prepared statements, which send parameters separately.

reference: <https://dev.mysql.com/doc/refman/8.0/en/string-literals.html>

-}

module Database.MySQL.Protocol.Escape (escapeText, escapeBytes) where

import           Control.Monad.ST         (ST)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as B
import           Data.Text                (Text)
import qualified Data.Text.Array          as TA
import qualified Data.Text.Internal       as T
import           Data.Word
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (Ptr, minusPtr, plusPtr)
import           Foreign.Storable         (peek, poke, pokeByteOff)
import           GHC.IO                   (unsafeDupablePerformIO)

-- Decision: keep 1.4.1's escapes but double the quote, whose @\'@ ended the
-- literal under @NO_BACKSLASH_ESCAPES@. 1.5.0 also sent control characters
-- and the double quote raw, which changed the SQL text of many more
-- parameters. See Note [Backslashes without backslash escapes].

-- Note [Backslashes without backslash escapes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Under @NO_BACKSLASH_ESCAPES@ a backslash is an ordinary character, so the
-- server stores every backslash the escaping writes: @C:\@, rendered as
-- @'C:\\'@, is stored as @C:\\@, and a newline, rendered as @'\n'@, as a
-- backslash and an @n@. Sending control characters and the double quote raw,
-- as 1.5.0 did, stores those right in both modes, but the backslash itself
-- cannot be sent raw: in the default mode @'C:\'@ escapes its own closing
-- quote. Rendering for the session's mode needs the
-- SERVER_STATUS_NO_BACKSLASH_ESCAPES flag of the server's OK packets, kept
-- per connection. 'Database.MySQL.Connection.MySQLConn' and its
-- 'Database.MySQL.Connection.ReplyState' are exported with their
-- constructors, so a place for that flag changes the API: that waits for a
-- major release.

-- | Escape text for a single-quoted literal, see the module header.
escapeText :: Text -> Text
escapeText (T.Text array offset len) = if len <= 0
    then T.empty
    else
        let (escaped, escapedLength) = TA.run2 $ do
                target <- TA.new (len * 2)
                escapeUnits array (offset + len) target offset 0
        in T.Text escaped 0 escapedLength

-- | Copies the code units from @source@ to @sourceEnd@ into @target@, from
-- @targetIndex@ on, escaped; it returns @target@ and the index after the last
-- unit written. A unit is a byte of UTF-8 (text 2) or a 16-bit unit of
-- UTF-16 (text 1); neither encoding puts an ASCII unit inside a longer
-- character, so units can be escaped one by one.
escapeUnits :: TA.Array -> Int -> TA.MArray s -> Int -> Int -> ST s (TA.MArray s, Int)
escapeUnits array sourceEnd target !source !targetIndex = if source == sourceEnd
    then return (target, targetIndex)
    else do
        let unit = TA.unsafeIndex array source
        case unitEscape unit of
            Kept -> do
                TA.unsafeWrite target targetIndex unit
                escapeUnits array sourceEnd target (source + 1) (targetIndex + 1)
            Escaped first second -> do
                TA.unsafeWrite target targetIndex first
                TA.unsafeWrite target (targetIndex + 1) second
                escapeUnits array sourceEnd target (source + 1) (targetIndex + 2)

-- | Escape bytes for a single-quoted literal, see the module header.
escapeBytes :: ByteString -> ByteString
escapeBytes (B.PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \ a ->
    B.createUptoN (len * 2) $ \ b -> do
        b' <- escapeByteRange (a `plusPtr` s) (a `plusPtr` s `plusPtr` len) b
        return (b' `minusPtr` b)

-- | Copies the bytes from @source@ to @sourceEnd@ to @target@, escaped; it
-- returns the address after the last byte written.
escapeByteRange :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
escapeByteRange !source sourceEnd !target = if source == sourceEnd
    then return target
    else do
        byte <- peek source
        case unitEscape byte of
            Kept -> do
                poke target byte
                escapeByteRange (source `plusPtr` 1) sourceEnd (target `plusPtr` 1)
            Escaped first second -> do
                poke target first
                pokeByteOff target 1 second
                escapeByteRange (source `plusPtr` 1) sourceEnd (target `plusPtr` 2)

-- | How one byte, or one 16-bit unit of UTF-16, is written.
data UnitEscape unit = Kept | Escaped unit unit

-- | NUL becomes a backslash and a NUL rather than @\\0@, as in 1.4.1; the
-- server reads both as NUL.
unitEscape :: (Eq unit, Num unit) => unit -> UnitEscape unit
unitEscape unit = if
    | unit == quote -> Escaped quote quote
    | unit == backslash || unit == doubleQuote || unit == 0 -> Escaped backslash unit
    | unit == 8 -> Escaped backslash 0x62 -- \b
    | unit == 9 -> Escaped backslash 0x74 -- \t
    | unit == 10 -> Escaped backslash 0x6E -- \n
    | unit == 13 -> Escaped backslash 0x72 -- \r
    | unit == 26 -> Escaped backslash 0x5A -- \Z
    | otherwise -> Kept
{-# INLINE unitEscape #-}

quote, doubleQuote, backslash :: Num unit => unit
quote = 0x27
doubleQuote = 0x22
backslash = 0x5C
