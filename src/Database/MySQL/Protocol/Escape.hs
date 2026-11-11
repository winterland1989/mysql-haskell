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
The result is only safe between single quotes: a double quote is not escaped.

A quote is doubled and a backslash escaped with a backslash; every other
character is kept as it is. Under @NO_BACKSLASH_ESCAPES@ the literal reads the
same, except that each backslash is stored twice.

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

-- Decision: escape only the quote, by doubling it, and the backslash, with a
-- backslash: the MySQL manual says only these two must be escaped in a
-- single-quoted literal. Escaping the quote as @\'@ ended the literal under
-- @NO_BACKSLASH_ESCAPES@, and the other escapes mysql_real_escape_string
-- writes (@\0 \n \r \Z \"@) only make logs readable while storing a
-- stray backslash in that mode. test/ParameterEscaping.hs round-trips every
-- byte 0x00 to 0xFF raw on MariaDB and MySQL. See Note [Backslashes without
-- backslash escapes].

-- Note [Backslashes without backslash escapes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Under @NO_BACKSLASH_ESCAPES@ a backslash is an ordinary character, so
-- @C:\@, rendered as @'C:\\'@, is stored as @C:\\@. Leaving the backslash
-- unescaped is no way out: in the default mode @'C:\'@ escapes its own
-- closing quote. Rendering for the session's mode needs the
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
-- @targetIndex@ on, doubling quotes and backslashes; it returns @target@ and
-- the index after the last unit written. A unit is a byte of UTF-8 (text 2)
-- or a 16-bit unit of UTF-16 (text 1); neither encoding puts a quote or a
-- backslash inside a longer character, so units can be copied one by one.
escapeUnits :: TA.Array -> Int -> TA.MArray s -> Int -> Int -> ST s (TA.MArray s, Int)
escapeUnits array sourceEnd target !source !targetIndex = if source == sourceEnd
    then return (target, targetIndex)
    else do
        let unit = TA.unsafeIndex array source
        TA.unsafeWrite target targetIndex unit
        if unit == quote || unit == backslash
            then do
                TA.unsafeWrite target (targetIndex + 1) unit
                escapeUnits array sourceEnd target (source + 1) (targetIndex + 2)
            else escapeUnits array sourceEnd target (source + 1) (targetIndex + 1)

-- | Escape bytes for a single-quoted literal, see the module header.
escapeBytes :: ByteString -> ByteString
escapeBytes (B.PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \ a ->
    B.createUptoN (len * 2) $ \ b -> do
        b' <- escapeByteRange (a `plusPtr` s) (a `plusPtr` s `plusPtr` len) b
        return (b' `minusPtr` b)

-- | Copies the bytes from @source@ to @sourceEnd@ to @target@, doubling
-- quotes and backslashes; it returns the address after the last byte written.
escapeByteRange :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
escapeByteRange !source sourceEnd !target = if source == sourceEnd
    then return target
    else do
        byte <- peek source
        poke target byte
        if byte == quote || byte == backslash
            then do
                pokeByteOff target 1 byte
                escapeByteRange (source `plusPtr` 1) sourceEnd (target `plusPtr` 2)
            else escapeByteRange (source `plusPtr` 1) sourceEnd (target `plusPtr` 1)

quote, backslash :: Num unit => unit
quote = 0x27
backslash = 0x5C
