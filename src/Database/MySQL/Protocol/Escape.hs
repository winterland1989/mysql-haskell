{-# LANGUAGE BangPatterns #-}

{-|
Module      : Database.MySQL.Protocol.Escape
Description : Pure haskell mysql escape
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide escape machinery for bytes and text types.

reference: <http://dev.mysql.com/doc/refman/5.7/en/string-literals.html>

    * Escape Sequence	Character Represented by Sequence
    * \0              	An ASCII NUL (X'00') character
    * \'              	A single quote (“'”) character
    * \"              	A double quote (“"”) character
    * \b              	A backspace character
    * \n              	A newline (linefeed) character
    * \r              	A carriage return character
    * \t              	A tab character
    * \Z              	ASCII 26 (Control+Z); see note following the table
    * \\              	A backslash (“\”) character
    * \%              	A “%” character; see note following the table
    * \_              	A “_” character; see note following the table

The @\%@ and @\_@ sequences are used to search for literal instances of @%@ and @_@ in pattern-matching contexts where they would otherwise be interpreted as wildcard characters, so we won't auto escape @%@ or @_@ here.

-}

module Database.MySQL.Protocol.Escape where

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

escapeText :: Text -> Text
escapeText (T.Text arr off len)
    | len <= 0  = T.empty
    | otherwise =
        let (arr', len') =  TA.run2 $ do
                marr <- TA.new (len * 2)
                loop arr (off + len) marr off 0
        in T.Text arr' 0 len'
  where
    escape c marr ix = do
        TA.unsafeWrite marr ix 92
        TA.unsafeWrite marr (ix+1) c

    loop oarr oend marr !ix !ix'
        | ix == oend = return (marr, ix')
        | otherwise  = do
            let c = TA.unsafeIndex oarr ix
                go1 = loop oarr oend marr (ix+1) (ix'+1)
                go2 = loop oarr oend marr (ix+1) (ix'+2)
            if  | c >= 0xD800 && c <= 0xDBFF  -> do let c2 = TA.unsafeIndex oarr (ix+1)
                                                    TA.unsafeWrite marr ix' c
                                                    TA.unsafeWrite marr (ix'+1) c2
                                                    loop oarr oend marr (ix+2) (ix'+2)
                | c == 0
                    || c == 39
                    || c == 34 -> escape c   marr ix' >> go2 -- \0 \' \"
                | c == 8       -> escape 98  marr ix' >> go2 -- \b
                | c == 10      -> escape 110 marr ix' >> go2 -- \n
                | c == 13      -> escape 114 marr ix' >> go2 -- \r
                | c == 9       -> escape 116 marr ix' >> go2 -- \t
                | c == 26      -> escape 90  marr ix' >> go2 -- \Z
                | c == 92      -> escape 92  marr ix' >> go2 -- \\

                | otherwise    -> TA.unsafeWrite marr ix' c >> go1

escapeBytes :: ByteString -> ByteString
escapeBytes (B.PS fp s len) = unsafeDupablePerformIO $ withForeignPtr fp $ \ a ->
    B.createUptoN (len * 2) $ \ b -> do
        b' <- loop (a `plusPtr` s) (a `plusPtr` s `plusPtr` len) b
        return (b' `minusPtr` b)
  where
    escape :: Word8 -> Ptr Word8 -> IO (Ptr Word8)
    escape c p = do
        poke p 92
        pokeByteOff p 1 c
        return (p `plusPtr` 2)

    loop !a aend !b
        | a == aend = return b
        | otherwise = do
            c <- peek a
            if  | c == 0
                    || c == 39
                    || c == 34 -> escape c   b >>= loop (a `plusPtr` 1) aend -- \0 \' \"
                | c == 8       -> escape 98  b >>= loop (a `plusPtr` 1) aend -- \b
                | c == 10      -> escape 110 b >>= loop (a `plusPtr` 1) aend -- \n
                | c == 13      -> escape 114 b >>= loop (a `plusPtr` 1) aend -- \r
                | c == 9       -> escape 116 b >>= loop (a `plusPtr` 1) aend -- \t
                | c == 26      -> escape 90  b >>= loop (a `plusPtr` 1) aend -- \Z
                | c == 92      -> escape 92  b >>= loop (a `plusPtr` 1) aend -- \\

                | otherwise    -> poke b c >> loop (a `plusPtr` 1) aend (b `plusPtr` 1)
