{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE UnliftedFFITypes #-}

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

import           Data.Bits
import           Data.Word
import qualified Z.Data.Builder           as B
import qualified Z.Data.Text              as T
import qualified Z.Data.Vector            as V
import           Z.Foreign

escapeText :: T.Text -> B.Builder ()
escapeText = escapeBytes . T.getUTF8Bytes

escapeBytes :: V.Bytes -> B.Builder ()
escapeBytes v =
    B.ensureN (V.length v `unsafeShiftL` 2) $ \ (MutablePrimArray mba) moff ->
        withPrimVectorUnsafe v $ \ ba off len ->
            escape_mysql_string ba off len mba moff

foreign import ccall unsafe escape_mysql_string :: BA# Word8 -> Int -> Int
                                                -> MBA# Word8 -> Int
                                                -> IO Int
