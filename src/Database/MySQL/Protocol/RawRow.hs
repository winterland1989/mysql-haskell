{-# LANGUAGE UnboxedSums   #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Database.MySQL.Protocol.RawRow
Description : Rows as their bytes plus the bounds of each field
Copyright   : (c) Jappie Klooster, 2026
License     : BSD3
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

A 'RawRow' is a row's bytes plus where each of its fields starts and how long
it is, found in one pass over the row. A field can then be read by its column
number without walking the row again, which is what a library decoding rows by
column number needs. "Database.MySQL.Decoder" reads 'RawRow's.
-}
module Database.MySQL.Protocol.RawRow
  ( -- * Protocols
    TextProtocol
  , BinaryProtocol
    -- * Raw rows
  , RawRow
  , rawRowBytes
  , rawRowFieldCount
  , RawField(..)
  , rawField
    -- * Building raw rows
  , textRawRow
  , BinaryWidth(..)
  , binaryWidth
  , binaryRawRow
    -- * Internal utilities
  , binaryNullMapLength
  , isNullInMap
  ) where

import           Control.Monad.ST                   (ST, runST)
import           Data.Bits                          (testBit, unsafeShiftR,
                                                     (.&.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Unsafe             as B
import           Data.Kind                          (Type)
import qualified Data.Vector                        as V
import qualified Data.Vector.Unboxed                as VU
import qualified Data.Vector.Unboxed.Mutable        as VUM
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue (RowError (..),
                                                     RowErrorKind (..),
                                                     lengthEncodedInt)

-- | Rows of a plain query, whose fields are text.
--
-- @since 1.3.4
data TextProtocol

-- | Rows of a prepared statement, whose fields are binary.
--
-- @since 1.3.4
data BinaryProtocol

-- | A row's bytes and the bounds of each of its fields. The @protocol@,
-- 'TextProtocol' or 'BinaryProtocol', says how the fields are encoded, so a
-- decoder for the other protocol cannot be run on it.
--
-- @since 1.3.4
data RawRow (protocol :: Type) = RawRow
    { rawRowBytes  :: !ByteString
      -- ^ the row packet's body
    , rawRowBounds :: !(VU.Vector Int)
      -- ^ field @i@ starts at element @2i@ and its length is element @2i + 1@,
      -- or 'nullFieldLength' for a NULL
    }

-- | @since 1.3.4
rawRowFieldCount :: RawRow protocol -> Int
rawRowFieldCount row = VU.length (rawRowBounds row) `unsafeShiftR` 1

-- | One field of a 'RawRow'.
--
-- @since 1.3.4
data RawField
    = RawNull
    | RawBytes !ByteString
      -- ^ the field's bytes, without a length prefix: the value as text in
      -- 'TextProtocol', its binary encoding in 'BinaryProtocol'
    | RawAbsent
      -- ^ the row has no field with that column number
    deriving (Show, Eq)

-- | The field of a column, counting from 0. Inlined so that a caller matching
-- on the result never allocates the 'RawField'.
--
-- @since 1.3.4
rawField :: RawRow protocol -> Int -> RawField
rawField row@(RawRow bytes bounds) column =
    -- Compared with the field count, not @2 * column + 1@ with the bounds'
    -- length: doubling a large column number overflows and passes the check.
    if column < 0 || column >= rawRowFieldCount row
    then RawAbsent
    else
        let start       = VU.unsafeIndex bounds (2 * column)
            fieldLength = VU.unsafeIndex bounds (2 * column + 1)
        in if fieldLength == nullFieldLength
           then RawNull
           else RawBytes (B.unsafeTake fieldLength (B.unsafeDrop start bytes))
{-# INLINE rawField #-}

nullFieldLength :: Int
nullFieldLength = -1

-- | The bounds of the first @columnCount@ fields of a text-protocol row. Bytes
-- after the last field are ignored, as 'decodeTextRow' ignores them.
--
-- @since 1.3.4
textRawRow :: Int -> ByteString -> Either RowError (RawRow TextProtocol)
textRawRow columnCount row = runST $ do
    bounds <- VUM.unsafeNew (2 * columnCount)
    outcome <- fillTextBounds row bounds columnCount 0 0
    case outcome of
        Left rowError -> pure (Left rowError)
        Right ()      -> Right . RawRow row <$> VU.unsafeFreeze bounds

-- | Records the bounds of the fields from @column@ on, the first of which
-- starts at byte @offset@.
fillTextBounds :: ByteString -> VUM.MVector s Int -> Int -> Int -> Int -> ST s (Either RowError ())
fillTextBounds row bounds columnCount column offset =
    if  | column >= columnCount -> pure (Right ())
        | offset >= B.length row -> pure (Left (RowError column offset RowEndsEarly))
        | B.unsafeIndex row offset == 0xFB -> do
            writeBounds bounds column offset nullFieldLength
            fillTextBounds row bounds columnCount (column + 1) (offset + 1)
        | otherwise -> case lengthEncodedInt row offset of
            (# kind | #) -> pure (Left (RowError column offset kind))
            (# | (# fieldLength, fieldStart #) #) ->
                if fieldLength > B.length row - fieldStart
                then pure (Left (RowError column offset RowEndsEarly))
                else do
                    writeBounds bounds column fieldStart fieldLength
                    fillTextBounds row bounds columnCount (column + 1) (fieldStart + fieldLength)

writeBounds :: VUM.MVector s Int -> Int -> Int -> Int -> ST s ()
writeBounds bounds column start fieldLength = do
    VUM.unsafeWrite bounds (2 * column) start
    VUM.unsafeWrite bounds (2 * column + 1) fieldLength

-- | How the binary protocol lays out the non-NULL fields of a column.
--
-- @since 1.3.4
data BinaryWidth
    = BinaryFixed !Int
      -- ^ always this many bytes: the integers, FLOAT, DOUBLE and YEAR
    | BinaryLengthEncoded
      -- ^ a length, then that many bytes: strings, DECIMAL, BIT, and the dates
      -- and times, whose length says which of their parts are present
    | BinaryNoBytes
      -- ^ a column of type NULL, whose fields the NULL map marks
    deriving (Show, Eq)

-- | @since 1.3.4
binaryWidth :: ColumnDef -> BinaryWidth
binaryWidth column =
    if  | t == mySQLTypeTiny -> BinaryFixed 1
        | t == mySQLTypeShort || t == mySQLTypeYear -> BinaryFixed 2
        | t == mySQLTypeLong || t == mySQLTypeInt24 || t == mySQLTypeFloat -> BinaryFixed 4
        | t == mySQLTypeLongLong || t == mySQLTypeDouble -> BinaryFixed 8
        | t == mySQLTypeNull -> BinaryNoBytes
        | otherwise -> BinaryLengthEncoded
  where
    t = columnType column

-- | The bounds of the fields of a binary-protocol row: a 0x00 header, a NULL
-- map with a bit per column from bit 2 on, then the non-NULL fields, laid out
-- as each column's 'BinaryWidth' says.
--
-- @since 1.3.4
binaryRawRow :: V.Vector BinaryWidth -> ByteString -> Either RowError (RawRow BinaryProtocol)
binaryRawRow widths row =
    let nullMapLength = binaryNullMapLength (V.length widths)
    in if 1 + nullMapLength > B.length row
       then Left (RowError 0 0 RowEndsEarly)
       else runST $ do
           bounds <- VUM.unsafeNew (2 * V.length widths)
           outcome <- fillBinaryBounds row widths bounds 0 (1 + nullMapLength)
           case outcome of
               Left rowError -> pure (Left rowError)
               Right ()      -> Right . RawRow row <$> VU.unsafeFreeze bounds

fillBinaryBounds :: ByteString -> V.Vector BinaryWidth -> VUM.MVector s Int -> Int -> Int
                 -> ST s (Either RowError ())
fillBinaryBounds row widths bounds column offset =
    if  | column >= V.length widths -> pure (Right ())
        | isNullInMap row column -> do
            writeBounds bounds column offset nullFieldLength
            fillBinaryBounds row widths bounds (column + 1) offset
        | otherwise -> case V.unsafeIndex widths column of
            BinaryNoBytes -> do
                writeBounds bounds column offset 0
                fillBinaryBounds row widths bounds (column + 1) offset
            BinaryFixed width ->
                if width > B.length row - offset
                then pure (Left (RowError column offset RowEndsEarly))
                else do
                    writeBounds bounds column offset width
                    fillBinaryBounds row widths bounds (column + 1) (offset + width)
            BinaryLengthEncoded ->
                if offset >= B.length row
                then pure (Left (RowError column offset RowEndsEarly))
                else case lengthEncodedInt row offset of
                    (# kind | #) -> pure (Left (RowError column offset kind))
                    (# | (# fieldLength, fieldStart #) #) ->
                        if fieldLength > B.length row - fieldStart
                        then pure (Left (RowError column offset RowEndsEarly))
                        else do
                            writeBounds bounds column fieldStart fieldLength
                            fillBinaryBounds row widths bounds (column + 1) (fieldStart + fieldLength)

-- | The bytes of a binary-protocol row's NULL map, which holds a bit per
-- column from bit 2 on.
binaryNullMapLength :: Int -> Int
binaryNullMapLength columnCount = (columnCount + 7 + 2) `unsafeShiftR` 3

-- | The NULL map starts after the header byte; column @i@ is bit @i + 2@. The
-- row must be long enough to hold the map.
isNullInMap :: ByteString -> Int -> Bool
isNullInMap row column =
    testBit (B.unsafeIndex row (1 + ((column + 2) `unsafeShiftR` 3))) ((column + 2) .&. 7)
