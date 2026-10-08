{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE UnboxedSums   #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Database.MySQL.Protocol.MySQLValue
Description : Text and binary protocol
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Core text and binary row decoder/encoder machinery.

-}

module Database.MySQL.Protocol.MySQLValue
  ( -- * MySQLValue decoder and encoder
    MySQLValue(..)
  , putParamMySQLType
  , getTextField
  , putTextField
  , getTextRow
  , getTextRowVector
    -- * Text rows with the column types resolved once per result set
  , TextColumn(..)
  , TextValue(..)
  , textColumn
  , decodeTextRow
  , decodeTextRowVector
  , TextRowError(..)
  , TextRowErrorKind(..)
  , TextFieldError(..)
  , describeTextRowError
  , getBinaryField
  , putBinaryField
  , getBinaryRow
  , getBinaryRowVector
  -- * Internal utilities
  , getBits
  , BitMap(..)
  , isColumnSet
  , isColumnNull
  , makeNullMap
  ) where

import qualified Blaze.Text                         as Textual
import           Control.Monad
import           Data.Binary.Put
import           Data.Binary.Parser
import           Data.Bits
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Builder            as BB
import           Data.ByteString.Builder.Scientific (FPFormat (..),
                                                     formatScientificBuilder)
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString.Lex.Fractional     as LexFrac
import qualified Data.ByteString.Lex.Integral       as LexInt
import qualified Data.ByteString.Unsafe             as B
import           Data.Fixed                         (Pico)
import           Data.Int
import           Data.Scientific                    (Scientific)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           Data.Time.Calendar                 (Day, fromGregorian,
                                                     toGregorian)
import           Data.Time.Format                   (defaultTimeLocale,
                                                     formatTime)
import           Data.Time.LocalTime                (LocalTime (..),
                                                     TimeOfDay (..))
import           Data.Word
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Escape
import           Database.MySQL.Protocol.Packet
import           GHC.Generics                       (Generic)
import qualified Data.Vector                        as V
import qualified Unwitch.Convert.Word64             as Word64
import qualified Unwitch.Convert.Word8              as Word8

--------------------------------------------------------------------------------
-- | Data type mapping between MySQL values and haskell values.
--
-- There're some subtle differences between MySQL values and haskell values:
--
-- MySQL's @DATETIME@ and @TIMESTAMP@ are different on timezone handling:
--
--  * DATETIME and DATE is just a represent of a calendar date, it has no timezone information involved,
--  you always get the same value as you put no matter what timezone you're using with MySQL.
--
--  * MySQL converts TIMESTAMP values from the current time zone to UTC for storage,
--  and back from UTC to the current time zone for retrieval. If you put a TIMESTAMP with timezone A,
--  then read it with timezone B, you may get different result because of this conversion, so always
--  be careful about setting up the right timezone with MySQL, you can do it with a simple @SET time_zone = timezone;@
--  for more info on timezone support, please read <http://dev.mysql.com/doc/refman/5.7/en/time-zone-support.html>
--
--  So we use 'LocalTime' to present both @DATETIME@ and @TIMESTAMP@, but the local here is different.
--
-- MySQL's @TIME@ type can present time of day, but also elapsed time or a time interval between two events.
-- @TIME@ values may range from @-838:59:59@ to @838:59:59@, so 'MySQLTime' values consist of a sign and a
-- 'TimeOfDay' whose hour part may exceeded 24. you can use @timeOfDayToTime@ to get the absolute time interval.
--
-- Under MySQL >= 5.7, @DATETIME@, @TIMESTAMP@ and @TIME@ may contain fractional part, which matches haskell's
-- precision.
--
data MySQLValue
    = MySQLDecimal       !Scientific   -- ^ DECIMAL, NEWDECIMAL
    | MySQLInt8U         !Word8        -- ^ Unsigned TINY
    | MySQLInt8          !Int8         -- ^ TINY
    | MySQLInt16U        !Word16       -- ^ Unsigned SHORT
    | MySQLInt16         !Int16        -- ^ SHORT
    | MySQLInt32U        !Word32       -- ^ Unsigned LONG, INT24
    | MySQLInt32         !Int32        -- ^ LONG, INT24
    | MySQLInt64U        !Word64       -- ^ Unsigned LONGLONG
    | MySQLInt64         !Int64        -- ^ LONGLONG
    | MySQLFloat         !Float        -- ^ IEEE 754 single precision format
    | MySQLDouble        !Double       -- ^ IEEE 754 double precision format
    | MySQLYear          !Word16       -- ^ YEAR
    | MySQLDateTime      !LocalTime    -- ^ DATETIME
    | MySQLTimeStamp     !LocalTime    -- ^ TIMESTAMP
    | MySQLDate          !Day              -- ^ DATE
    | MySQLTime          !Word8 !TimeOfDay -- ^ sign(0 = non-negative, 1 = negative) hh mm ss microsecond
                                           -- The sign is OPPOSITE to binlog one !!!
    | MySQLGeometry      !ByteString       -- ^ todo: parsing to something meanful
    | MySQLBytes         !ByteString
    | MySQLBit           !Word64
    | MySQLText          !Text
    | MySQLNull
  deriving (Show, Eq, Generic)

-- | Put 'FieldType' and usigned bit(0x80/0x00) for 'MySQLValue's.
--
putParamMySQLType :: MySQLValue -> Put
putParamMySQLType (MySQLDecimal      _)  = putFieldType mySQLTypeDecimal  >> putWord8 0x00
putParamMySQLType (MySQLInt8U        _)  = putFieldType mySQLTypeTiny     >> putWord8 0x80
putParamMySQLType (MySQLInt8         _)  = putFieldType mySQLTypeTiny     >> putWord8 0x00
putParamMySQLType (MySQLInt16U       _)  = putFieldType mySQLTypeShort    >> putWord8 0x80
putParamMySQLType (MySQLInt16        _)  = putFieldType mySQLTypeShort    >> putWord8 0x00
putParamMySQLType (MySQLInt32U       _)  = putFieldType mySQLTypeLong     >> putWord8 0x80
putParamMySQLType (MySQLInt32        _)  = putFieldType mySQLTypeLong     >> putWord8 0x00
putParamMySQLType (MySQLInt64U       _)  = putFieldType mySQLTypeLongLong >> putWord8 0x80
putParamMySQLType (MySQLInt64        _)  = putFieldType mySQLTypeLongLong >> putWord8 0x00
putParamMySQLType (MySQLFloat        _)  = putFieldType mySQLTypeFloat    >> putWord8 0x00
putParamMySQLType (MySQLDouble       _)  = putFieldType mySQLTypeDouble   >> putWord8 0x00
putParamMySQLType (MySQLYear         _)  = putFieldType mySQLTypeShort    >> putWord8 0x80
putParamMySQLType (MySQLDateTime     _)  = putFieldType mySQLTypeDateTime >> putWord8 0x00
putParamMySQLType (MySQLTimeStamp    _)  = putFieldType mySQLTypeTimestamp>> putWord8 0x00
putParamMySQLType (MySQLDate         _)  = putFieldType mySQLTypeDate     >> putWord8 0x00
putParamMySQLType (MySQLTime       _ _)  = putFieldType mySQLTypeTime     >> putWord8 0x00
putParamMySQLType (MySQLBytes        _)  = putFieldType mySQLTypeBlob     >> putWord8 0x00
putParamMySQLType (MySQLGeometry     _)  = putFieldType mySQLTypeGeometry >> putWord8 0x00
putParamMySQLType (MySQLBit          _)  = putFieldType mySQLTypeLongLong >> putWord8 0x80
putParamMySQLType (MySQLText         _)  = putFieldType mySQLTypeString   >> putWord8 0x00
putParamMySQLType MySQLNull              = putFieldType mySQLTypeNull     >> putWord8 0x00

--------------------------------------------------------------------------------
-- | Text protocol decoder
getTextField :: ColumnDef -> Get MySQLValue
getTextField f = case textColumn f of
    TextColumnNull -> pure MySQLNull
    TextColumnValue fieldType value -> do
        bytes <- getLenEncBytes
        case decodeTextValue fieldType value bytes of
            Left fieldError -> fail (describeTextFieldError fieldError)
            Right decoded   -> pure decoded

{- Note [Text rows decoded once per result set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'getTextRow' runs 'getTextField' for every field of every row, which compares
the column type against two dozen constants before it reads a byte, inside
binary's continuation-based 'Get' over a lazy packet body. Decoding text rows
was 47.5% of select CPU (benchmark/cpu-profile-1.3.3.md).

The query functions in "Database.MySQL.Base" instead resolve each column to a
'TextColumn' once, when the column definitions arrive, and decode every row
with 'decodeTextRow': a walk over the row as one strict 'ByteString' that
returns unboxed sums, so no 'Either' or tuple is allocated per field.

Decision: values stay as lazy as 'getTextField' leaves them, so invalid UTF-8
still raises when the value is used. Evaluating every value while decoding
saved 3% of the instructions when a caller uses every value, and cost 2.75
times the instructions when it uses none (docs/direct-row-decoding.md).

'getTextField' and 'getTextRow' keep their 'Get' interface and share
'textColumn' and 'decodeTextValue', so both paths turn the same bytes into the
same values. They differ only on rows a server does not send: an 8-byte length
above 'maxBound' of 'Int', which 'decodeTextRow' rejects, and a zero-length
BIT field, which is now 0 instead of the byte after it.
-}

-- | How a text-protocol row decodes one column, resolved once per result set
-- by 'textColumn'. See Note [Text rows decoded once per result set].
--
-- @since 1.3.4
data TextColumn
    = TextColumnNull
      -- ^ a column of type NULL: its fields are the NULL marker, which the row
      -- decoder reads, so no other bytes belong to it
    | TextColumnValue !FieldType !TextValue
      -- ^ a length-encoded field; the column type is kept for error messages
    deriving (Show, Eq)

-- | The 'MySQLValue' constructor a length-encoded text-protocol field becomes.
--
-- @since 1.3.4
data TextValue
    = TextDecimal
    | TextInt8U
    | TextInt8
    | TextInt16U
    | TextInt16
    | TextInt32U
    | TextInt32
    | TextInt64U
    | TextInt64
    | TextFloat
    | TextDouble
    | TextYear
    | TextTimeStamp
    | TextDateTime
    | TextDate
    | TextTime
    | TextGeometry
    | TextUtf8         -- ^ a string column in any character set but binary
    | TextBytes        -- ^ a string column in the binary character set
    | TextBit
    | TextUnsupported  -- ^ a column type without a text decoder
    deriving (Show, Eq)

-- | The decoder a column's text-protocol fields need.
--
-- @since 1.3.4
textColumn :: ColumnDef -> TextColumn
textColumn f =
    if  | t == mySQLTypeNull -> TextColumnNull
        | t == mySQLTypeDecimal || t == mySQLTypeNewDecimal -> TextColumnValue t TextDecimal
        | t == mySQLTypeTiny ->
            TextColumnValue t (if isUnsigned then TextInt8U else TextInt8)
        | t == mySQLTypeShort ->
            TextColumnValue t (if isUnsigned then TextInt16U else TextInt16)
        | t == mySQLTypeLong || t == mySQLTypeInt24 ->
            TextColumnValue t (if isUnsigned then TextInt32U else TextInt32)
        | t == mySQLTypeLongLong ->
            TextColumnValue t (if isUnsigned then TextInt64U else TextInt64)
        | t == mySQLTypeFloat -> TextColumnValue t TextFloat
        | t == mySQLTypeDouble -> TextColumnValue t TextDouble
        | t == mySQLTypeYear -> TextColumnValue t TextYear
        | t == mySQLTypeTimestamp || t == mySQLTypeTimestamp2 -> TextColumnValue t TextTimeStamp
        | t == mySQLTypeDateTime || t == mySQLTypeDateTime2 -> TextColumnValue t TextDateTime
        | t == mySQLTypeDate || t == mySQLTypeNewDate -> TextColumnValue t TextDate
        | t == mySQLTypeTime || t == mySQLTypeTime2 -> TextColumnValue t TextTime
        | t == mySQLTypeGeometry -> TextColumnValue t TextGeometry
        | t == mySQLTypeVarChar
            || t == mySQLTypeEnum
            || t == mySQLTypeSet
            || t == mySQLTypeTinyBlob
            || t == mySQLTypeMediumBlob
            || t == mySQLTypeLongBlob
            || t == mySQLTypeBlob
            || t == mySQLTypeVarString
            || t == mySQLTypeString ->
            TextColumnValue t (if isText then TextUtf8 else TextBytes)
        | t == mySQLTypeBit -> TextColumnValue t TextBit
        | otherwise -> TextColumnValue t TextUnsupported
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63

-- | Why the bytes of a text-protocol field did not decode.
--
-- @since 1.3.4
data TextFieldError
    = TextFieldUnparsable !FieldType !ByteString
      -- ^ the bytes are not a value of the column's type
    | TextFieldBitTooWide !Int
      -- ^ a BIT field longer than the 8 bytes of a 'Word64'
    | TextFieldUnsupportedType !FieldType
      -- ^ a column type without a text decoder
    deriving (Show, Eq)

-- | The message 'getTextField' fails with.
describeTextFieldError :: TextFieldError -> String
describeTextFieldError fieldError = case fieldError of
    TextFieldUnparsable fieldType bytes ->
        "Database.MySQL.Protocol.MySQLValue: parsing " ++ show fieldType
            ++ " failed, input: " ++ BC.unpack bytes
    TextFieldBitTooWide width ->
        "Database.MySQL.Protocol.MySQLValue: wrong bit length size: " ++ show width
    TextFieldUnsupportedType fieldType ->
        "Database.MySQL.Protocol.MySQLValue: missing text decoder for " ++ show fieldType

-- | The value of a field's bytes. The lexer runs at once, so a malformed field
-- fails while the row is read; a lexed value and any 'Text' are left lazy,
-- while a constructor around bytes already in hand is built at once, as that
-- is cheaper than a thunk. See Note [Text rows decoded once per result set].
-- Inlined so that 'decodeTextFieldAt' matches the 'Right' away instead of
-- allocating it.
decodeTextValue :: FieldType -> TextValue -> ByteString -> Either TextFieldError MySQLValue
decodeTextValue fieldType value bytes = case value of
    TextDecimal     -> lexedValue fieldType MySQLDecimal lexSignedFraction bytes
    TextInt8U       -> lexedValue fieldType MySQLInt8U lexSignedIntegral bytes
    TextInt8        -> lexedValue fieldType MySQLInt8 lexSignedIntegral bytes
    TextInt16U      -> lexedValue fieldType MySQLInt16U lexSignedIntegral bytes
    TextInt16       -> lexedValue fieldType MySQLInt16 lexSignedIntegral bytes
    TextInt32U      -> lexedValue fieldType MySQLInt32U lexSignedIntegral bytes
    TextInt32       -> lexedValue fieldType MySQLInt32 lexSignedIntegral bytes
    TextInt64U      -> lexedValue fieldType MySQLInt64U lexSignedIntegral bytes
    TextInt64       -> lexedValue fieldType MySQLInt64 lexSignedIntegral bytes
    TextFloat       -> lexedValue fieldType MySQLFloat lexSignedFraction bytes
    TextDouble      -> lexedValue fieldType MySQLDouble lexSignedFraction bytes
    TextYear        -> lexedValue fieldType MySQLYear lexSignedIntegral bytes
    TextTimeStamp   -> lexedValue fieldType MySQLTimeStamp lexLocalTime bytes
    TextDateTime    -> lexedValue fieldType MySQLDateTime lexLocalTime bytes
    TextDate        -> lexedValue fieldType MySQLDate lexDate bytes
    TextTime        -> lexedValue fieldType id lexTime bytes
    TextGeometry    -> Right $! MySQLGeometry bytes
    TextUtf8        -> Right (MySQLText (T.decodeUtf8 bytes))
    TextBytes       -> Right $! MySQLBytes bytes
    TextBit         -> decodeTextBit bytes
    TextUnsupported -> Left (TextFieldUnsupportedType fieldType)
{-# INLINE decodeTextValue #-}

lexedValue :: FieldType -> (a -> MySQLValue) -> (ByteString -> Maybe a) -> ByteString
           -> Either TextFieldError MySQLValue
lexedValue fieldType construct lexer bytes = case lexer bytes of
    Just lexed -> Right (construct lexed)
    Nothing    -> Left (TextFieldUnparsable fieldType bytes)
{-# INLINE lexedValue #-}

-- | A BIT field's bytes, most significant first.
decodeTextBit :: ByteString -> Either TextFieldError MySQLValue
decodeTextBit bytes =
    if B.length bytes > 8
    then Left (TextFieldBitTooWide (B.length bytes))
    else Right $! MySQLBit (B.foldl' appendBitByte 0 bytes)

appendBitByte :: Word64 -> Word8 -> Word64
appendBitByte bits byte = unsafeShiftL bits 8 .|. Word8.toWord64 byte

lexSignedIntegral :: Integral a => ByteString -> Maybe a
lexSignedIntegral bytes = fst <$> LexInt.readSigned LexInt.readDecimal bytes

lexSignedFraction :: Fractional a => ByteString -> Maybe a
lexSignedFraction bytes = fst <$> LexFrac.readSigned LexFrac.readDecimal bytes

-- | @YYYY-MM-DD hh:mm:ss[.fraction]@, as DATETIME and TIMESTAMP fields come.
lexLocalTime :: ByteString -> Maybe LocalTime
lexLocalTime bytes = do
    guard (B.length bytes >= 12)
    LocalTime <$> lexDate bytes <*> lexTimeOfDay (B.drop 11 bytes)

-- | @YYYY-MM-DD@.
lexDate :: ByteString -> Maybe Day
lexDate bytes = do
    (yyyy, rest) <- LexInt.readDecimal bytes
    guard (not (B.null rest))
    (mm, rest') <- LexInt.readDecimal (B.tail rest)
    guard (not (B.null rest'))
    (dd, _) <- LexInt.readDecimal (B.tail rest')
    return (fromGregorian yyyy mm dd)

-- | A TIME field: an optional minus sign before @hh:mm:ss[.fraction]@, where the
-- hours may exceed 24.
lexTime :: ByteString -> Maybe MySQLValue
lexTime bytes = do
    guard (not (B.null bytes))
    if B.index bytes 0 == 45  -- '-'
    then MySQLTime 1 <$> lexTimeOfDay (B.drop 1 bytes)
    else MySQLTime 0 <$> lexTimeOfDay bytes

-- | @hh:mm:ss[.fraction]@.
lexTimeOfDay :: ByteString -> Maybe TimeOfDay
lexTimeOfDay bytes = do
    (hh, rest) <- LexInt.readDecimal bytes
    guard (not (B.null rest))
    (mm, rest') <- LexInt.readDecimal (B.tail rest)
    guard (not (B.null rest'))
    (ss, _) <- LexFrac.readDecimal (B.tail rest')
    return (TimeOfDay hh mm ss)

-- | Where and why a text-protocol row did not decode.
--
-- @since 1.3.4
data TextRowError = TextRowError
    { textRowErrorColumn :: !Int               -- ^ zero-based index of the column
    , textRowErrorOffset :: !Int               -- ^ byte offset of that column's field in the row
    , textRowErrorKind   :: !TextRowErrorKind
    } deriving (Show, Eq)

-- | @since 1.3.4
data TextRowErrorKind
    = TextRowEndsEarly
      -- ^ the row ends before this column's field does
    | TextRowInvalidLengthPrefix !Word8
      -- ^ a field starting with a byte that is neither a length nor the NULL marker
    | TextRowLengthOverflow
      -- ^ an 8-byte length above 'maxBound' of 'Int'
    | TextRowField !TextFieldError
      -- ^ the field's bytes did not decode
    deriving (Show, Eq)

-- | @since 1.3.4
describeTextRowError :: TextRowError -> String
describeTextRowError (TextRowError column offset kind) =
    "Database.MySQL.Protocol.MySQLValue: column " ++ show column ++ " at byte "
        ++ show offset ++ ": " ++ kindDescription
  where
    kindDescription = case kind of
        TextRowEndsEarly                  -> "the row ends inside this field"
        TextRowInvalidLengthPrefix prefix -> "invalid length prefix " ++ show prefix
        TextRowLengthOverflow             -> "length does not fit an Int"
        TextRowField fieldError           -> describeTextFieldError fieldError

-- | The values of a text-protocol row, one per 'TextColumn'. Bytes after the
-- last column are ignored, as 'getTextRow' ignores them.
-- See Note [Text rows decoded once per result set].
--
-- @since 1.3.4
decodeTextRow :: [TextColumn] -> ByteString -> Either TextRowError [MySQLValue]
decodeTextRow columns row = case decodeTextFields row 0 0 columns of
    (# rowError | #) -> Left rowError
    (# | values #)   -> Right values

-- | 'V.Vector' version of 'decodeTextRow'.
--
-- @since 1.3.4
decodeTextRowVector :: V.Vector TextColumn -> ByteString -> Either TextRowError (V.Vector MySQLValue)
decodeTextRowVector columns row =
    V.fromListN (V.length columns) <$> decodeTextRow (V.toList columns) row

-- | The fields of @columns@, the first of which is column @columnIndex@ and
-- starts at byte @offset@ of the row.
decodeTextFields :: ByteString -> Int -> Int -> [TextColumn] -> (# TextRowError | [MySQLValue] #)
decodeTextFields row columnIndex offset columns = case columns of
    [] -> (# | [] #)
    column : laterColumns -> case decodeTextFieldAt row columnIndex offset column of
        (# rowError | #) -> (# rowError | #)
        (# | (# value, nextOffset #) #) ->
            case decodeTextFields row (columnIndex + 1) nextOffset laterColumns of
                (# rowError | #) -> (# rowError | #)
                (# | values #)   -> (# | value : values #)

-- | The field at byte @offset@ and the offset after it. The NULL marker is
-- checked before the column, as 'getTextRow' does.
decodeTextFieldAt :: ByteString -> Int -> Int -> TextColumn
                  -> (# TextRowError | (# MySQLValue, Int #) #)
decodeTextFieldAt row columnIndex offset column =
    if  | offset >= B.length row ->
            (# TextRowError columnIndex offset TextRowEndsEarly | #)
        | B.unsafeIndex row offset == 0xFB -> (# | (# MySQLNull, offset + 1 #) #)
        | otherwise -> case column of
            TextColumnNull -> (# | (# MySQLNull, offset #) #)
            TextColumnValue fieldType value -> case lengthEncodedInt row offset of
                (# kind | #) -> (# TextRowError columnIndex offset kind | #)
                (# | (# fieldLength, fieldStart #) #) ->
                    if fieldLength > B.length row - fieldStart
                    then (# TextRowError columnIndex offset TextRowEndsEarly | #)
                    else case decodeTextValue fieldType value
                                (B.unsafeTake fieldLength (B.unsafeDrop fieldStart row)) of
                        Left fieldError ->
                            (# TextRowError columnIndex offset (TextRowField fieldError) | #)
                        Right decoded -> (# | (# decoded, fieldStart + fieldLength #) #)

-- | The length-encoded integer at byte @offset@, which must be inside the row,
-- and the offset after it.
lengthEncodedInt :: ByteString -> Int -> (# TextRowErrorKind | (# Int, Int #) #)
lengthEncodedInt row offset =
    let prefix = B.unsafeIndex row offset
    in if  | prefix < 0xFB  -> (# | (# Word8.toInt prefix, offset + 1 #) #)
           | prefix == 0xFC -> littleEndianLength row (offset + 1) 2
           | prefix == 0xFD -> littleEndianLength row (offset + 1) 3
           | prefix == 0xFE -> littleEndianLength row (offset + 1) 8
           | otherwise      -> (# TextRowInvalidLengthPrefix prefix | #)

-- | The little-endian length of @width@ bytes at byte @start@, and the offset after it.
littleEndianLength :: ByteString -> Int -> Int -> (# TextRowErrorKind | (# Int, Int #) #)
littleEndianLength row start width =
    if width > B.length row - start
    then (# TextRowEndsEarly | #)
    else case Word64.toInt (B.foldr' prependLittleEndianByte 0
                                (B.unsafeTake width (B.unsafeDrop start row))) of
        Nothing          -> (# TextRowLengthOverflow | #)
        Just fieldLength -> (# | (# fieldLength, start + width #) #)

prependLittleEndianByte :: Word8 -> Word64 -> Word64
prependLittleEndianByte byte higherBytes = unsafeShiftL higherBytes 8 .|. Word8.toWord64 byte

feedLenEncBytes :: FieldType -> (t -> b) -> (ByteString -> Maybe t) -> Get b
feedLenEncBytes typ con parser = do
    bs <- getLenEncBytes
    case parser bs of
        Just v -> return (con v)
        Nothing -> fail $ "Database.MySQL.Protocol.MySQLValue: parsing " ++ show typ ++ " failed, \
                          \input: " ++ BC.unpack bs
{-# INLINE feedLenEncBytes #-}

--------------------------------------------------------------------------------
-- | Text protocol encoder
putTextField :: MySQLValue -> Put
putTextField (MySQLDecimal    n) = putBuilder (formatScientificBuilder Fixed Nothing n)
putTextField (MySQLInt8U      n) = putBuilder (Textual.integral n)
putTextField (MySQLInt8       n) = putBuilder (Textual.integral n)
putTextField (MySQLInt16U     n) = putBuilder (Textual.integral n)
putTextField (MySQLInt16      n) = putBuilder (Textual.integral n)
putTextField (MySQLInt32U     n) = putBuilder (Textual.integral n)
putTextField (MySQLInt32      n) = putBuilder (Textual.integral n)
putTextField (MySQLInt64U     n) = putBuilder (Textual.integral n)
putTextField (MySQLInt64      n) = putBuilder (Textual.integral n)
putTextField (MySQLFloat      x) = putBuilder (Textual.float x)
putTextField (MySQLDouble     x) = putBuilder (Textual.double x)
putTextField (MySQLYear       n) = putBuilder (Textual.integral n)
putTextField (MySQLDateTime  dt) = putInQuotes $
                                      putByteString (BC.pack (formatTime defaultTimeLocale "%F %T%Q" dt))
putTextField (MySQLTimeStamp dt) = putInQuotes $
                                      putByteString (BC.pack (formatTime defaultTimeLocale "%F %T%Q" dt))
putTextField (MySQLDate       d) = putInQuotes $
                                      putByteString (BC.pack (formatTime defaultTimeLocale "%F" d))
putTextField (MySQLTime  sign t) = putInQuotes $ do
                                      when (sign == 1) (putCharUtf8 '-')
                                      putByteString (BC.pack (formatTime defaultTimeLocale "%T%Q" t))
                                      -- this works even for hour > 24
putTextField (MySQLGeometry  bs) = putInQuotes $ putByteString . escapeBytes $ bs
putTextField (MySQLBytes     bs) = putInQuotes $ putByteString . escapeBytes $ bs
putTextField (MySQLText       t) = putInQuotes $
                                      putByteString . T.encodeUtf8 . escapeText $ t
putTextField (MySQLBit        b) = do putBuilder "b\'"
                                      putBuilder . execPut $ putTextBits b
                                      putCharUtf8 '\''
  where
    putTextBits :: Word64 -> Put
    putTextBits word = forM_ [63,62..0] $ \ pos ->
            if word `testBit` pos then putCharUtf8 '1' else putCharUtf8 '0'
    {-# INLINE putTextBits #-}

putTextField MySQLNull           = putBuilder "NULL"

putInQuotes :: Put -> Put
putInQuotes p = putCharUtf8 '\'' >> p >> putCharUtf8 '\''
{-# INLINE putInQuotes #-}

--------------------------------------------------------------------------------
-- | Text row decoder
getTextRow :: [ColumnDef] -> Get [MySQLValue]
getTextRow fs = forM fs $ \ f -> do
    p <- peek
    if p == 0xFB
    then skipN 1 >> return MySQLNull
    else getTextField f
{-# INLINE getTextRow #-}

getTextRowVector :: V.Vector ColumnDef -> Get (V.Vector MySQLValue)
getTextRowVector fs = V.forM fs $ \ f -> do
    p <- peek
    if p == 0xFB
    then skipN 1 >> return MySQLNull
    else getTextField f
{-# INLINE getTextRowVector #-}

--------------------------------------------------------------------------------
-- | Binary protocol decoder
getBinaryField :: ColumnDef -> Get MySQLValue
getBinaryField f
    | t == mySQLTypeNull              = pure MySQLNull
    | t == mySQLTypeDecimal
        || t == mySQLTypeNewDecimal   = feedLenEncBytes t MySQLDecimal fracLexer
    | t == mySQLTypeTiny              = if isUnsigned then MySQLInt8U <$> getWord8
                                                      else MySQLInt8  <$> getInt8
    | t == mySQLTypeShort             = if isUnsigned then MySQLInt16U <$> getWord16le
                                                      else MySQLInt16  <$> getInt16le
    | t == mySQLTypeLong
        || t == mySQLTypeInt24        = if isUnsigned then MySQLInt32U <$> getWord32le
                                                      else MySQLInt32  <$> getInt32le
    | t == mySQLTypeYear              = MySQLYear <$> getWord16le
    | t == mySQLTypeLongLong          = if isUnsigned then MySQLInt64U <$> getWord64le
                                                      else MySQLInt64  <$> getInt64le
    | t == mySQLTypeFloat             = MySQLFloat  <$> getFloatle
    | t == mySQLTypeDouble            = MySQLDouble <$> getDoublele
    | t == mySQLTypeTimestamp
        || t == mySQLTypeTimestamp2   = do
            n <- getLenEncInt
            case n of
               0 -> pure $ MySQLTimeStamp (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
               4 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   pure $ MySQLTimeStamp (LocalTime d (TimeOfDay 0 0 0))
               7 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4
                   pure $ MySQLTimeStamp (LocalTime d td)
               11 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8
                   pure $ MySQLTimeStamp (LocalTime d td)
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong TIMESTAMP length"
    | t == mySQLTypeDateTime
        || t == mySQLTypeDateTime2    = do
            n <- getLenEncInt
            case n of
               0 -> pure $ MySQLDateTime (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
               4 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   pure $ MySQLDateTime (LocalTime d (TimeOfDay 0 0 0))
               7 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4
                   pure $ MySQLDateTime (LocalTime d td)
               11 -> do
                   d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8
                   pure $ MySQLDateTime (LocalTime d td)
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong DATETIME length"

    | t == mySQLTypeDate
        || t == mySQLTypeNewDate      = do
            n <- getLenEncInt
            case n of
               0 -> pure $ MySQLDate (fromGregorian 0 0 0)
               4 -> MySQLDate <$> (fromGregorian <$> getYear <*> getInt8' <*> getInt8')
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong DATE length"

    | t == mySQLTypeTime
        || t == mySQLTypeTime2        = do
            n <- getLenEncInt
            case n of
               0 -> pure $ MySQLTime 0 (TimeOfDay 0 0 0)
               8 -> do
                   sign <- getWord8   -- is_negative(1 if minus, 0 for plus)
                   d <- fromIntegral <$> getWord32le
                   h <-  getInt8'
                   MySQLTime sign <$> (TimeOfDay (d*24 + h) <$> getInt8' <*> getSecond4)

               12 -> do
                   sign <- getWord8   -- is_negative(1 if minus, 0 for plus)
                   d <- fromIntegral <$> getWord32le
                   h <-  getInt8'
                   MySQLTime sign <$> (TimeOfDay (d*24 + h) <$> getInt8' <*> getSecond8)
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong TIME length"

    | t == mySQLTypeGeometry          = MySQLGeometry <$> getLenEncBytes
    | t == mySQLTypeVarChar
        || t == mySQLTypeEnum
        || t == mySQLTypeSet
        || t == mySQLTypeTinyBlob
        || t == mySQLTypeMediumBlob
        || t == mySQLTypeLongBlob
        || t == mySQLTypeBlob
        || t == mySQLTypeVarString
        || t == mySQLTypeString       = if isText then MySQLText . T.decodeUtf8 <$> getLenEncBytes
                                                  else MySQLBytes <$> getLenEncBytes
    | t == mySQLTypeBit               = MySQLBit <$> (getBits =<< getLenEncInt)
    | otherwise                       = fail $ "Database.MySQL.Protocol.MySQLValue:\
                                               \ missing binary decoder for " ++ show t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63
    fracLexer bs = fst <$> LexFrac.readSigned LexFrac.readDecimal bs
    getYear :: Get Integer
    getYear = fromIntegral <$> getWord16le
    getInt8' :: Get Int
    getInt8' = fromIntegral <$> getWord8
    getSecond4 :: Get Pico
    getSecond4 = realToFrac <$> getWord8
    getSecond8 :: Get Pico
    getSecond8 =  do
        s <- getInt8'
        ms <- fromIntegral <$> getWord32le :: Get Int
        pure $! (realToFrac s + realToFrac ms / 1000000 :: Pico)


-- | Get a bit sequence as a Word64
--
-- Since 'Word64' has a @Bits@ instance, it's easier to deal with in haskell.
--
getBits :: Int -> Get Word64
getBits bytes =
    if  | bytes == 0 || bytes == 1 -> fromIntegral <$> getWord8
        | bytes == 2 -> fromIntegral <$> getWord16be
        | bytes == 3 -> fromIntegral <$> getWord24be
        | bytes == 4 -> fromIntegral <$> getWord32be
        | bytes == 5 -> getWord40be
        | bytes == 6 -> getWord48be
        | bytes == 7 -> getWord56be
        | bytes == 8 -> getWord64be
        | otherwise  -> fail $  "Database.MySQL.Protocol.MySQLValue: \
                                \wrong bit length size: " ++ show bytes
{-# INLINE getBits #-}


--------------------------------------------------------------------------------
-- | Binary protocol encoder
putBinaryField :: MySQLValue -> Put
putBinaryField (MySQLDecimal    n) = putLenEncBytes . L.toStrict . BB.toLazyByteString $
                                        formatScientificBuilder Fixed Nothing n
putBinaryField (MySQLInt8U      n) = putWord8 n
putBinaryField (MySQLInt8       n) = putWord8 (fromIntegral n)
putBinaryField (MySQLInt16U     n) = putWord16le n
putBinaryField (MySQLInt16      n) = putInt16le n
putBinaryField (MySQLInt32U     n) = putWord32le n
putBinaryField (MySQLInt32      n) = putInt32le n
putBinaryField (MySQLInt64U     n) = putWord64le n
putBinaryField (MySQLInt64      n) = putInt64le n
putBinaryField (MySQLFloat      x) = putFloatle x
putBinaryField (MySQLDouble     x) = putDoublele x
putBinaryField (MySQLYear       n) = putWord16le n
putBinaryField (MySQLTimeStamp (LocalTime date time)) = do putWord8 11    -- always put full
                                                           putBinaryDay date
                                                           putBinaryTime' time
putBinaryField (MySQLDateTime  (LocalTime date time)) = do putWord8 11    -- always put full
                                                           putBinaryDay date
                                                           putBinaryTime' time
putBinaryField (MySQLDate    d)    = do putWord8 4
                                        putBinaryDay d
putBinaryField (MySQLTime sign t)  = do putWord8 12    -- always put full
                                        putWord8 sign
                                        putBinaryTime t
putBinaryField (MySQLGeometry bs)  = putLenEncBytes bs
putBinaryField (MySQLBytes  bs)    = putLenEncBytes bs
putBinaryField (MySQLBit    word)  = putWord64le word
putBinaryField (MySQLText    t)    = putLenEncBytes (T.encodeUtf8 t)
putBinaryField MySQLNull           = return ()

putBinaryDay :: Day -> Put
putBinaryDay d = do let (yyyy, mm, dd) = toGregorian d
                    putWord16le (fromIntegral yyyy)
                    putWord8 (fromIntegral mm)
                    putWord8 (fromIntegral dd)
{-# INLINE putBinaryDay #-}

putBinaryTime' :: TimeOfDay -> Put
putBinaryTime' (TimeOfDay hh mm ss) = do let s = floor ss
                                             ms = floor $ (ss - realToFrac s) * 1000000
                                         putWord8 (fromIntegral hh)
                                         putWord8 (fromIntegral mm)
                                         putWord8 s
                                         putWord32le ms
{-# INLINE putBinaryTime' #-}

putBinaryTime :: TimeOfDay -> Put
putBinaryTime (TimeOfDay hh mm ss) = do let s = floor ss
                                            ms = floor $ (ss - realToFrac s) * 1000000
                                            (d, h) = hh `quotRem` 24  -- hour may exceed 24 here
                                        putWord32le (fromIntegral d)
                                        putWord8 (fromIntegral h)
                                        putWord8 (fromIntegral mm)
                                        putWord8 s
                                        putWord32le ms
{-# INLINE putBinaryTime #-}

--------------------------------------------------------------------------------
-- | Binary row decoder
--
-- MySQL use a special null bitmap without offset = 2 here.
--
getBinaryRow :: [ColumnDef] -> Int -> Get [MySQLValue]
getBinaryRow fields flen = do
    skipN 1           -- 0x00
    let maplen = (flen + 7 + 2) `shiftR` 3
    nullmap <- BitMap <$> getByteString maplen
    go fields nullmap 0
  where
    go :: [ColumnDef] -> BitMap -> Int -> Get [MySQLValue]
    go []     _       _   = pure []
    go (f:fs) nullmap pos = do
        r <- if isColumnNull nullmap pos
                then return MySQLNull
                else getBinaryField f
        let pos' = pos + 1
        rest <- pos' `seq` go fs nullmap pos'
        return (r `seq` (r : rest))
{-# INLINE getBinaryRow #-}

getBinaryRowVector :: V.Vector ColumnDef -> Int -> Get (V.Vector MySQLValue)
getBinaryRowVector fields flen = do
    skipN 1           -- 0x00
    let maplen = (flen + 7 + 2) `shiftR` 3
    nullmap <- BitMap <$> getByteString maplen
    (`V.imapM` fields) $ \ pos f ->
        if isColumnNull nullmap pos then return MySQLNull else getBinaryField f
{-# INLINE getBinaryRowVector #-}

--------------------------------------------------------------------------------
-- | Use 'ByteString' to present a bitmap.
--
-- When used for represent bits values, the underlining 'ByteString' follows:
--
--  * byteString: head       -> tail
--  * bit:        high bit   -> low bit
--
-- When used as a null-map/present-map, every bit inside a byte
-- is mapped to a column, the mapping order is following:
--
--  * byteString: head -> tail
--  * column:     left -> right
--
-- We don't use 'Int64' here because there maybe more than 64 columns.
--
newtype BitMap = BitMap { fromBitMap :: ByteString } deriving (Eq, Show)

-- | Test if a column is set(binlog protocol).
--
-- The number counts from left to right.
--
isColumnSet :: BitMap -> Int -> Bool
isColumnSet (BitMap bitmap) pos =
  let i = pos `unsafeShiftR` 3
      j = pos .&. 7
  in (bitmap `B.unsafeIndex` i) `testBit` j
{-# INLINE isColumnSet #-}

-- | Test if a column is null(binary protocol).
--
-- The number counts from left to right.
--
isColumnNull :: BitMap -> Int -> Bool
isColumnNull (BitMap nullmap) pos =
  let
    pos' = pos + 2
    i    = pos' `unsafeShiftR` 3
    j    = pos' .&. 7
  in (nullmap `B.unsafeIndex` i) `testBit` j
{-# INLINE isColumnNull #-}

-- | Make a nullmap for params(binary protocol) without offset.
--
makeNullMap :: [MySQLValue] -> BitMap
makeNullMap values = BitMap . B.pack $ go values 0x00 0
  where
    go :: [MySQLValue] -> Word8 -> Int -> [Word8]
    go []             byte   8  = [byte]
    go vs             byte   8  = byte : go vs 0x00 0
    go []             byte   _  = [byte]
    go (MySQLNull:vs) byte pos  = let pos' = pos + 1
                                      byte' = byte .|. bit pos
                                  in pos' `seq` byte' `seq` go vs byte' pos'
    go (_        :vs) byte pos  = let pos' = pos + 1 in pos' `seq` go vs byte pos'

--------------------------------------------------------------------------------
-- TODO: add helpers to parse mySQLTypeGEOMETRY
-- reference: https://github.com/felixge/node-mysql/blob/master/lib/protocol/Parser.js
