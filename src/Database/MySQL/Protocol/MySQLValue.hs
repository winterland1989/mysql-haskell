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
  , RawDate(..)
  , putParamMySQLType
  , getTextField
  , putTextField
  , getTextRow
  , getTextRowVector
    -- * Text rows with the column types resolved once per result set
  , ColumnKind(..)
  , ValueKind(..)
  , columnKind
  , decodeTextRow
  , decodeTextRowVector
  , ColumnNumber(..)
  , nextColumn
  , RowError(..)
  , RowErrorKind(..)
  , TextFieldError(..)
  , InvalidUtf8Text(..)
  , describeRowError
  , getBinaryField
  , putBinaryField
  , getBinaryRow
  , getBinaryRowVector
  , decodeMySQLLatin1
  -- * Internal utilities
  , decodeTextValue
  , decodeTextBit
  , lengthEncodedInt
  , lexSignedIntegral
  , lexSignedFraction
  , textDateParts
  , lexDateTimeParts
  , calendarDate
  , dateValue
  , dateTimeValue
  , timeStampValue
  , lexSignedTime
  , getBits
  , BitMap(..)
  , isColumnSet
  , isColumnNull
  , makeNullMap
  ) where

import qualified Blaze.Text                         as Textual
import           Control.Exception                  (Exception (displayException), throw)
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
import           Data.Time.Calendar                 (Day (ModifiedJulianDay),
                                                     fromGregorianValid,
                                                     toGregorian)
import           Data.Time.Format                   (defaultTimeLocale,
                                                     formatTime)
import           Data.Time.LocalTime                (LocalTime (..),
                                                     TimeOfDay (..))
import           Data.Word
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Escape
import           Database.MySQL.Protocol.FloatingPoint (readDouble, readFloat)
import           Database.MySQL.Protocol.Packet
import           GHC.Generics                       (Generic)
import           Text.Printf                        (printf)
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Unwitch.Convert.Int                as Int
import qualified Unwitch.Convert.Word16             as Word16
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
-- A @DATE@, @DATETIME@ or @TIMESTAMP@ whose date a 'Day' cannot hold, such as the zero
-- date @0000-00-00@, arrives as 'MySQLDateRaw', 'MySQLDateTimeRaw' or 'MySQLTimeStampRaw'
-- and is sent back unchanged, see 'RawDate'.
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
    | MySQLDateRaw       !RawDate              -- ^ DATE that a 'Day' cannot hold
    | MySQLDateTimeRaw   !RawDate !TimeOfDay   -- ^ DATETIME whose date a 'Day' cannot hold
    | MySQLTimeStampRaw  !RawDate !TimeOfDay   -- ^ TIMESTAMP whose date a 'Day' cannot hold
    | MySQLTime          !Word8 !TimeOfDay -- ^ sign(0 = non-negative, 1 = negative) hh mm ss microsecond
                                           -- The sign is OPPOSITE to binlog one !!!
    | MySQLGeometry      !ByteString       -- ^ todo: parsing to something meanful
    | MySQLBytes         !ByteString
    | MySQLBit           !Word64
    | MySQLText          !Text
    | MySQLNull
  deriving (Show, Eq, Generic)

-- | A date as the server stores it, when a 'Day' cannot hold it: the zero date
-- @0000-00-00@, a date with a zero month or day such as @2024-05-00@ (unless
-- @NO_ZERO_DATE@ and @NO_ZERO_IN_DATE@ are set), or under @ALLOW_INVALID_DATES@
-- a day past the end of its month such as @2024-02-30@.
data RawDate = RawDate
    { rawDateYear  :: !Word16
    , rawDateMonth :: !Word8
    , rawDateDay   :: !Word8
    } deriving (Show, Eq, Generic)

-- | 'Right' when a 'Day' can hold the date.
calendarDate :: RawDate -> Either RawDate Day
calendarDate raw@(RawDate year month day) =
    -- Every month has 28 days, so these dates exist in every year and need no
    -- check; only the rest go through 'fromGregorianValid', whose Integer
    -- arithmetic was most of a date row's cost (docs/direct-row-decoding.md).
    if month >= 1 && month <= 12 && day >= 1 && day <= 28
    then Right (dayOfCommonDate (Word16.toInt year) (Word8.toInt month) (Word8.toInt day))
    else maybe (Left raw) Right
        (fromGregorianValid (Word16.toInteger year) (Word8.toInt month) (Word8.toInt day))

-- | The 'Day' of a date that exists, computed in 'Int': Howard Hinnant's
-- days_from_civil (http://howardhinnant.github.io/date_algorithms.html),
-- counted from the Modified Julian Day epoch, 1858-11-17. The year counts from
-- March, so that February's leap day falls at its end.
dayOfCommonDate :: Int -> Int -> Int -> Day
dayOfCommonDate year month day =
    let marchYear = if month <= 2 then year - 1 else year
        era = marchYear `div` 400
        yearOfEra = marchYear - era * 400
        dayOfEra = yearOfEra * 365 + yearOfEra `div` 4 - yearOfEra `div` 100 + marchDayOfYear month day
    in ModifiedJulianDay (Int.toInteger (era * 146097 + dayOfEra - 678881))

-- | The day's number in a year that starts on 1 March, from 0.
marchDayOfYear :: Int -> Int -> Int
marchDayOfYear month day = (153 * (if month > 2 then month - 3 else month + 9) + 2) `div` 5 + day - 1

dateValue :: RawDate -> MySQLValue
dateValue = either MySQLDateRaw MySQLDate . calendarDate

dateTimeValue :: RawDate -> TimeOfDay -> MySQLValue
dateTimeValue raw time =
    either (`MySQLDateTimeRaw` time) (\day -> MySQLDateTime (LocalTime day time)) (calendarDate raw)

timeStampValue :: RawDate -> TimeOfDay -> MySQLValue
timeStampValue raw time =
    either (`MySQLTimeStampRaw` time) (\day -> MySQLTimeStamp (LocalTime day time)) (calendarDate raw)

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
putParamMySQLType (MySQLDateRaw      _)  = putFieldType mySQLTypeDate     >> putWord8 0x00
putParamMySQLType (MySQLDateTimeRaw _ _) = putFieldType mySQLTypeDateTime >> putWord8 0x00
putParamMySQLType (MySQLTimeStampRaw _ _) = putFieldType mySQLTypeTimestamp >> putWord8 0x00
putParamMySQLType (MySQLTime       _ _)  = putFieldType mySQLTypeTime     >> putWord8 0x00
putParamMySQLType (MySQLBytes        _)  = putFieldType mySQLTypeBlob     >> putWord8 0x00
putParamMySQLType (MySQLGeometry     _)  = putFieldType mySQLTypeGeometry >> putWord8 0x00
putParamMySQLType (MySQLBit          _)  = putFieldType mySQLTypeLongLong >> putWord8 0x80
putParamMySQLType (MySQLText         _)  = putFieldType mySQLTypeString   >> putWord8 0x00
putParamMySQLType MySQLNull              = putFieldType mySQLTypeNull     >> putWord8 0x00

--------------------------------------------------------------------------------
-- | Text protocol decoder
getTextField :: ColumnDef -> Get MySQLValue
getTextField f = case columnKind f of
    NullColumn -> pure MySQLNull
    ValueColumn fieldType value -> do
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
'ColumnKind' once, when the column definitions arrive, and decode every row
with 'decodeTextRow': a walk over the row as one strict 'ByteString' that
returns unboxed sums, so no 'Either' or tuple is allocated per field.

Decision: values stay as lazy as 'getTextField' leaves them. Evaluating every
value while decoding saved 3% of the instructions when a caller uses every
value, and cost 2.75 times the instructions when it uses none
(docs/direct-row-decoding.md). A string is decoded as UTF-8 only when its
value is used, as in 1.3: bytes that are not UTF-8 raise 'InvalidUtf8Text'
from pure code then. 1.4.0 checked every string while reading the row
instead, which cost a validation pass on text nobody looked at and a second
pass on text that was used; callers who want the check up front use the
typed decoders of "Database.MySQL.Decoder", which check as they read.

'getTextField' and 'getTextRow' keep their 'Get' interface and share
'columnKind' and 'decodeTextValue', so both paths turn the same bytes into the
same values. They differ only on rows a server does not send: an 8-byte length
above 'maxBound' of 'Int', which 'decodeTextRow' rejects, and a zero-length
BIT field, which is now 0 instead of the byte after it.
-}

-- | How a column's fields decode, in either protocol, resolved once per result
-- set by 'columnKind'. See Note [Text rows decoded once per result set].
--
-- @since 1.4.0
data ColumnKind
    = NullColumn
      -- ^ a column of type NULL: every field of it is NULL, so no bytes of a
      -- text-protocol row belong to it beyond the NULL marker
    | ValueColumn !FieldType !ValueKind
      -- ^ a column with values; the column type is kept for error messages
    deriving (Show, Eq)

-- | The 'MySQLValue' constructor a column's non-NULL fields become.
--
-- @since 1.4.0
data ValueKind
    = KindDecimal
    | KindInt8U
    | KindInt8
    | KindInt16U
    | KindInt16
    | KindInt32U
    | KindInt32
    | KindInt64U
    | KindInt64
    | KindFloat
    | KindDouble
    | KindYear
    | KindTimeStamp
    | KindDateTime
    | KindDate
    | KindTime
    | KindGeometry
    | KindText !Word16
      -- ^ a string column in a character set other than binary or latin1, read
      -- as UTF-8; the character set id is kept for error messages
    | KindLatin1Text   -- ^ a string column in latin1, which MySQL means as Windows-1252
    | KindBytes        -- ^ a string column in the binary character set
    | KindBit
    | KindUnsupported  -- ^ a column type without a decoder
    deriving (Show, Eq)

-- | How a column's fields decode.
--
-- @since 1.4.0
columnKind :: ColumnDef -> ColumnKind
columnKind f =
    if  | t == mySQLTypeNull -> NullColumn
        | t == mySQLTypeDecimal || t == mySQLTypeNewDecimal -> ValueColumn t KindDecimal
        | t == mySQLTypeTiny ->
            ValueColumn t (if isUnsigned then KindInt8U else KindInt8)
        | t == mySQLTypeShort ->
            ValueColumn t (if isUnsigned then KindInt16U else KindInt16)
        | t == mySQLTypeLong || t == mySQLTypeInt24 ->
            ValueColumn t (if isUnsigned then KindInt32U else KindInt32)
        | t == mySQLTypeLongLong ->
            ValueColumn t (if isUnsigned then KindInt64U else KindInt64)
        | t == mySQLTypeFloat -> ValueColumn t KindFloat
        | t == mySQLTypeDouble -> ValueColumn t KindDouble
        | t == mySQLTypeYear -> ValueColumn t KindYear
        | t == mySQLTypeTimestamp || t == mySQLTypeTimestamp2 -> ValueColumn t KindTimeStamp
        | t == mySQLTypeDateTime || t == mySQLTypeDateTime2 -> ValueColumn t KindDateTime
        | t == mySQLTypeDate || t == mySQLTypeNewDate -> ValueColumn t KindDate
        | t == mySQLTypeTime || t == mySQLTypeTime2 -> ValueColumn t KindTime
        | t == mySQLTypeGeometry -> ValueColumn t KindGeometry
        | t == mySQLTypeVarChar
            || t == mySQLTypeEnum
            || t == mySQLTypeSet
            || t == mySQLTypeTinyBlob
            || t == mySQLTypeMediumBlob
            || t == mySQLTypeLongBlob
            || t == mySQLTypeBlob
            || t == mySQLTypeVarString
            || t == mySQLTypeString ->
            ValueColumn t (stringKind (columnCharSet f))
        | t == mySQLTypeBit -> ValueColumn t KindBit
        | otherwise -> ValueColumn t KindUnsupported
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)

-- | Why the bytes of a text-protocol field did not decode.
--
-- @since 1.4.0
data TextFieldError
    = TextFieldUnparsable !FieldType !ByteString
      -- ^ the bytes are not a value of the column's type
    | TextFieldBitTooWide !Int
      -- ^ a BIT field longer than the 8 bytes of a 'Word64'
    | TextFieldUnsupportedType !FieldType
      -- ^ a column type without a text decoder
    | TextFieldInvalidUtf8 !Word16 !ByteString
      -- ^ a string in this character set id whose bytes are not UTF-8. Not
      -- raised since 1.4.1, where such a string raises 'InvalidUtf8Text' when
      -- its value is used; kept so that matches on it still compile.
    deriving (Show, Eq)

-- | A string value, in the character set with this id, whose bytes are not
-- UTF-8. Raised when the 'MySQLText' is evaluated, not while its row is read.
--
-- @since 1.4.1
data InvalidUtf8Text = InvalidUtf8Text !Word16 !ByteString
    deriving (Show, Eq)

instance Exception InvalidUtf8Text where
    displayException (InvalidUtf8Text charsetId _) =
        "Database.MySQL.Protocol.MySQLValue: a string value arrived in character set id "
            ++ show charsetId ++ ", which mysql-haskell can only decode as UTF-8 or latin1,"
            ++ " and its bytes are not valid UTF-8. Use a UTF-8 connection charset such as"
            ++ " defaultConnectInfoMB4's, or select the column as BINARY to get its bytes."

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
    TextFieldInvalidUtf8 charsetId _ ->
        "Database.MySQL.Protocol.MySQLValue: a string column arrived in character set id "
            ++ show charsetId ++ ", which mysql-haskell can only decode as UTF-8 or latin1,"
            ++ " and its bytes are not valid UTF-8. Use a UTF-8 connection charset such as"
            ++ " defaultConnectInfoMB4's, or select the column as BINARY to get its bytes."

-- | The value of a field's bytes. The lexer runs at once, so a malformed field
-- fails while the row is read; a lexed value and the 'Text' of a string are left lazy,
-- while a constructor around bytes already in hand is built at once, as that
-- is cheaper than a thunk. See Note [Text rows decoded once per result set].
-- Inlined so that 'decodeTextFieldAt' matches the 'Right' away instead of
-- allocating it.
decodeTextValue :: FieldType -> ValueKind -> ByteString -> Either TextFieldError MySQLValue
decodeTextValue fieldType value bytes = case value of
    KindDecimal     -> lexedValue fieldType MySQLDecimal lexSignedFraction bytes
    KindInt8U       -> lexedValue fieldType MySQLInt8U lexSignedIntegral bytes
    KindInt8        -> lexedValue fieldType MySQLInt8 lexSignedIntegral bytes
    KindInt16U      -> lexedValue fieldType MySQLInt16U lexSignedIntegral bytes
    KindInt16       -> lexedValue fieldType MySQLInt16 lexSignedIntegral bytes
    KindInt32U      -> lexedValue fieldType MySQLInt32U lexSignedIntegral bytes
    KindInt32       -> lexedValue fieldType MySQLInt32 lexSignedIntegral bytes
    KindInt64U      -> lexedValue fieldType MySQLInt64U lexSignedIntegral bytes
    KindInt64       -> lexedValue fieldType MySQLInt64 lexSignedIntegral bytes
    KindFloat       -> lexedValue fieldType MySQLFloat readFloat bytes
    KindDouble      -> lexedValue fieldType MySQLDouble readDouble bytes
    KindYear        -> lexedValue fieldType MySQLYear lexSignedIntegral bytes
    KindTimeStamp   -> lexedValue fieldType (uncurry timeStampValue) lexDateTimeParts bytes
    KindDateTime    -> lexedValue fieldType (uncurry dateTimeValue) lexDateTimeParts bytes
    KindDate        -> lexedValue fieldType dateValue textDateParts bytes
    KindTime        -> lexedValue fieldType id lexTime bytes
    KindGeometry    -> Right $! MySQLGeometry bytes
    KindText charsetId -> utf8Value charsetId bytes
    KindLatin1Text  -> Right (MySQLText (decodeMySQLLatin1 bytes))
    KindBytes       -> Right $! MySQLBytes bytes
    KindBit         -> decodeTextBit bytes
    KindUnsupported -> Left (TextFieldUnsupportedType fieldType)
{-# INLINE decodeTextValue #-}

-- | Always 'Right': the 'MySQLText' is decoded, in one UTF-8 pass, when it is
-- used. See Note [Text rows decoded once per result set].
utf8Value :: Word16 -> ByteString -> Either TextFieldError MySQLValue
utf8Value charsetId bytes = Right (lazyUtf8Text charsetId bytes)
{-# INLINE utf8Value #-}

lazyUtf8Text :: Word16 -> ByteString -> MySQLValue
lazyUtf8Text charsetId bytes = case T.decodeUtf8' bytes of
    Right decoded -> MySQLText decoded
    Left _        -> throw (InvalidUtf8Text charsetId bytes)

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
-- Specialised here so that "Database.MySQL.Decoder", which reaches these
-- lexers through higher-order arguments, gets the same specialised digit loops
-- as 'decodeTextValue' instead of the dictionary-passing ones.
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Int8 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Word8 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Int16 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Word16 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Int32 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Word32 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Int64 #-}
{-# SPECIALIZE lexSignedIntegral :: ByteString -> Maybe Word64 #-}

lexSignedFraction :: Fractional a => ByteString -> Maybe a
lexSignedFraction bytes = fst <$> LexFrac.readSigned LexFrac.readDecimal bytes
{-# SPECIALIZE lexSignedFraction :: ByteString -> Maybe Scientific #-}


-- | @YYYY-MM-DD hh:mm:ss[.fraction]@, as DATETIME and TIMESTAMP fields come;
-- the date may be one a 'Day' cannot hold.
lexDateTimeParts :: ByteString -> Maybe (RawDate, TimeOfDay)
lexDateTimeParts bytes = do
    guard (B.length bytes >= 12)
    (,) <$> textDateParts bytes <*> lexTimeOfDay (B.drop 11 bytes)

lexTime :: ByteString -> Maybe MySQLValue
lexTime bytes = uncurry MySQLTime <$> lexSignedTime bytes

-- | A TIME field: an optional minus sign before @hh:mm:ss[.fraction]@, where the
-- hours may exceed 24. The sign is 1 for negative, as in 'MySQLTime'.
lexSignedTime :: ByteString -> Maybe (Word8, TimeOfDay)
lexSignedTime bytes = do
    guard (not (B.null bytes))
    if B.index bytes 0 == 45  -- '-'
    then (,) 1 <$> lexTimeOfDay (B.drop 1 bytes)
    else (,) 0 <$> lexTimeOfDay bytes

-- | @hh:mm:ss[.fraction]@.
lexTimeOfDay :: ByteString -> Maybe TimeOfDay
lexTimeOfDay bytes = do
    (hh, rest) <- LexInt.readDecimal bytes
    guard (not (B.null rest))
    (mm, rest') <- LexInt.readDecimal (B.tail rest)
    guard (not (B.null rest'))
    (ss, _) <- LexFrac.readDecimal (B.tail rest')
    return (TimeOfDay hh mm ss)

-- | A column's position in a result set, counting from 0.
--
-- @since 1.4.0
newtype ColumnNumber = ColumnNumber Int
    deriving (Show, Eq, Ord)

-- | @since 1.4.0
nextColumn :: ColumnNumber -> ColumnNumber
nextColumn (ColumnNumber column) = ColumnNumber (column + 1)

-- | Where and why a text-protocol row did not decode.
--
-- @since 1.4.0
data RowError = RowError
    { rowErrorColumn :: !ColumnNumber
    , rowErrorOffset :: !Int               -- ^ byte offset of that column's field in the row
    , rowErrorKind   :: !RowErrorKind
    } deriving (Show, Eq)

-- | @since 1.4.0
data RowErrorKind
    = RowEndsEarly
      -- ^ the row ends before this column's field does
    | RowInvalidLengthPrefix !Word8
      -- ^ a field starting with a byte that is neither a length nor the NULL marker
    | RowLengthOverflow
      -- ^ an 8-byte length above 'maxBound' of 'Int'
    | RowFieldError !TextFieldError
      -- ^ the field's bytes did not decode
    deriving (Show, Eq)

-- | @since 1.4.0
describeRowError :: RowError -> String
describeRowError (RowError (ColumnNumber column) offset kind) =
    "Database.MySQL.Protocol.MySQLValue: column " ++ show column ++ " at byte "
        ++ show offset ++ ": " ++ kindDescription
  where
    kindDescription = case kind of
        RowEndsEarly                  -> "the row ends inside this field"
        RowInvalidLengthPrefix prefix -> "invalid length prefix " ++ show prefix
        RowLengthOverflow             -> "length does not fit an Int"
        RowFieldError fieldError      -> describeTextFieldError fieldError

-- | The values of a text-protocol row, one per 'ColumnKind'. Bytes after the
-- last column are ignored, as 'getTextRow' ignores them.
-- See Note [Text rows decoded once per result set].
--
-- @since 1.4.0
decodeTextRow :: [ColumnKind] -> ByteString -> Either RowError [MySQLValue]
decodeTextRow columns row = case decodeTextFields row (ColumnNumber 0) 0 columns of
    (# rowError | #) -> Left rowError
    (# | values #)   -> Right values

-- | 'V.Vector' version of 'decodeTextRow'.
--
-- @since 1.4.0
decodeTextRowVector :: V.Vector ColumnKind -> ByteString -> Either RowError (V.Vector MySQLValue)
decodeTextRowVector columns row =
    V.fromListN (V.length columns) <$> decodeTextRow (V.toList columns) row

-- | The fields of @columns@, the first of which is @column@ and starts at byte
-- @offset@ of the row.
decodeTextFields :: ByteString -> ColumnNumber -> Int -> [ColumnKind] -> (# RowError | [MySQLValue] #)
decodeTextFields row columnNumber offset columns = case columns of
    [] -> (# | [] #)
    column : laterColumns -> case decodeTextFieldAt row columnNumber offset column of
        (# rowError | #) -> (# rowError | #)
        (# | (# value, nextOffset #) #) ->
            case decodeTextFields row (nextColumn columnNumber) nextOffset laterColumns of
                (# rowError | #) -> (# rowError | #)
                (# | values #)   -> (# | value : values #)

-- | The field at byte @offset@ and the offset after it. The NULL marker is
-- checked before the column, as 'getTextRow' does.
decodeTextFieldAt :: ByteString -> ColumnNumber -> Int -> ColumnKind
                  -> (# RowError | (# MySQLValue, Int #) #)
decodeTextFieldAt row columnNumber offset column =
    if  | offset >= B.length row ->
            (# RowError columnNumber offset RowEndsEarly | #)
        | B.unsafeIndex row offset == 0xFB -> (# | (# MySQLNull, offset + 1 #) #)
        | otherwise -> case column of
            NullColumn -> (# | (# MySQLNull, offset #) #)
            ValueColumn fieldType value -> case lengthEncodedInt row offset of
                (# kind | #) -> (# RowError columnNumber offset kind | #)
                (# | (# fieldLength, fieldStart #) #) ->
                    if fieldLength > B.length row - fieldStart
                    then (# RowError columnNumber offset RowEndsEarly | #)
                    else case decodeTextValue fieldType value
                                (B.unsafeTake fieldLength (B.unsafeDrop fieldStart row)) of
                        Left fieldError ->
                            (# RowError columnNumber offset (RowFieldError fieldError) | #)
                        Right decoded -> (# | (# decoded, fieldStart + fieldLength #) #)

-- | The length-encoded integer at byte @offset@, which must be inside the row,
-- and the offset after it.
lengthEncodedInt :: ByteString -> Int -> (# RowErrorKind | (# Int, Int #) #)
lengthEncodedInt row offset =
    let prefix = B.unsafeIndex row offset
    in if  | prefix < 0xFB  -> (# | (# Word8.toInt prefix, offset + 1 #) #)
           | prefix == 0xFC -> littleEndianLength row (offset + 1) 2
           | prefix == 0xFD -> littleEndianLength row (offset + 1) 3
           | prefix == 0xFE -> littleEndianLength row (offset + 1) 8
           | otherwise      -> (# RowInvalidLengthPrefix prefix | #)

-- | The little-endian length of @width@ bytes at byte @start@, and the offset after it.
littleEndianLength :: ByteString -> Int -> Int -> (# RowErrorKind | (# Int, Int #) #)
littleEndianLength row start width =
    if width > B.length row - start
    then (# RowEndsEarly | #)
    else case Word64.toInt (B.foldr' prependLittleEndianByte 0
                                (B.unsafeTake width (B.unsafeDrop start row))) of
        Nothing          -> (# RowLengthOverflow | #)
        Just fieldLength -> (# | (# fieldLength, start + width #) #)

prependLittleEndianByte :: Word8 -> Word64 -> Word64
prependLittleEndianByte byte higherBytes = unsafeShiftL higherBytes 8 .|. Word8.toWord64 byte

-- | The @YYYY-MM-DD@ that starts a text-protocol date, which may be zero.
textDateParts :: ByteString -> Maybe RawDate
textDateParts bs = do
    (yyyy, rest) <- LexInt.readDecimal bs :: Maybe (Int, ByteString)
    guard (not (B.null rest))
    (mm, rest') <- LexInt.readDecimal (B.tail rest) :: Maybe (Int, ByteString)
    guard (not (B.null rest'))
    (dd, _) <- LexInt.readDecimal (B.tail rest') :: Maybe (Int, ByteString)
    RawDate <$> Int.toWord16 yyyy <*> Int.toWord8 mm <*> Int.toWord8 dd

-- | How the bytes of a string column decode, by the character set id the
-- server sent them in. See Note [Result character sets].
stringKind :: Word16 -> ValueKind
stringKind charsetId =
    if  | charsetId == 63 -> KindBytes
        | charsetId `elem` latin1Collations -> KindLatin1Text
        | otherwise -> KindText charsetId

-- | The ids of @information_schema.collations@ whose character set is latin1,
-- in MySQL 8.0 and MariaDB 11.4 (which adds the two nopad ones).
latin1Collations :: [Word16]
latin1Collations = [5, 8, 15, 31, 47, 48, 49, 94, 1032, 1071]

-- Note [Result character sets]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The server converts string columns to character_set_results, so with a
-- UTF-8 connection charset every text column arrives as UTF-8. A connection
-- set to another charset, as after @SET NAMES latin1@, gets that charset's
-- bytes. latin1 is decoded here; any other charset is read as UTF-8, which
-- keeps ASCII-only values working as they always did. Bytes that are not
-- UTF-8 raise an 'InvalidUtf8Text' naming the charset when the value is used,
-- rather than return mangled text.

stringColumnValue :: ColumnDef -> ByteString -> Get MySQLValue
stringColumnValue column bytes =
    case decodeTextValue (columnType column) (stringKind (columnCharSet column)) bytes of
        Right value -> pure value
        Left fieldError ->
            fail (describeTextFieldError fieldError ++ " Column: " ++ show (columnName column))

-- | MySQL's latin1, which is Windows-1252 with its five unassigned bytes
-- (0x81, 0x8D, 0x8F, 0x90, 0x9D) mapped to the C1 controls of the same number.
-- 'T.decodeLatin1' alone is ISO-8859-1, which reads all of 0x80 to 0x9F as
-- C1 controls, so the 27 bytes Windows-1252 assigns are remapped after it.
decodeMySQLLatin1 :: ByteString -> Text
decodeMySQLLatin1 = T.map windows1252Char . T.decodeLatin1

windows1252Char :: Char -> Char
windows1252Char char = case char of
    '\x80' -> '\x20AC'
    '\x82' -> '\x201A'
    '\x83' -> '\x0192'
    '\x84' -> '\x201E'
    '\x85' -> '\x2026'
    '\x86' -> '\x2020'
    '\x87' -> '\x2021'
    '\x88' -> '\x02C6'
    '\x89' -> '\x2030'
    '\x8A' -> '\x0160'
    '\x8B' -> '\x2039'
    '\x8C' -> '\x0152'
    '\x8E' -> '\x017D'
    '\x91' -> '\x2018'
    '\x92' -> '\x2019'
    '\x93' -> '\x201C'
    '\x94' -> '\x201D'
    '\x95' -> '\x2022'
    '\x96' -> '\x2013'
    '\x97' -> '\x2014'
    '\x98' -> '\x02DC'
    '\x99' -> '\x2122'
    '\x9A' -> '\x0161'
    '\x9B' -> '\x203A'
    '\x9C' -> '\x0153'
    '\x9E' -> '\x017E'
    '\x9F' -> '\x0178'
    sameCodePoint -> sameCodePoint

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
putTextField (MySQLDateRaw    d) = putInQuotes $ putByteString (BC.pack (rawDateText d))
putTextField (MySQLDateTimeRaw d t) = putInQuotes $
                                      putByteString (BC.pack (rawDateText d ++ formatTime defaultTimeLocale " %T%Q" t))
putTextField (MySQLTimeStampRaw d t) = putInQuotes $
                                      putByteString (BC.pack (rawDateText d ++ formatTime defaultTimeLocale " %T%Q" t))
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

rawDateText :: RawDate -> String
rawDateText (RawDate year month day) = printf "%04d-%02d-%02d" year month day

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
               0 -> pure $ timeStampValue (RawDate 0 0 0) (TimeOfDay 0 0 0)
               4 -> do
                   d <- getBinaryDateParts
                   pure $ timeStampValue d (TimeOfDay 0 0 0)
               7 -> do
                   d <- getBinaryDateParts
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4
                   pure $ timeStampValue d td
               11 -> do
                   d <- getBinaryDateParts
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8
                   pure $ timeStampValue d td
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong TIMESTAMP length"
    | t == mySQLTypeDateTime
        || t == mySQLTypeDateTime2    = do
            n <- getLenEncInt
            case n of
               0 -> pure $ dateTimeValue (RawDate 0 0 0) (TimeOfDay 0 0 0)
               4 -> do
                   d <- getBinaryDateParts
                   pure $ dateTimeValue d (TimeOfDay 0 0 0)
               7 -> do
                   d <- getBinaryDateParts
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4
                   pure $ dateTimeValue d td
               11 -> do
                   d <- getBinaryDateParts
                   td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8
                   pure $ dateTimeValue d td
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong DATETIME length"

    | t == mySQLTypeDate
        || t == mySQLTypeNewDate      = do
            n <- getLenEncInt
            case n of
               0 -> pure $ dateValue (RawDate 0 0 0)
               4 -> dateValue <$> getBinaryDateParts
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
        || t == mySQLTypeString       = getLenEncBytes >>= stringColumnValue f
    | t == mySQLTypeBit               = MySQLBit <$> (getBits =<< getLenEncInt)
    | otherwise                       = fail $ "Database.MySQL.Protocol.MySQLValue:\
                                               \ missing binary decoder for " ++ show t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    fracLexer bs = fst <$> LexFrac.readSigned LexFrac.readDecimal bs
    getInt8' :: Get Int
    getInt8' = fromIntegral <$> getWord8
    getSecond4 :: Get Pico
    getSecond4 = realToFrac <$> getWord8
    getSecond8 :: Get Pico
    getSecond8 =  do
        s <- getInt8'
        ms <- fromIntegral <$> getWord32le :: Get Int
        pure $! (realToFrac s + realToFrac ms / 1000000 :: Pico)


getBinaryDateParts :: Get RawDate
getBinaryDateParts = RawDate <$> getWord16le <*> getWord8 <*> getWord8

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
putBinaryField (MySQLDateRaw d)    = do putWord8 4
                                        putRawDate d
putBinaryField (MySQLDateTimeRaw d time) = do putWord8 11
                                              putRawDate d
                                              putBinaryTime' time
putBinaryField (MySQLTimeStampRaw d time) = do putWord8 11
                                               putRawDate d
                                               putBinaryTime' time
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

putRawDate :: RawDate -> Put
putRawDate (RawDate year month day) = putWord16le year >> putWord8 month >> putWord8 day

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
