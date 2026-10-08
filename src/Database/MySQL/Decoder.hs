{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedSums   #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Database.MySQL.Decoder
Description : Decode rows straight into Haskell values, without 'MySQLValue'
Copyright   : (c) Jappie Klooster, 2026
License     : BSD3
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

Decoders that turn a row's fields straight into Haskell values, for
'Database.MySQL.Base.queryRows' and 'Database.MySQL.Base.queryStmtRows':

@
import qualified Database.MySQL.Decoder as Decode

data Employee = Employee { number :: Int32, born :: Day, name :: Text }

employee :: Decode.RowDecoder Employee
employee = Employee
    \<$\> Decode.field Decode.int32
    \<*\> Decode.field Decode.day
    \<*\> Decode.field Decode.text

(columns, employees) <- queryRows_ employee conn "SELECT emp_no, birth_date, first_name FROM employees"
@

A 'FieldDecoder' is checked against its column's definition once per result
set; a column it does not fit raises a 'ColumnMismatch' before any row is
read. A decoder for a Haskell type accepts exactly the columns whose
'MySQLValue' constructor "Database.MySQL.Field" decodes into that type, so
the two never disagree. Values are evaluated as they are decoded, so a field
that does not decode raises a 'FieldError' with its column while the row is
read.

Libraries that decode by column number, such as a persistent backend, can use
'prepareFieldParser' and 'runFieldParser' on the 'RawRow's of
'Database.MySQL.Base.queryRawRows_' and 'Database.MySQL.Base.queryStmtRawRows'
instead of a 'RowDecoder'. See @docs/direct-row-decoding.md@.
-}
module Database.MySQL.Decoder
  ( -- * Decoding a column
    FieldDecoder
  , int8, word8, int16, word16, int32, word32, int64, word64
  , float, double, scientific
  , text, bytes
  , day, localTime, timeOfDay
  , bool
  , mysqlValue
  , nullable
    -- * Decoding a row
  , RowDecoder
  , field
    -- * Errors
  , ColumnMismatch(..)
  , FieldError(..)
  , FieldErrorKind(..)
    -- * Running decoders on raw rows
  , RowProtocol
  , FieldParser
  , prepareFieldParser
  , runFieldParser
  , PreparedRow
  , prepareRowDecoder
  , runPreparedRow
  , module Database.MySQL.Protocol.RawRow
  ) where

import           Control.Exception                  (Exception)
import           Data.Bits                          (unsafeShiftL, (.|.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Unsafe             as B
import           Data.Kind                          (Type)
import           Data.Fixed                         (Fixed (MkFixed), Pico)
import           Data.Int                           (Int16, Int32, Int64, Int8)
import           Data.Scientific                    (Scientific)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           Data.Time.Calendar                 (Day, fromGregorian)
import           Data.Time.LocalTime                (LocalTime (..),
                                                     TimeOfDay (..), midnight)
import qualified Data.Vector                        as V
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue (ColumnKind (..),
                                                     MySQLValue (..),
                                                     RowErrorKind (..),
                                                     TextFieldError (..),
                                                     ValueKind (..),
                                                     columnKind, decodeTextBit,
                                                     decodeTextValue,
                                                     lexDate, lexLocalTime,
                                                     lexSignedFraction,
                                                     lexSignedIntegral,
                                                     lengthEncodedInt,
                                                     lexSignedTime)
import           Database.MySQL.Protocol.RawRow      hiding (binaryNullMapLength,
                                                             isNullInMap)
import qualified Database.MySQL.Protocol.RawRow      as RawRow
import           GHC.Exts                           (word16ToInt16#,
                                                     word32ToInt32#,
                                                     word64ToInt64#,
                                                     word8ToInt8#)
import           GHC.Float                          (castWord32ToFloat,
                                                     castWord64ToDouble)
import           GHC.Int                            (Int16 (I16#), Int32 (I32#),
                                                     Int64 (I64#), Int8 (I8#))
import           GHC.Word                           (Word16 (W16#),
                                                     Word32 (W32#),
                                                     Word64 (W64#), Word8 (W8#))
import qualified Unwitch.Convert.Word16             as Word16
import qualified Unwitch.Convert.Word32             as Word32
import qualified Unwitch.Convert.Word8              as Word8

-- | How the non-NULL bytes of one field become a value. An unboxed sum, so
-- that no 'Either' is allocated per field.
type ParseBytes a = ByteString -> (# FieldErrorKind | a #)

-- | Turns the fields of one column into @a@. See the module header.
--
-- @since 1.3.4
data FieldDecoder a = FieldDecoder
    { decoderTypeName :: !Text
      -- ^ the Haskell type, for 'ColumnTypeMismatch'
    , decoderNull     :: !(Maybe a)
      -- ^ the value of a NULL field; 'Nothing' makes a NULL a 'FieldUnexpectedNull'
    , decoderText     :: ColumnKind -> Maybe (ParseBytes a)
      -- ^ the parser for a column's text-protocol fields, if the column fits
    , decoderBinary   :: ColumnKind -> Maybe (ParseBytes a)
      -- ^ the parser for a column's binary-protocol fields, if the column fits
    }

instance Functor FieldDecoder where
    fmap f decoder = FieldDecoder
        { decoderTypeName = decoderTypeName decoder
        , decoderNull     = f <$> decoderNull decoder
        , decoderText     = fmap (mapParse f) . decoderText decoder
        , decoderBinary   = fmap (mapParse f) . decoderBinary decoder
        }

mapParse :: (a -> b) -> ParseBytes a -> ParseBytes b
mapParse f parse = \fieldBytes -> case parse fieldBytes of
    (# kind | #)  -> (# kind | #)
    (# | value #) -> (# | f value #)
{-# INLINE mapParse #-}

-- | NULL becomes 'Nothing'; any other field is decoded as before.
--
-- @since 1.3.4
nullable :: FieldDecoder a -> FieldDecoder (Maybe a)
nullable decoder = (Just <$> decoder)
    { decoderTypeName = "Maybe " <> decoderTypeName decoder
    , decoderNull     = Just Nothing
    }

-- | A decoder for the columns whose fields become one of @kinds@. A column of
-- type NULL fits every decoder: all its fields are NULL, which 'decoderNull'
-- answers.
kindDecoder :: Text -> [ValueKind] -> ParseBytes a -> ParseBytes a -> FieldDecoder a
kindDecoder typeName kinds textParse binaryParse = FieldDecoder
    { decoderTypeName = typeName
    , decoderNull     = Nothing
    , decoderText     = whenKindIn kinds textParse
    , decoderBinary   = whenKindIn kinds binaryParse
    }

whenKindIn :: [ValueKind] -> ParseBytes a -> ColumnKind -> Maybe (ParseBytes a)
whenKindIn kinds parse column = case column of
    NullColumn         -> Just parse
    ValueColumn _ kind -> if kind `elem` kinds then Just parse else Nothing

-- | TINYINT.
int8 :: FieldDecoder Int8
int8 = kindDecoder "Int8" [KindInt8] (lexedField lexSignedIntegral)
    (fixedField 1 (int8Bits . B.unsafeHead))

-- | TINYINT UNSIGNED.
word8 :: FieldDecoder Word8
word8 = kindDecoder "Word8" [KindInt8U] (lexedField lexSignedIntegral)
    (fixedField 1 B.unsafeHead)

-- | SMALLINT.
int16 :: FieldDecoder Int16
int16 = kindDecoder "Int16" [KindInt16] (lexedField lexSignedIntegral)
    (fixedField 2 (int16Bits . littleEndian16))

-- | SMALLINT UNSIGNED.
word16 :: FieldDecoder Word16
word16 = kindDecoder "Word16" [KindInt16U] (lexedField lexSignedIntegral)
    (fixedField 2 littleEndian16)

-- | INT and MEDIUMINT.
int32 :: FieldDecoder Int32
int32 = kindDecoder "Int32" [KindInt32] (lexedField lexSignedIntegral)
    (fixedField 4 (int32Bits . littleEndian32))

-- | INT UNSIGNED and MEDIUMINT UNSIGNED.
word32 :: FieldDecoder Word32
word32 = kindDecoder "Word32" [KindInt32U] (lexedField lexSignedIntegral)
    (fixedField 4 littleEndian32)

-- | BIGINT.
int64 :: FieldDecoder Int64
int64 = kindDecoder "Int64" [KindInt64] (lexedField lexSignedIntegral)
    (fixedField 8 (int64Bits . littleEndian64))

-- | BIGINT UNSIGNED.
word64 :: FieldDecoder Word64
word64 = kindDecoder "Word64" [KindInt64U] (lexedField lexSignedIntegral)
    (fixedField 8 littleEndian64)

-- | FLOAT.
float :: FieldDecoder Float
float = kindDecoder "Float" [KindFloat] (lexedField lexSignedFraction)
    (fixedField 4 (castWord32ToFloat . littleEndian32))

-- | DOUBLE.
double :: FieldDecoder Double
double = kindDecoder "Double" [KindDouble] (lexedField lexSignedFraction)
    (fixedField 8 (castWord64ToDouble . littleEndian64))

-- | DECIMAL; the binary protocol sends it as text too.
scientific :: FieldDecoder Scientific
scientific = kindDecoder "Scientific" [KindDecimal] (lexedField lexSignedFraction)
    (lexedField lexSignedFraction)

-- | The string columns in a character set other than binary. Invalid UTF-8 is
-- a 'FieldInvalidUtf8'.
text :: FieldDecoder Text
text = kindDecoder "Text" [KindText] utf8Field utf8Field

-- | The string columns in the binary character set: BINARY, VARBINARY, BLOB.
bytes :: FieldDecoder ByteString
bytes = kindDecoder "ByteString" [KindBytes] bytesField bytesField

-- | DATE.
day :: FieldDecoder Day
day = kindDecoder "Day" [KindDate] (lexedField lexDate) binaryDay

-- | DATETIME and TIMESTAMP.
localTime :: FieldDecoder LocalTime
localTime = kindDecoder "LocalTime" [KindDateTime, KindTimeStamp] (lexedField lexLocalTime)
    binaryLocalTime

-- | TIME, when not negative; a negative one is a 'FieldNegativeTime'. The
-- hours may exceed 24, as TIME spans up to 838 hours.
timeOfDay :: FieldDecoder TimeOfDay
timeOfDay = kindDecoder "TimeOfDay" [KindTime]
    (nonNegativeTime (lexedField lexSignedTime)) (nonNegativeTime binarySignedTime)

-- | TINYINT and TINYINT UNSIGNED, as MySQL stores BOOL: anything but 0 is 'True'.
bool :: FieldDecoder Bool
bool = FieldDecoder
    { decoderTypeName = "Bool"
    , decoderNull     = Nothing
    , decoderText     = boolText
    , decoderBinary   = whenKindIn [KindInt8, KindInt8U] (fixedField 1 ((/= 0) . B.unsafeHead))
    }

-- | The signed and unsigned text of a TINYINT overflow differently, so each
-- kind keeps its own type, as 'Database.MySQL.Field.decodeBool' does.
boolText :: ColumnKind -> Maybe (ParseBytes Bool)
boolText column = case column of
    NullColumn -> Just (lexedField lexNonZeroInt8)
    ValueColumn _ kind ->
        if  | kind == KindInt8  -> Just (lexedField lexNonZeroInt8)
            | kind == KindInt8U -> Just (lexedField lexNonZeroWord8)
            | otherwise         -> Nothing

lexNonZeroInt8 :: ByteString -> Maybe Bool
lexNonZeroInt8 fieldBytes = (/= (0 :: Int8)) <$> lexSignedIntegral fieldBytes

lexNonZeroWord8 :: ByteString -> Maybe Bool
lexNonZeroWord8 fieldBytes = (/= (0 :: Word8)) <$> lexSignedIntegral fieldBytes

-- | Every column, as 'Database.MySQL.Base.query_' decodes it; NULL is
-- 'MySQLNull'. The values are as lazy as 'query_' leaves them.
mysqlValue :: FieldDecoder MySQLValue
mysqlValue = FieldDecoder
    { decoderTypeName = "MySQLValue"
    , decoderNull     = Just MySQLNull
    , decoderText     = Just . textMySQLValue
    , decoderBinary   = Just . binaryMySQLValue
    }

textMySQLValue :: ColumnKind -> ParseBytes MySQLValue
textMySQLValue column fieldBytes = case column of
    NullColumn -> (# | MySQLNull #)
    ValueColumn fieldType kind -> case decodeTextValue fieldType kind fieldBytes of
        Left fieldError -> (# textFieldErrorKind fieldError | #)
        Right value     -> (# | value #)

-- | 'Database.MySQL.Protocol.MySQLValue.getBinaryField' on a field's bytes.
binaryMySQLValue :: ColumnKind -> ParseBytes MySQLValue
binaryMySQLValue column fieldBytes = case column of
    NullColumn -> (# | MySQLNull #)
    ValueColumn fieldType kind -> case kind of
        KindDecimal     -> mapParse MySQLDecimal (lexedField lexSignedFraction) fieldBytes
        KindInt8U       -> mapParse MySQLInt8U (fixedField 1 B.unsafeHead) fieldBytes
        KindInt8        -> mapParse MySQLInt8 (fixedField 1 (int8Bits . B.unsafeHead)) fieldBytes
        KindInt16U      -> mapParse MySQLInt16U (fixedField 2 littleEndian16) fieldBytes
        KindInt16       -> mapParse MySQLInt16 (fixedField 2 (int16Bits . littleEndian16)) fieldBytes
        KindInt32U      -> mapParse MySQLInt32U (fixedField 4 littleEndian32) fieldBytes
        KindInt32       -> mapParse MySQLInt32 (fixedField 4 (int32Bits . littleEndian32)) fieldBytes
        KindInt64U      -> mapParse MySQLInt64U (fixedField 8 littleEndian64) fieldBytes
        KindInt64       -> mapParse MySQLInt64 (fixedField 8 (int64Bits . littleEndian64)) fieldBytes
        KindFloat       -> mapParse MySQLFloat (fixedField 4 (castWord32ToFloat . littleEndian32)) fieldBytes
        KindDouble      -> mapParse MySQLDouble (fixedField 8 (castWord64ToDouble . littleEndian64)) fieldBytes
        KindYear        -> mapParse MySQLYear (fixedField 2 littleEndian16) fieldBytes
        KindTimeStamp   -> mapParse MySQLTimeStamp binaryLocalTime fieldBytes
        KindDateTime    -> mapParse MySQLDateTime binaryLocalTime fieldBytes
        KindDate        -> mapParse MySQLDate binaryDay fieldBytes
        KindTime        -> mapParse (uncurry MySQLTime) binarySignedTime fieldBytes
        KindGeometry    -> (# | MySQLGeometry fieldBytes #)
        KindText        -> (# | MySQLText (T.decodeUtf8 fieldBytes) #)
        KindBytes       -> (# | MySQLBytes fieldBytes #)
        KindBit         -> case decodeTextBit fieldBytes of
            Left fieldError -> (# textFieldErrorKind fieldError | #)
            Right value     -> (# | value #)
        KindUnsupported -> (# FieldUnsupportedType fieldType | #)

textFieldErrorKind :: TextFieldError -> FieldErrorKind
textFieldErrorKind fieldError = case fieldError of
    TextFieldUnparsable _ fieldBytes     -> FieldUnparsable fieldBytes
    TextFieldBitTooWide width            -> FieldBitTooWide width
    TextFieldUnsupportedType fieldType   -> FieldUnsupportedType fieldType

-- | A text field the lexer reads, evaluated.
lexedField :: (ByteString -> Maybe a) -> ParseBytes a
lexedField lexer = \fieldBytes -> case lexer fieldBytes of
    Just !value -> (# | value #)
    Nothing     -> (# FieldUnparsable fieldBytes | #)
{-# INLINE lexedField #-}

utf8Field :: ParseBytes Text
utf8Field fieldBytes = case T.decodeUtf8' fieldBytes of
    Right !decoded -> (# | decoded #)
    Left _         -> (# FieldInvalidUtf8 fieldBytes | #)

bytesField :: ParseBytes ByteString
bytesField fieldBytes = (# | fieldBytes #)

-- | A binary field of exactly @width@ bytes, evaluated.
fixedField :: Int -> (ByteString -> a) -> ParseBytes a
fixedField width decode = \fieldBytes ->
    if B.length fieldBytes == width
    then let !value = decode fieldBytes in (# | value #)
    else (# FieldUnparsable fieldBytes | #)
{-# INLINE fixedField #-}

-- | The binary protocol sends signed integers in two's complement, so the
-- signed value has the same bits as the unsigned one read from the wire.
int8Bits :: Word8 -> Int8
int8Bits (W8# bits) = I8# (word8ToInt8# bits)

int16Bits :: Word16 -> Int16
int16Bits (W16# bits) = I16# (word16ToInt16# bits)

int32Bits :: Word32 -> Int32
int32Bits (W32# bits) = I32# (word32ToInt32# bits)

int64Bits :: Word64 -> Int64
int64Bits (W64# bits) = I64# (word64ToInt64# bits)

littleEndian16 :: ByteString -> Word16
littleEndian16 = B.foldr' prependByte16 0

littleEndian32 :: ByteString -> Word32
littleEndian32 = B.foldr' prependByte32 0

littleEndian64 :: ByteString -> Word64
littleEndian64 = B.foldr' prependByte64 0

prependByte16 :: Word8 -> Word16 -> Word16
prependByte16 byte higherBytes = unsafeShiftL higherBytes 8 .|. Word8.toWord16 byte

prependByte32 :: Word8 -> Word32 -> Word32
prependByte32 byte higherBytes = unsafeShiftL higherBytes 8 .|. Word8.toWord32 byte

prependByte64 :: Word8 -> Word64 -> Word64
prependByte64 byte higherBytes = unsafeShiftL higherBytes 8 .|. Word8.toWord64 byte

-- | A binary DATE: no bytes for the zero date, or a 2-byte year, month and day.
binaryDay :: ParseBytes Day
binaryDay fieldBytes =
    if  | B.null fieldBytes        -> (# | fromGregorian 0 0 0 #)
        | B.length fieldBytes == 4 -> let !date = binaryDate fieldBytes in (# | date #)
        | otherwise                -> (# FieldUnparsable fieldBytes | #)

binaryDate :: ByteString -> Day
binaryDate fieldBytes = fromGregorian
    (Word16.toInteger (littleEndian16 (B.unsafeTake 2 fieldBytes)))
    (Word8.toInt (B.unsafeIndex fieldBytes 2))
    (Word8.toInt (B.unsafeIndex fieldBytes 3))

-- | A binary DATETIME or TIMESTAMP: its length says which parts are present,
-- 0 (the zero date), 4 (the date), 7 (and the time) or 11 (and microseconds).
binaryLocalTime :: ParseBytes LocalTime
binaryLocalTime fieldBytes =
    if  | B.null fieldBytes ->
            (# | LocalTime (fromGregorian 0 0 0) midnight #)
        | B.length fieldBytes == 4 ->
            let !value = LocalTime (binaryDate fieldBytes) midnight in (# | value #)
        | B.length fieldBytes == 7 || B.length fieldBytes == 11 ->
            let !value = LocalTime (binaryDate fieldBytes)
                                   (binaryClock (B.unsafeDrop 4 fieldBytes))
            in (# | value #)
        | otherwise -> (# FieldUnparsable fieldBytes | #)

-- | Hours, minutes and seconds, then microseconds when 7 bytes are left.
binaryClock :: ByteString -> TimeOfDay
binaryClock clock = TimeOfDay
    (Word8.toInt (B.unsafeIndex clock 0))
    (Word8.toInt (B.unsafeIndex clock 1))
    (binarySeconds (B.unsafeIndex clock 2) (B.unsafeDrop 3 clock))

-- | Whole seconds plus the 4-byte microseconds if present, exactly: a 'Pico'
-- counts 10^12 per second.
binarySeconds :: Word8 -> ByteString -> Pico
binarySeconds seconds microseconds = MkFixed
    (Word8.toInteger seconds * 1000000000000
        + (if B.length microseconds == 4
           then Word32.toInteger (littleEndian32 microseconds) * 1000000
           else 0))

-- | A binary TIME: no bytes for zero, or a sign, 4-byte days, hours, minutes,
-- seconds and, in the 12-byte form, microseconds. The sign is 1 for negative.
binarySignedTime :: ParseBytes (Word8, TimeOfDay)
binarySignedTime fieldBytes =
    if  | B.null fieldBytes -> (# | (0, midnight) #)
        | B.length fieldBytes == 8 || B.length fieldBytes == 12 ->
            case Word32.toInt (littleEndian32 (B.unsafeTake 4 (B.unsafeDrop 1 fieldBytes))) of
                Nothing -> (# FieldUnparsable fieldBytes | #)
                Just days ->
                    let !value = ( B.unsafeHead fieldBytes
                                 , TimeOfDay (days * 24 + Word8.toInt (B.unsafeIndex fieldBytes 5))
                                             (Word8.toInt (B.unsafeIndex fieldBytes 6))
                                             (binarySeconds (B.unsafeIndex fieldBytes 7)
                                                            (B.unsafeDrop 8 fieldBytes)) )
                    in (# | value #)
        | otherwise -> (# FieldUnparsable fieldBytes | #)

nonNegativeTime :: ParseBytes (Word8, TimeOfDay) -> ParseBytes TimeOfDay
nonNegativeTime parse = \fieldBytes -> case parse fieldBytes of
    (# kind | #) -> (# kind | #)
    (# | (sign, time) #) ->
        if sign == 0 then (# | time #) else (# FieldNegativeTime fieldBytes | #)
{-# INLINE nonNegativeTime #-}

-- | Decodes a row, one 'field' per column, in order. It is checked against the
-- result set's column definitions once, before the first row.
--
-- @since 1.3.4
data RowDecoder a = RowDecoder
    !Int
    -- ^ how many columns it reads
    (forall protocol. RowProtocol protocol
        => V.Vector ColumnDef -> Int -> Either ColumnMismatch (FieldSteps protocol a))
    -- ^ fits it to the columns from the given column number on

-- Decision: the instance methods and the step combinators are inlined, so that
-- a decoder written as one expression, such as @Employee \<$\> field a \<*\>
-- field b@, compiles to one walk that applies @Employee@ to all its fields at
-- once. Left to the closures of the run-time composition, every field paid a
-- generic partial application of the constructor (about 1,400 of 9,300
-- instructions per row on the employees benchmark).
instance Functor RowDecoder where
    fmap f (RowDecoder width prepare) =
        RowDecoder width (\columns start -> mapSteps f <$> prepare columns start)
    {-# INLINE fmap #-}

instance Applicative RowDecoder where
    pure value = RowDecoder 0 (\_ _ -> Right (pureSteps value))
    {-# INLINE pure #-}
    RowDecoder functionWidth prepareFunction <*> RowDecoder argumentWidth prepareArgument =
        RowDecoder (functionWidth + argumentWidth) (\columns start ->
            apSteps <$> prepareFunction columns start
                    <*> prepareArgument columns (start + functionWidth))
    {-# INLINE (<*>) #-}

-- | Decodes the next column with this decoder.
--
-- @since 1.3.4
field :: FieldDecoder a -> RowDecoder a
field decoder = RowDecoder 1 (\columns column -> case columns V.!? column of
    Nothing -> Left (ColumnCountMismatch (column + 1) (V.length columns))
    Just definition ->
        fieldSteps column definition <$> prepareFieldParser decoder column definition)
{-# INLINE field #-}

-- | Reads fields from a row's bytes at an offset, and returns the offset after
-- them. A row is walked once, in column order, each field parsed as it is
-- reached, so no field bounds are stored.
newtype FieldSteps (protocol :: Type) a = FieldSteps (ByteString -> Int -> (# FieldError | (# a, Int #) #))

pureSteps :: a -> FieldSteps protocol a
pureSteps value = FieldSteps (\_ offset -> (# | (# value, offset #) #))
{-# INLINE pureSteps #-}

mapSteps :: (a -> b) -> FieldSteps protocol a -> FieldSteps protocol b
mapSteps f (FieldSteps run) = FieldSteps (\row offset -> case run row offset of
    (# fieldError | #)        -> (# fieldError | #)
    (# | (# value, next #) #) -> let !mapped = f value in (# | (# mapped, next #) #))
{-# INLINE mapSteps #-}

-- | Applies as soon as both sides are decoded, so a row decodes to an evaluated
-- value instead of a chain of thunks that each later force has to update.
apSteps :: FieldSteps protocol (a -> b) -> FieldSteps protocol a -> FieldSteps protocol b
apSteps (FieldSteps runFunction) (FieldSteps runArgument) = FieldSteps (\row offset ->
    case runFunction row offset of
        (# fieldError | #) -> (# fieldError | #)
        (# | (# function, afterFunction #) #) -> case runArgument row afterFunction of
            (# fieldError | #) -> (# fieldError | #)
            (# | (# argument, afterArgument #) #) ->
                let !applied = function argument in (# | (# applied, afterArgument #) #))
{-# INLINE apSteps #-}

-- | A 'RowDecoder' fitted to a result set's columns.
--
-- @since 1.3.4
data PreparedRow (protocol :: Type) a = PreparedRow (RowStart protocol) (FieldSteps protocol a)

-- | Where a row's first field starts, or why the row is too short to have one.
newtype RowStart (protocol :: Type) = RowStart (ByteString -> (# FieldError | Int #))

-- | Fits a 'RowDecoder' to a result set's columns. It must read exactly as many
-- columns as there are, each of a type its decoder accepts.
--
-- @since 1.3.4
prepareRowDecoder :: RowProtocol protocol
                  => RowDecoder a -> V.Vector ColumnDef -> Either ColumnMismatch (PreparedRow protocol a)
prepareRowDecoder (RowDecoder width prepare) columns =
    if width /= V.length columns
    then Left (ColumnCountMismatch width (V.length columns))
    else PreparedRow (rowStart width) <$> prepare columns 0

-- | Decodes the body of a row packet of the protocol. Bytes after the last
-- field are ignored, as 'decodeTextRow' ignores them.
--
-- @since 1.3.4
runPreparedRow :: PreparedRow protocol a -> ByteString -> Either FieldError a
runPreparedRow (PreparedRow (RowStart start) (FieldSteps run)) row = case start row of
    (# fieldError | #) -> Left fieldError
    (# | offset #) -> case run row offset of
        (# fieldError | #)     -> Left fieldError
        (# | (# value, _ #) #) -> Right value

-- | 'TextProtocol' or 'BinaryProtocol': how the rows of that protocol lay out
-- their fields, and which of a 'FieldDecoder''s parsers they need.
--
-- @since 1.3.4
class RowProtocol (protocol :: Type) where
    protocolParser :: FieldDecoder a -> ColumnKind -> Maybe (FieldParser protocol a)
    fieldSteps     :: Int -> ColumnDef -> FieldParser protocol a -> FieldSteps protocol a
    rowStart       :: Int -> RowStart protocol

instance RowProtocol TextProtocol where
    protocolParser decoder column = FieldParser (decoderNull decoder) <$> decoderText decoder column
    fieldSteps column _ parser = FieldSteps (textFieldStep column parser)
    rowStart _ = RowStart textRowStart

instance RowProtocol BinaryProtocol where
    protocolParser decoder column = FieldParser (decoderNull decoder) <$> decoderBinary decoder column
    fieldSteps column definition parser =
        FieldSteps (binaryFieldStep column (binaryWidth definition) parser)
    rowStart columnCount = RowStart (binaryRowStart columnCount)

textRowStart :: ByteString -> (# FieldError | Int #)
textRowStart _ = (# | 0 #)

-- | A text-protocol field: the NULL marker or a length-encoded value.
textFieldStep :: Int -> FieldParser TextProtocol a -> ByteString -> Int -> (# FieldError | (# a, Int #) #)
textFieldStep column parser = \row offset ->
    if  | offset >= B.length row -> (# FieldError column FieldRowEndsEarly | #)
        | B.unsafeIndex row offset == 0xFB -> nullStep column parser (offset + 1)
        | otherwise -> lengthEncodedStep column parser row offset
{-# INLINE textFieldStep #-}

-- | The fields of a binary-protocol row follow the 0x00 header and the NULL map.
binaryRowStart :: Int -> ByteString -> (# FieldError | Int #)
binaryRowStart columnCount row =
    let start = 1 + RawRow.binaryNullMapLength columnCount
    in if start > B.length row then (# FieldError 0 FieldRowEndsEarly | #) else (# | start #)

-- | A binary-protocol field: absent when the NULL map marks it, otherwise laid
-- out as its column's 'BinaryWidth' says.
binaryFieldStep :: Int -> BinaryWidth -> FieldParser BinaryProtocol a -> ByteString -> Int
                -> (# FieldError | (# a, Int #) #)
binaryFieldStep column width parser = \row offset ->
    if RawRow.isNullInMap row column
    then nullStep column parser offset
    else case width of
        BinaryNoBytes       -> sliceStep column parser row offset 0
        BinaryFixed size    -> sliceStep column parser row offset size
        BinaryLengthEncoded ->
            if offset >= B.length row
            then (# FieldError column FieldRowEndsEarly | #)
            else lengthEncodedStep column parser row offset
{-# INLINE binaryFieldStep #-}

nullStep :: Int -> FieldParser protocol a -> Int -> (# FieldError | (# a, Int #) #)
nullStep column parser next = case parserNull parser of
    Just value -> (# | (# value, next #) #)
    Nothing    -> (# FieldError column FieldUnexpectedNull | #)

-- | A length at @offset@, which must be inside the row, then that many bytes.
lengthEncodedStep :: Int -> FieldParser protocol a -> ByteString -> Int -> (# FieldError | (# a, Int #) #)
lengthEncodedStep column parser row offset = case lengthEncodedInt row offset of
    (# kind | #) -> (# FieldError column (lengthErrorKind kind) | #)
    (# | (# fieldLength, fieldStart #) #) -> sliceStep column parser row fieldStart fieldLength

sliceStep :: Int -> FieldParser protocol a -> ByteString -> Int -> Int -> (# FieldError | (# a, Int #) #)
sliceStep column parser row start fieldLength =
    if fieldLength > B.length row - start
    then (# FieldError column FieldRowEndsEarly | #)
    else case parserBytes parser (B.unsafeTake fieldLength (B.unsafeDrop start row)) of
        (# kind | #)  -> (# FieldError column kind | #)
        (# | value #) -> (# | (# value, start + fieldLength #) #)

lengthErrorKind :: RowErrorKind -> FieldErrorKind
lengthErrorKind kind = case kind of
    RowEndsEarly                  -> FieldRowEndsEarly
    RowInvalidLengthPrefix prefix -> FieldInvalidLengthPrefix prefix
    RowLengthOverflow             -> FieldLengthOverflow
    RowFieldError fieldError      -> textFieldErrorKind fieldError

-- | A 'FieldDecoder' fitted to one column of a result set.
--
-- @since 1.3.4
data FieldParser (protocol :: Type) a = FieldParser
    { parserNull  :: !(Maybe a)
    , parserBytes :: ParseBytes a
    }

-- | Fits a 'FieldDecoder' to column number @column@, whose definition this is.
--
-- @since 1.3.4
prepareFieldParser :: RowProtocol protocol
                   => FieldDecoder a -> Int -> ColumnDef -> Either ColumnMismatch (FieldParser protocol a)
prepareFieldParser decoder column definition =
    case protocolParser decoder (columnKind definition) of
        Just parser -> Right parser
        Nothing     -> Left (ColumnTypeMismatch column (columnName definition)
                                (columnType definition) (decoderTypeName decoder))

-- | Decodes field @column@ of a 'RawRow', passing the outcome to a continuation
-- so that no 'Either' is allocated, as a library decoding by column number
-- needs.
--
-- @since 1.3.4
runFieldParser :: FieldParser protocol a -> RawRow protocol -> Int -> (FieldError -> r) -> (a -> r) -> r
runFieldParser parser row column onError onValue = case parseRawField column parser row of
    (# fieldError | #) -> onError fieldError
    (# | value #)      -> onValue value
{-# INLINE runFieldParser #-}

parseRawField :: Int -> FieldParser protocol a -> RawRow protocol -> (# FieldError | a #)
parseRawField column parser row = case rawField row column of
    RawNull -> case parserNull parser of
        Just value -> (# | value #)
        Nothing    -> (# FieldError column FieldUnexpectedNull | #)
    RawBytes fieldBytes -> case parserBytes parser fieldBytes of
        (# kind | #)  -> (# FieldError column kind | #)
        (# | value #) -> (# | value #)
    RawAbsent -> (# FieldError column FieldAbsent | #)

-- | A result set that does not fit a 'RowDecoder', raised before any row is read.
--
-- @since 1.3.4
data ColumnMismatch
    = ColumnTypeMismatch !Int !ByteString !FieldType !Text
      -- ^ column number, column name, its type, and the Haskell type of the
      -- decoder that does not accept it
    | ColumnCountMismatch !Int !Int
      -- ^ how many columns the decoder reads, how many the result set has
    deriving (Show, Eq)

instance Exception ColumnMismatch

-- | A field that did not decode, with its column number.
--
-- @since 1.3.4
data FieldError = FieldError
    { fieldErrorColumn :: !Int
    , fieldErrorKind   :: !FieldErrorKind
    } deriving (Show, Eq)

instance Exception FieldError

-- | @since 1.3.4
data FieldErrorKind
    = FieldUnexpectedNull
      -- ^ a NULL for a decoder that takes none; see 'nullable'
    | FieldUnparsable !ByteString
      -- ^ the bytes are not a value of the column's type
    | FieldInvalidUtf8 !ByteString
    | FieldNegativeTime !ByteString
      -- ^ a negative TIME for 'timeOfDay'
    | FieldBitTooWide !Int
      -- ^ a BIT field longer than the 8 bytes of a 'Word64'
    | FieldUnsupportedType !FieldType
      -- ^ a column type 'mysqlValue' has no decoder for
    | FieldAbsent
      -- ^ the 'RawRow' has no field with that column number
    | FieldRowEndsEarly
      -- ^ the row ends inside or before this field
    | FieldInvalidLengthPrefix !Word8
      -- ^ a field starting with a byte that is neither a length nor the NULL marker
    | FieldLengthOverflow
      -- ^ an 8-byte length above 'maxBound' of 'Int'
    deriving (Show, Eq)
