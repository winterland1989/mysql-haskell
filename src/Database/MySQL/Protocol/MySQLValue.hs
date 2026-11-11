{-# OPTIONS_GHC -funbox-strict-fields #-}

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
  , getBinaryField
  , putBinaryField
  , getBinaryRow
  , getBinaryRowVector
  , decodeMySQLLatin1
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
import           Data.Time.Calendar                 (Day, fromGregorianValid,
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
import           Text.Printf                        (printf)
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Unwitch.Convert.Int                as Int
import qualified Unwitch.Convert.Word16             as Word16
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
    maybe (Left raw) Right
        (fromGregorianValid (Word16.toInteger year) (Word8.toInt month) (Word8.toInt day))

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
getTextField f
    | t == mySQLTypeNull            = pure MySQLNull
    | t == mySQLTypeDecimal
        || t == mySQLTypeNewDecimal = feedLenEncBytes t MySQLDecimal fracLexer
    | t == mySQLTypeTiny            = if isUnsigned then feedLenEncBytes t MySQLInt8U intLexer
                                                    else feedLenEncBytes t MySQLInt8 intLexer
    | t == mySQLTypeShort           = if isUnsigned then feedLenEncBytes t MySQLInt16U intLexer
                                                    else feedLenEncBytes t MySQLInt16 intLexer
    | t == mySQLTypeLong
        || t == mySQLTypeInt24      = if isUnsigned then feedLenEncBytes t MySQLInt32U intLexer
                                                    else feedLenEncBytes t MySQLInt32 intLexer
    | t == mySQLTypeLongLong        = if isUnsigned then feedLenEncBytes t MySQLInt64U intLexer
                                                    else feedLenEncBytes t MySQLInt64 intLexer
    | t == mySQLTypeFloat           = feedLenEncBytes t MySQLFloat fracLexer
    | t == mySQLTypeDouble          = feedLenEncBytes t MySQLDouble fracLexer
    | t == mySQLTypeYear            = feedLenEncBytes t MySQLYear intLexer
    | t == mySQLTypeTimestamp
        || t == mySQLTypeTimestamp2 = feedLenEncBytes t (uncurry timeStampValue) $ \ bs ->
                                          guard (B.length bs >= 12) >>
                                          (,) <$> textDateParts bs <*> timeParser (B.drop 11 bs)
    | t == mySQLTypeDateTime
        || t == mySQLTypeDateTime2  = feedLenEncBytes t (uncurry dateTimeValue) $ \ bs ->
                                          guard (B.length bs >= 12) >>
                                          (,) <$> textDateParts bs <*> timeParser (B.drop 11 bs)
    | t == mySQLTypeDate
        || t == mySQLTypeNewDate    = feedLenEncBytes t dateValue textDateParts
    | t == mySQLTypeTime
        || t == mySQLTypeTime2      = feedLenEncBytes t id $ \ bs ->
                                          guard (not (B.null bs)) >>
                                          if B.index bs 0 == 45  -- '-'
                                               then MySQLTime 1 <$> timeParser (B.drop 1 bs)
                                               else MySQLTime 0 <$> timeParser bs

    | t == mySQLTypeGeometry        = MySQLGeometry <$> getLenEncBytes
    | t == mySQLTypeVarChar
        || t == mySQLTypeEnum
        || t == mySQLTypeSet
        || t == mySQLTypeTinyBlob
        || t == mySQLTypeMediumBlob
        || t == mySQLTypeLongBlob
        || t == mySQLTypeBlob
        || t == mySQLTypeVarString
        || t == mySQLTypeString     = getLenEncBytes >>= stringColumnValue f

    | t == mySQLTypeBit             = MySQLBit <$> (getBits =<< getLenEncInt)

    | otherwise                     = fail $ "Database.MySQL.Protocol.MySQLValue: missing text decoder for " ++ show t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    intLexer bs = fst <$> LexInt.readSigned LexInt.readDecimal bs
    fracLexer bs = fst <$> LexFrac.readSigned LexFrac.readDecimal bs

    timeParser bs = do
        (hh, rest) <- LexInt.readDecimal bs
        guard (not (B.null rest))
        (mm, rest') <- LexInt.readDecimal (B.tail rest)
        guard (not (B.null rest'))
        (ss, _) <- LexFrac.readDecimal (B.tail rest')
        return (TimeOfDay hh mm ss)


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
data ResultCharset
    = BinaryCharset
    | Latin1Charset
    | Utf8OrOtherCharset

resultCharset :: Word16 -> ResultCharset
resultCharset charsetId =
    if  | charsetId == 63 -> BinaryCharset
        | charsetId `elem` latin1Collations -> Latin1Charset
        | otherwise -> Utf8OrOtherCharset

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
-- keeps ASCII-only values working as they always did, and fails with a
-- DecodePacketException naming the charset and column on bytes that are not
-- UTF-8, rather than return mangled text.

stringColumnValue :: ColumnDef -> ByteString -> Get MySQLValue
stringColumnValue column bytes = case resultCharset (columnCharSet column) of
    BinaryCharset -> pure (MySQLBytes bytes)
    Latin1Charset -> pure (MySQLText (decodeMySQLLatin1 bytes))
    Utf8OrOtherCharset -> case T.decodeUtf8' bytes of
        Right text -> pure (MySQLText text)
        Left _ -> fail $ "Database.MySQL.Protocol.MySQLValue: column "
            ++ show (columnName column) ++ " arrived in character set id "
            ++ show (columnCharSet column) ++ ", which mysql-haskell can only decode as UTF-8 or latin1,"
            ++ " and its bytes are not valid UTF-8. Use a UTF-8 connection charset such as"
            ++ " defaultConnectInfoMB4's, or select the column as BINARY to get its bytes."

-- | MySQL's latin1, which is Windows-1252 with its five unassigned bytes
-- (0x81, 0x8D, 0x8F, 0x90, 0x9D) mapped to the C1 controls of the same number.
decodeMySQLLatin1 :: ByteString -> Text
decodeMySQLLatin1 = T.pack . map latin1Char . B.unpack

latin1Char :: Word8 -> Char
latin1Char byte = case byte of
    0x80 -> '\x20AC'
    0x82 -> '\x201A'
    0x83 -> '\x0192'
    0x84 -> '\x201E'
    0x85 -> '\x2026'
    0x86 -> '\x2020'
    0x87 -> '\x2021'
    0x88 -> '\x02C6'
    0x89 -> '\x2030'
    0x8A -> '\x0160'
    0x8B -> '\x2039'
    0x8C -> '\x0152'
    0x8E -> '\x017D'
    0x91 -> '\x2018'
    0x92 -> '\x2019'
    0x93 -> '\x201C'
    0x94 -> '\x201D'
    0x95 -> '\x2022'
    0x96 -> '\x2013'
    0x97 -> '\x2014'
    0x98 -> '\x02DC'
    0x99 -> '\x2122'
    0x9A -> '\x0161'
    0x9B -> '\x203A'
    0x9C -> '\x0153'
    0x9E -> '\x017E'
    0x9F -> '\x0178'
    sameCodePoint -> toEnum (Word8.toInt sameCodePoint)

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
