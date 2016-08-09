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

module Database.MySQL.Protocol.MySQLValue where

import qualified Blaze.Text                         as Textual
import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import           Data.ByteString.Builder.Scientific (formatScientificBuilder, FPFormat(..))
import  qualified         Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lex.Fractional     as LexFrac
import qualified Data.ByteString.Lex.Integral       as LexInt
import qualified Data.ByteString.Unsafe             as B
import qualified Data.ByteString.Lazy            as L
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
import           Data.Typeable
import           Data.Word
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Escape
import           Database.MySQL.Protocol.Packet
import           GHC.Generics (Generic)

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
-- TIME values may range from '-838:59:59' to '838:59:59', so 'MySQLTime' values consist of a sign and a
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
    | MySQLDate          !Day
    | MySQLTime          !Word8 !TimeOfDay -- ^ sign(0 = non-negative, 1 = negative) hh mm ss microsecond
                                           -- The sign is OPPOSITE to binlog one !!!
    | MySQLGeometry      !ByteString       -- ^ todo: parsing to something meanful
    | MySQLBytes         !ByteString
    | MySQLBit           !Word64
    | MySQLText          !Text
    | MySQLNull
  deriving (Show, Eq, Generic)

-- | Decide if usigned bit(0x80) and 'FieldType' for 'MySQLValue's.
--
mySQLValueType :: MySQLValue -> (FieldType, Word8)
mySQLValueType (MySQLDecimal      _)  = (MYSQL_TYPE_DECIMAL  , 0x00)
mySQLValueType (MySQLInt8U        _)  = (MYSQL_TYPE_TINY     , 0x80)
mySQLValueType (MySQLInt8         _)  = (MYSQL_TYPE_TINY     , 0x00)
mySQLValueType (MySQLInt16U       _)  = (MYSQL_TYPE_SHORT    , 0x80)
mySQLValueType (MySQLInt16        _)  = (MYSQL_TYPE_SHORT    , 0x00)
mySQLValueType (MySQLInt32U       _)  = (MYSQL_TYPE_LONG     , 0x80)
mySQLValueType (MySQLInt32        _)  = (MYSQL_TYPE_LONG     , 0x00)
mySQLValueType (MySQLInt64U       _)  = (MYSQL_TYPE_LONGLONG , 0x80)
mySQLValueType (MySQLInt64        _)  = (MYSQL_TYPE_LONGLONG , 0x00)
mySQLValueType (MySQLFloat        _)  = (MYSQL_TYPE_FLOAT    , 0x00)
mySQLValueType (MySQLDouble       _)  = (MYSQL_TYPE_DOUBLE   , 0x00)
mySQLValueType (MySQLYear         _)  = (MYSQL_TYPE_YEAR     , 0x80)
mySQLValueType (MySQLDateTime     _)  = (MYSQL_TYPE_DATETIME , 0x00)
mySQLValueType (MySQLTimeStamp    _)  = (MYSQL_TYPE_TIMESTAMP, 0x00)
mySQLValueType (MySQLDate         _)  = (MYSQL_TYPE_DATE     , 0x00)
mySQLValueType (MySQLTime       _ _)  = (MYSQL_TYPE_TIME     , 0x00)
mySQLValueType (MySQLBytes        _)  = (MYSQL_TYPE_BLOB     , 0x00)
mySQLValueType (MySQLGeometry     _)  = (MYSQL_TYPE_GEOMETRY , 0x00)
mySQLValueType (MySQLBit          _)  = (MYSQL_TYPE_BIT      , 0x00)
mySQLValueType (MySQLText         _)  = (MYSQL_TYPE_STRING   , 0x00)
mySQLValueType MySQLNull              = (MYSQL_TYPE_NULL     , 0x00)

--------------------------------------------------------------------------------
-- | Text protocol decoder
getTextField :: ColumnDef -> Get MySQLValue
getTextField f
    | t == MYSQL_TYPE_NULL              = pure MySQLNull
    | t == MYSQL_TYPE_DECIMAL
        || t == MYSQL_TYPE_NEWDECIMAL   = feedLenEncBytes t MySQLDecimal fracLexer
    | t == MYSQL_TYPE_TINY              = if isUnsigned then feedLenEncBytes t MySQLInt8U intLexer
                                                        else feedLenEncBytes t MySQLInt8 intLexer
    | t == MYSQL_TYPE_SHORT             = if isUnsigned then feedLenEncBytes t MySQLInt16U intLexer
                                                        else feedLenEncBytes t MySQLInt16 intLexer
    | t == MYSQL_TYPE_LONG
        || t == MYSQL_TYPE_INT24        = if isUnsigned then feedLenEncBytes t MySQLInt32U intLexer
                                                        else feedLenEncBytes t MySQLInt32 intLexer
    | t == MYSQL_TYPE_LONGLONG          = if isUnsigned then feedLenEncBytes t MySQLInt64U intLexer
                                                        else feedLenEncBytes t MySQLInt64 intLexer
    | t == MYSQL_TYPE_FLOAT             = feedLenEncBytes t MySQLFloat fracLexer
    | t == MYSQL_TYPE_DOUBLE            = feedLenEncBytes t MySQLDouble fracLexer
    | t == MYSQL_TYPE_YEAR              = feedLenEncBytes t MySQLYear intLexer
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_TIMESTAMP2   = feedLenEncBytes t MySQLTimeStamp $ \ bs ->
                                            LocalTime <$> dateParser bs <*> timeParser (B.drop 11 bs)
    | t == MYSQL_TYPE_DATETIME
        || t == MYSQL_TYPE_DATETIME2    = feedLenEncBytes t MySQLDateTime $ \ bs ->
                                            LocalTime <$> dateParser bs <*> timeParser (B.drop 11 bs)
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = feedLenEncBytes t MySQLDate dateParser
    | t == MYSQL_TYPE_TIME
        || t == MYSQL_TYPE_TIME2        = feedLenEncBytes t id $ \ bs ->
                                            if B.null bs
                                            then pure MySQLNull
                                            else if bs `BC.index` 0 == '-'
                                                 then MySQLTime 1 <$> timeParser (BC.drop 1 bs)
                                                 else MySQLTime 0 <$> timeParser bs

    | t == MYSQL_TYPE_GEOMETRY          = MySQLGeometry <$> getLenEncBytes
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = (if isText then MySQLText . T.decodeUtf8 else MySQLBytes) <$> getLenEncBytes

    | t == MYSQL_TYPE_BIT               = do len <- getLenEncInt
                                             if len == 0 then pure MySQLNull
                                                         else MySQLBit <$> getBits len

    | otherwise                         = fail $ "Database.MySQL.Protocol.MySQLValue: missing text decoder for " ++ show t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63
    intLexer bs = fst <$> LexInt.readSigned LexInt.readDecimal bs
    fracLexer bs = fst <$> LexFrac.readSigned LexFrac.readDecimal bs
    dateParser bs = do
        (yyyy, rest) <- LexInt.readDecimal bs
        (mm, rest') <- LexInt.readDecimal (B.tail rest)
        (dd, _) <- LexInt.readDecimal (B.tail rest')
        return (fromGregorian yyyy mm dd)

    timeParser bs = do
        (hh, rest) <- LexInt.readDecimal bs
        (mm, rest') <- LexInt.readDecimal (B.tail rest)
        (ss, _) <- LexFrac.readDecimal (B.tail rest')
        return (TimeOfDay hh mm ss)

    feedLenEncBytes typ con parser = do
        bs <- getLenEncBytes
        if B.null bs
            then return MySQLNull
            else case parser bs of
                Just v -> return (con v)
                Nothing -> fail $ "Database.MySQL.Protocol.MySQLValue: parsing " ++ show typ ++ " failed, input: " ++ BC.unpack bs

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
putTextField MySQLNull           = putBuilder "NULL"

putInQuotes :: Put -> Put
putInQuotes p = putCharUtf8 '\'' >> p >> putCharUtf8 '\''

--------------------------------------------------------------------------------
-- | Text row decoder
getTextRow :: [ColumnDef] -> Get [MySQLValue]
getTextRow fs = forM fs $ \ f -> do
    p <- lookAhead getWord8
    if p == 0xFB
    then getWord8 >> return MySQLNull
    else getTextField f

--------------------------------------------------------------------------------
-- | Binary protocol decoder
getBinaryField :: ColumnDef -> Get MySQLValue
getBinaryField f
    | t == MYSQL_TYPE_NULL              = pure MySQLNull
    | t == MYSQL_TYPE_DECIMAL
        || t == MYSQL_TYPE_NEWDECIMAL   = feedLenEncBytes t MySQLDecimal fracLexer
    | t == MYSQL_TYPE_TINY              = if isUnsigned then MySQLInt8U <$> getWord8
                                                        else MySQLInt8  <$> getInt8
    | t == MYSQL_TYPE_SHORT             = if isUnsigned then MySQLInt16U <$> getWord16le
                                                        else MySQLInt16  <$> getInt16le
    | t == MYSQL_TYPE_LONG
        || t == MYSQL_TYPE_INT24        = if isUnsigned then MySQLInt32U <$> getWord32le
                                                        else MySQLInt32  <$> getInt32le
    | t == MYSQL_TYPE_YEAR              = MySQLYear . fromIntegral <$> getWord16le
    | t == MYSQL_TYPE_LONGLONG          = if isUnsigned then MySQLInt64U <$> getWord64le
                                                        else MySQLInt64  <$> getInt64le
    | t == MYSQL_TYPE_FLOAT             = MySQLFloat  <$> getFloatle
    | t == MYSQL_TYPE_DOUBLE            = MySQLDouble <$> getDoublele
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_TIMESTAMP2   = do
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
    | t == MYSQL_TYPE_DATETIME
        || t == MYSQL_TYPE_DATETIME2    = do
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

    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = do
            n <- getLenEncInt
            case n of
               0 -> pure $ MySQLDate (fromGregorian 0 0 0)
               4 -> MySQLDate <$> (fromGregorian <$> getYear <*> getInt8' <*> getInt8')
               _ -> fail "Database.MySQL.Protocol.MySQLValue: wrong DATE length"

    | t == MYSQL_TYPE_TIME
        || t == MYSQL_TYPE_TIME2 = do
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

    | t == MYSQL_TYPE_GEOMETRY          = MySQLGeometry <$> getLenEncBytes
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = if isText then MySQLText . T.decodeUtf8 <$> getLenEncBytes
                                                    else MySQLBytes <$> getLenEncBytes
    | t == MYSQL_TYPE_BIT               = do len <- getLenEncInt
                                             if len == 0 then pure MySQLNull
                                                         else MySQLBit <$> getBits len
    | otherwise                         = fail $ "Database.MySQL.Protocol.MySQLValue:\
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
    getSecond8 = realToFrac <$> do
        s <- getInt8'
        ms <- fromIntegral <$> getWord32le :: Get Int
        pure $! (realToFrac s + realToFrac ms / 1000000 :: Pico)

    feedLenEncBytes typ con parser = do
        bs <- getLenEncBytes
        case parser bs of
            Just v -> return (con v)
            Nothing -> fail $ "Database.MySQL.Protocol.MySQLValue: \
                              \parsing " ++ show typ ++ " failed, input: " ++ BC.unpack bs

-- | convert a 'BitMap' to a Word64
--
-- Since 'Word64' has a @Bits@ instance, it's easier to deal with in haskell.
getBits :: Int -> Get Word64
getBits bytes =
    if  | bytes == 1 -> fromIntegral <$> getWord8
        | bytes == 2 -> fromIntegral <$> getWord16be
        | bytes == 3 -> fromIntegral <$> getWord24be
        | bytes == 4 -> fromIntegral <$> getWord32be
        | bytes == 5 -> fromIntegral <$> getWord40be
        | bytes == 6 -> fromIntegral <$> getWord48be
        | bytes == 7 -> fromIntegral <$> getWord56be
        | bytes == 8 -> fromIntegral <$> getWord64be
        | otherwise  -> fail $  "Database.MySQL.Protocol.MySQLValue: \
                                \wrong bit length size: " ++ show bytes
  where
    getWord40be, getWord48be, getWord56be :: Get Word64
    getWord40be = do
        a <- fromIntegral <$> getWord32be
        b <- fromIntegral <$> getWord8
        return $! (a `shiftL` 8) .|. b
    getWord48be = do
        a <- fromIntegral <$> getWord32be
        b <- fromIntegral <$> getWord16be
        return $! (a `shiftL` 16) .|. b
    getWord56be = do
        a <- fromIntegral <$> getWord32be
        b <- fromIntegral <$> getWord24be
        return $! (a `shiftL` 24) .|. b

putTextBits :: Word64 -> Put
putTextBits word = forM_ [63,62..0] $ \ pos ->
        if word `testBit` pos then putCharUtf8 '1' else putCharUtf8 '0'

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
putBinaryField (MySQLYear       n) = putLenEncBytes . L.toStrict . BB.toLazyByteString $
                                        Textual.integral n  -- this's really weird, it's not documented anywhere
                                                            -- we must put encode year into string in binary mode!
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
putBinaryField (MySQLBit    word)  = do putWord8 8     -- always put full
                                        putWord64be word
putBinaryField (MySQLText    t)    = putLenEncBytes (T.encodeUtf8 t)
putBinaryField MySQLNull           = return ()

putBinaryDay :: Day -> Put
putBinaryDay d = do let (yyyy, mm, dd) = toGregorian d
                    putWord16le (fromIntegral yyyy)
                    putWord8 (fromIntegral mm)
                    putWord8 (fromIntegral dd)

putBinaryTime' :: TimeOfDay -> Put
putBinaryTime' (TimeOfDay hh mm ss) = do let s = floor ss
                                             ms = floor $ (ss - realToFrac s) * 1000000
                                         putWord8 (fromIntegral hh)
                                         putWord8 (fromIntegral mm)
                                         putWord8 s
                                         putWord32le ms
putBinaryTime :: TimeOfDay -> Put
putBinaryTime (TimeOfDay hh mm ss) = do let s = floor ss
                                            ms = floor $ (ss - realToFrac s) * 1000000
                                            (d, h) = hh `quotRem` 24  -- hour may exceed 24 here
                                        putWord32le (fromIntegral d)
                                        putWord8 (fromIntegral h)
                                        putWord8 (fromIntegral mm)
                                        putWord8 s
                                        putWord32le ms

--------------------------------------------------------------------------------
-- | Binary row encoder
--
-- MySQL use a special null bitmap without offset = 2 here.
getBinaryRow :: [ColumnDef] -> Int -> Get [MySQLValue]
getBinaryRow fields flen = do
    _ <- getWord8           -- 0x00
    let maplen = (flen + 7 + 2) `shiftR` 3
    nullmap <- getByteString maplen
    go fields nullmap (0 :: Int)
  where
    go [] _       _        = pure []
    go (f:fs) nullmap pos = do
        r <- if isNull nullmap pos
                then return MySQLNull
                else getBinaryField f
        let pos' = pos + 1
        rest <- pos' `seq` go fs nullmap pos'
        return (r `seq` rest `seq` (r : rest))

    isNull nullmap pos =  -- This 'isNull' is special for offset = 2
        let (i, j) = (pos + 2) `quotRem` 8
        in (nullmap `B.unsafeIndex` i) `testBit` j

-- | Use 'ByteString' to present a bitmap.
--
-- When used for represent bits values, the underlining 'ByteString' follows:
--
--  * byteString: head       -> tail
--  * bit:        big endian -> little endian
--
-- When used as a null-map/present-map, every bit inside a byte
-- is mapped to a column, the mapping order is following:
--
--  * byteString: head -> tail
--  * column:     left -> right
--
newtype BitMap = BitMap { fromBitMap :: ByteString } deriving (Eq, Show)
-- | test if a column is set
--
-- The number counts from left to right.
--
isColumnSet :: BitMap -> Int -> Bool
isColumnSet (BitMap bitmap) pos =
    let (i, j) = pos `quotRem` 8
    in (bitmap `B.unsafeIndex` i) `testBit` j


-- | make a nullmap for params without offset.
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
    go (_        :vs) byte pos = let pos' = pos + 1 in pos' `seq` go vs byte pos'

--------------------------------------------------------------------------------
-- TODO: add helpers to parse MYSQL_TYPE_GEOMETRY
-- reference: https://github.com/felixge/node-mysql/blob/master/lib/protocol/Parser.js
