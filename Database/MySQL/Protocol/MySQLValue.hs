{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
  , encodeParamMySQLType
  , decodeTextField
  , encodeTextField
  , decodeTextRow
  , decodeBinaryField
  , encodeBinaryField
  , decodeBinaryRow
  -- * Internal utilities
  , decodeBits
  , BitMap(..)
  , isColumnSet
  , isColumnNull
  , makeNullMap
  ) where

import           Control.Monad
import           Control.DeepSeq
import           Data.Bits
import           Data.Fixed                        (Pico)
import           Data.Int
import           Data.Scientific                   (Scientific)
import           Data.Time.Calendar                (Day, fromGregorian,
                                                    toGregorian)
import           Data.Time.Format                  (defaultTimeLocale,
                                                    formatTime)
import           Data.Time.LocalTime               (LocalTime (..),
                                                    TimeOfDay (..))
import           Data.Word
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Escape
import           Database.MySQL.Protocol.Packet
import           GHC.Generics                      (Generic)
import qualified Z.Data.Builder                    as B
import qualified Z.Data.Parser                     as P
import qualified Z.Data.Parser.Time                as P
import qualified Z.Data.Text                       as T
import qualified Z.Data.Text.Base                  as T
import qualified Z.Data.Vector                     as V
import qualified Z.Data.Vector.Extra               as V
import           Z.IO

--------------------------------------------------------------------------------
-- | Data type mapping between MySQL values and haskell values.
--
-- There're some subtle differences between MySQL values and haskell values:
--
-- MySQL's @DATETIME@ and @TIMESTAMP@ are different on timezone handling:
--
--  * DATETIME and DATE is just a represent of a calendar date, it has no timezone information involved,
--  you always decode the same value as you put no matter what timezone you're using with MySQL.
--
--  * MySQL converts TIMESTAMP values from the current time zone to UTC for storage,
--  and back from UTC to the current time zone for retrieval. If you put a TIMESTAMP with timezone A,
--  then read it with timezone B, you may decode different result because of this conversion, so always
--  be careful about setting up the right timezone with MySQL, you can do it with a simple @SET time_zone = timezone;@
--  for more info on timezone support, please read <http://dev.mysql.com/doc/refman/5.7/en/time-zone-support.html>
--
--  So we use 'LocalTime' to present both @DATETIME@ and @TIMESTAMP@, but the local here is different.
--
-- MySQL's @TIME@ type can present time of day, but also elapsed time or a time interval between two events.
-- @TIME@ values may range from @-838:59:59@ to @838:59:59@, so 'MySQLTime' values consist of a sign and a
-- 'TimeOfDay' whose hour part may exceeded 24. you can use @timeOfDayToTime@ to decode the absolute time interval.
--
-- Under MySQL >= 5.7, @DATETIME@, @TIMESTAMP@ and @TIME@ may contain fractional part, which matches haskell's
-- precision.
--
data MySQLValue
    = MySQLDecimal       {-# UNPACK #-} !Scientific   -- ^ DECIMAL, NEWDECIMAL
    | MySQLInt8U         {-# UNPACK #-} !Word8        -- ^ Unsigned TINY
    | MySQLInt8          {-# UNPACK #-} !Int8         -- ^ TINY
    | MySQLInt16U        {-# UNPACK #-} !Word16       -- ^ Unsigned SHORT
    | MySQLInt16         {-# UNPACK #-} !Int16        -- ^ SHORT
    | MySQLInt32U        {-# UNPACK #-} !Word32       -- ^ Unsigned LONG, INT24
    | MySQLInt32         {-# UNPACK #-} !Int32        -- ^ LONG, INT24
    | MySQLInt64U        {-# UNPACK #-} !Word64       -- ^ Unsigned LONGLONG
    | MySQLInt64         {-# UNPACK #-} !Int64        -- ^ LONGLONG
    | MySQLFloat         {-# UNPACK #-} !Float        -- ^ IEEE 754 single precision format
    | MySQLDouble        {-# UNPACK #-} !Double       -- ^ IEEE 754 double precision format
    | MySQLYear          {-# UNPACK #-} !Word16       -- ^ YEAR
    | MySQLDateTime      {-# UNPACK #-} !LocalTime    -- ^ DATETIME
    | MySQLTimeStamp     {-# UNPACK #-} !LocalTime    -- ^ TIMESTAMP
    | MySQLDate                         !Day          -- ^ DATE
    | MySQLTime          {-# UNPACK #-} !Word8      -- ^ sign(0 = non-negative, 1 = negative), the sign is OPPOSITE to binlog one !!!
                         {-# UNPACK #-} !TimeOfDay  -- ^ hh mm ss microsecond
    | MySQLGeometry      {-# UNPACK #-} !V.Bytes       -- ^ todo: parsing to something meanful
    | MySQLBytes         {-# UNPACK #-} !V.Bytes
    | MySQLBit           {-# UNPACK #-} !Word64
    | MySQLText          {-# UNPACK #-} !T.Text
    | MySQLNull
  deriving (Show, Eq, Generic, NFData)

-- | Put 'FieldType' and usigned bit(0x80/0x00) for 'MySQLValue's.
--
encodeParamMySQLType :: MySQLValue -> B.Builder ()
encodeParamMySQLType (MySQLDecimal      _)  = B.encodePrim (MySQLTypeDecimal  , 0x00::Word8)
encodeParamMySQLType (MySQLInt8U        _)  = B.encodePrim (MySQLTypeTiny     , 0x80::Word8)
encodeParamMySQLType (MySQLInt8         _)  = B.encodePrim (MySQLTypeTiny     , 0x00::Word8)
encodeParamMySQLType (MySQLInt16U       _)  = B.encodePrim (MySQLTypeShort    , 0x80::Word8)
encodeParamMySQLType (MySQLInt16        _)  = B.encodePrim (MySQLTypeShort    , 0x00::Word8)
encodeParamMySQLType (MySQLInt32U       _)  = B.encodePrim (MySQLTypeLong     , 0x80::Word8)
encodeParamMySQLType (MySQLInt32        _)  = B.encodePrim (MySQLTypeLong     , 0x00::Word8)
encodeParamMySQLType (MySQLInt64U       _)  = B.encodePrim (MySQLTypeLongLong , 0x80::Word8)
encodeParamMySQLType (MySQLInt64        _)  = B.encodePrim (MySQLTypeLongLong , 0x00::Word8)
encodeParamMySQLType (MySQLFloat        _)  = B.encodePrim (MySQLTypeFloat    , 0x00::Word8)
encodeParamMySQLType (MySQLDouble       _)  = B.encodePrim (MySQLTypeDouble   , 0x00::Word8)
encodeParamMySQLType (MySQLYear         _)  = B.encodePrim (MySQLTypeYear     , 0x80::Word8)
encodeParamMySQLType (MySQLDateTime     _)  = B.encodePrim (MySQLTypeDateTime , 0x00::Word8)
encodeParamMySQLType (MySQLTimeStamp    _)  = B.encodePrim (MySQLTypeTimestamp, 0x00::Word8)
encodeParamMySQLType (MySQLDate         _)  = B.encodePrim (MySQLTypeDate     , 0x00::Word8)
encodeParamMySQLType (MySQLTime       _ _)  = B.encodePrim (MySQLTypeTime     , 0x00::Word8)
encodeParamMySQLType (MySQLBytes        _)  = B.encodePrim (MySQLTypeBlob     , 0x00::Word8)
encodeParamMySQLType (MySQLGeometry     _)  = B.encodePrim (MySQLTypeGeometry , 0x00::Word8)
encodeParamMySQLType (MySQLBit          _)  = B.encodePrim (MySQLTypeBit      , 0x00::Word8)
encodeParamMySQLType (MySQLText         _)  = B.encodePrim (MySQLTypeString   , 0x00::Word8)
encodeParamMySQLType MySQLNull              = B.encodePrim (MySQLTypeNull     , 0x00::Word8)

--------------------------------------------------------------------------------
-- | Text protocol decoder
decodeTextField :: ColumnDef -> P.Parser MySQLValue
{-# INLINE decodeTextField #-}
decodeTextField f
    | t == MySQLTypeNull            = pure MySQLNull
    | t == MySQLTypeDecimal
        || t == MySQLTypeNewDecimal = feedLenEncBytes t MySQLDecimal P.scientific'
    | t == MySQLTypeTiny            = if isUnsigned then feedLenEncBytes t MySQLInt8U P.uint
                                                    else feedLenEncBytes t MySQLInt8 P.int
    | t == MySQLTypeShort           = if isUnsigned then feedLenEncBytes t MySQLInt16U P.uint
                                                    else feedLenEncBytes t MySQLInt16 P.int
    | t == MySQLTypeLong
        || t == MySQLTypeInt24      = if isUnsigned then feedLenEncBytes t MySQLInt32U P.uint
                                                    else feedLenEncBytes t MySQLInt32 P.int
    | t == MySQLTypeLongLong        = if isUnsigned then feedLenEncBytes t MySQLInt64U P.uint
                                                    else feedLenEncBytes t MySQLInt64 P.int
    | t == MySQLTypeFloat           = feedLenEncBytes t MySQLFloat P.float'
    | t == MySQLTypeDouble          = feedLenEncBytes t MySQLDouble P.double'
    | t == MySQLTypeYear            = feedLenEncBytes t MySQLYear P.int
    | t == MySQLTypeTimestamp
        || t == MySQLTypeTimestamp2 = feedLenEncBytes t MySQLTimeStamp P.localTime
    | t == MySQLTypeDateTime
        || t == MySQLTypeDateTime2  = feedLenEncBytes t MySQLDateTime P.localTime
    | t == MySQLTypeDate
        || t == MySQLTypeNewDate    = feedLenEncBytes t MySQLDate P.day
    | t == MySQLTypeTime
        || t == MySQLTypeTime2      = feedLenEncBytes t id $ do
                                          sign <- P.peek
                                          if sign == 45  -- '-'
                                          then P.skipWord8 >> (MySQLTime 1 <$> P.timeOfDay)
                                          else MySQLTime 0 <$> P.timeOfDay

    | t == MySQLTypeGeometry        = MySQLGeometry <$> decodeLenEncBytes
    | t == MySQLTypeVarChar
        || t == MySQLTypeEnum
        || t == MySQLTypeSet
        || t == MySQLTypeTinyBlob
        || t == MySQLTypeMediumBlob
        || t == MySQLTypeLongBlob
        || t == MySQLTypeBlob
        || t == MySQLTypeVarString
        || t == MySQLTypeString     =  do bs <- decodeLenEncBytes
                                          if isText
                                          then pure (MySQLText (T.validate bs))
                                          else pure (MySQLBytes bs)

    | t == MySQLTypeBit             = MySQLBit <$> (decodeBits =<< decodeLenEncInt)

    | otherwise                     = P.fail' $ "Database.MySQL.Protocol.MySQLValue: missing text decoder for " <> T.toText t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63

feedLenEncBytes :: FieldType -> (t -> b) -> P.Parser t -> P.Parser b
{-# INLINE feedLenEncBytes #-}
feedLenEncBytes typ con p = do
    bs <- decodeLenEncBytes
    case P.parse' p bs of
        Right v -> return (con v)
        Left e -> P.fail' $ T.concat ["Database.MySQL.Protocol.MySQLValue: parsing ", T.toText typ, " failed, ", T.toText e]

--------------------------------------------------------------------------------
-- | Text protocol encoder
encodeTextField :: MySQLValue -> B.Builder ()
encodeTextField (MySQLDecimal    n) = B.scientific n
encodeTextField (MySQLInt8U      n) = B.int n
encodeTextField (MySQLInt8       n) = B.int n
encodeTextField (MySQLInt16U     n) = B.int n
encodeTextField (MySQLInt16      n) = B.int n
encodeTextField (MySQLInt32U     n) = B.int n
encodeTextField (MySQLInt32      n) = B.int n
encodeTextField (MySQLInt64U     n) = B.int n
encodeTextField (MySQLInt64      n) = B.int n
encodeTextField (MySQLFloat      x) = B.float x
encodeTextField (MySQLDouble     x) = B.double x
encodeTextField (MySQLYear       n) = B.int n
encodeTextField (MySQLDateTime  dt) = B.squotes $ B.localTime dt
encodeTextField (MySQLTimeStamp dt) = B.squotes $ B.localTime dt
encodeTextField (MySQLDate       d) = B.squotes $ B.day d
encodeTextField (MySQLTime  sign t) = B.squotes $ do
                                          when (sign == 1) (B.char8 '-')
                                          B.string8 (formatTime defaultTimeLocale "%T%Q" t)
                                      -- this works even for hour > 24
encodeTextField (MySQLGeometry  bs) = B.squotes $ escapeBytes $ bs
encodeTextField (MySQLBytes     bs) = B.squotes $ escapeBytes $ bs
encodeTextField (MySQLText       t) = B.squotes $ escapeText $ t
encodeTextField (MySQLBit        b) = do
                                      "b\'"
                                      encodeTextBits b
                                      B.char8 '\''
  where
    encodeTextBits :: Word64 -> B.Builder ()
    encodeTextBits word = forM_ [63,62..0] $ \ pos ->
            if word `testBit` pos then B.char8 '1' else B.char8 '0'
    {-# INLINE encodeTextBits #-}

encodeTextField MySQLNull           = "NULL"

--------------------------------------------------------------------------------
-- | Text row decoder

decodeTextRow :: V.Vector ColumnDef -> P.Parser (V.Vector MySQLValue)
{-# INLINABLE decodeTextRow #-}
decodeTextRow fs = (`V.traverse` fs) $ \ f -> do
    p <- P.peek
    if p == 0xFB
    then P.skipWord8 >> return MySQLNull
    else decodeTextField f

--------------------------------------------------------------------------------
-- | Binary protocol decoder
decodeBinaryField :: ColumnDef -> P.Parser MySQLValue
{-# INLINE decodeBinaryField #-}
decodeBinaryField f
    | t == MySQLTypeNull              = pure MySQLNull
    | t == MySQLTypeDecimal
        || t == MySQLTypeNewDecimal   = feedLenEncBytes t MySQLDecimal P.scientific'
    | t == MySQLTypeTiny              = if isUnsigned then MySQLInt8U <$> P.decodePrim
                                                      else MySQLInt8  <$> P.decodePrim
    | t == MySQLTypeShort             = if isUnsigned then MySQLInt16U <$> P.decodePrimLE
                                                      else MySQLInt16  <$> P.decodePrimLE
    | t == MySQLTypeLong
        || t == MySQLTypeInt24        = if isUnsigned then MySQLInt32U <$> P.decodePrimLE
                                                      else MySQLInt32  <$> P.decodePrimLE
    | t == MySQLTypeYear              = MySQLYear <$> P.decodePrimLE
    | t == MySQLTypeLongLong          = if isUnsigned then MySQLInt64U <$> P.decodePrimLE
                                                      else MySQLInt64  <$> P.decodePrimLE
    | t == MySQLTypeFloat             = MySQLFloat  <$> P.decodePrimLE
    | t == MySQLTypeDouble            = MySQLDouble <$> P.decodePrimLE
    | t == MySQLTypeTimestamp
        || t == MySQLTypeTimestamp2   = do
            n <- decodeLenEncInt
            case n of
               0 -> pure $ MySQLTimeStamp (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
               4 -> do
                   d <- decodeDay
                   pure $ MySQLTimeStamp (LocalTime d (TimeOfDay 0 0 0))
               7 -> do
                   d <- decodeDay
                   td <- decodeTimeOfDay
                   pure $ MySQLTimeStamp (LocalTime d td)
               11 -> do
                   d <- decodeDay
                   td <- decodeTimeOfDay'
                   pure $ MySQLTimeStamp (LocalTime d td)
               _ -> P.fail' "Database.MySQL.Protocol.MySQLValue: wrong TIMESTAMP length"
    | t == MySQLTypeDateTime
        || t == MySQLTypeDateTime2    = do
            n <- decodeLenEncInt
            case n of
               0 -> pure $ MySQLDateTime (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
               4 -> do
                   d <- decodeDay
                   pure $ MySQLDateTime (LocalTime d (TimeOfDay 0 0 0))
               7 -> do
                   d <- decodeDay
                   td <- decodeTimeOfDay
                   pure $ MySQLDateTime (LocalTime d td)
               11 -> do
                   d <- decodeDay
                   td <- decodeTimeOfDay'
                   pure $ MySQLDateTime (LocalTime d td)
               _ -> P.fail' "Database.MySQL.Protocol.MySQLValue: wrong DATETIME length"

    | t == MySQLTypeDate
        || t == MySQLTypeNewDate      = do
            n <- decodeLenEncInt
            case n of
               0 -> pure $ MySQLDate (fromGregorian 0 0 0)
               4 -> MySQLDate <$> decodeDay
               _ -> P.fail' "Database.MySQL.Protocol.MySQLValue: wrong DATE length"

    | t == MySQLTypeTime
        || t == MySQLTypeTime2        = do
            n <- decodeLenEncInt
            case n of
               0 -> pure $ MySQLTime 0 (TimeOfDay 0 0 0)
               8 -> do
                   sign <- P.anyWord8   -- is_negative(1 if minus, 0 for plus)
                   d <- fromIntegral <$> P.decodePrimLE @Word32
                   h <-  decodeInt8
                   MySQLTime sign <$> (TimeOfDay (d*24 + h) <$> decodeInt8 <*> decodeSecond4)

               12 -> do
                   sign <- P.anyWord8   -- is_negative(1 if minus, 0 for plus)
                   d <- fromIntegral <$> P.decodePrimLE @Word32
                   h <-  decodeInt8
                   MySQLTime sign <$> (TimeOfDay (d*24 + h) <$> decodeInt8 <*> decodeSecond8)
               _ -> P.fail' "Database.MySQL.Protocol.MySQLValue: wrong TIME length"

    | t == MySQLTypeGeometry          = MySQLGeometry <$> decodeLenEncBytes
    | t == MySQLTypeVarChar
        || t == MySQLTypeEnum
        || t == MySQLTypeSet
        || t == MySQLTypeTinyBlob
        || t == MySQLTypeMediumBlob
        || t == MySQLTypeLongBlob
        || t == MySQLTypeBlob
        || t == MySQLTypeVarString
        || t == MySQLTypeString       = do
            bs <- decodeLenEncBytes
            if isText
            then pure (MySQLText (T.validate bs))
            else pure (MySQLBytes bs)
    | t == MySQLTypeBit               = MySQLBit <$> (decodeBits =<< decodeLenEncInt)
    | otherwise                       = P.fail' $ "Database.MySQL.Protocol.MySQLValue: missing binary decoder for type tag: " <> T.toText t
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63
    decodeDay :: P.Parser Day
    decodeDay = do
        y <- decodeYear
        m <- decodeInt8
        d <- decodeInt8
        case P.fromGregorianValidInt64 y m d of
            Just d -> pure d
            _ -> P.fail' $ T.concat ["Database.MySQL.Protocol.MySQLValue: invalid date: ", T.toText y, "-", T.toText m, "-", T.toText d]
    decodeTimeOfDay :: P.Parser TimeOfDay
    decodeTimeOfDay = TimeOfDay <$> decodeInt8 <*> decodeInt8 <*> decodeSecond4
    decodeTimeOfDay' :: P.Parser TimeOfDay
    decodeTimeOfDay' = TimeOfDay <$> decodeInt8 <*> decodeInt8 <*> decodeSecond8
    decodeYear :: P.Parser Int64
    decodeYear = fromIntegral <$> P.decodeWord16LE
    decodeInt8 :: P.Parser Int
    decodeInt8 = fromIntegral <$> P.anyWord8
    decodeSecond4 :: P.Parser Pico
    decodeSecond4 = realToFrac <$> P.anyWord8
    decodeSecond8 :: P.Parser Pico
    decodeSecond8 = realToFrac <$> do
        s <- decodeInt8
        (ms :: Int) <- fromIntegral <$> P.decodePrimLE @Word32
        pure $! (realToFrac s + realToFrac ms / 1000000 :: Pico)


-- | decode a bit sequence as a Word64
--
-- Since 'Word64' has a @Bits@ instance, it's easier to deal with in Haskell.
--
decodeBits :: Int -> P.Parser Word64
decodeBits bytes =
    if  | bytes == 0 || bytes == 1 -> fromIntegral <$> P.anyWord8
        | bytes == 2 -> fromIntegral <$> P.decodePrimBE @Word16
        | bytes == 3 -> fromIntegral <$> decodeWord24BE
        | bytes == 4 -> fromIntegral <$> P.decodePrimBE @Word32
        | bytes == 5 -> fromIntegral <$> decodeWord40BE
        | bytes == 6 -> fromIntegral <$> decodeWord48BE
        | bytes == 7 -> fromIntegral <$> decodeWord56BE
        | bytes == 8 -> fromIntegral <$> P.decodePrimBE @Word64
        | otherwise  -> P.fail' $  "Database.MySQL.Protocol.MySQLValue: \
                                \wrong bit length size: " <> T.toText bytes
{-# INLINE decodeBits #-}


--------------------------------------------------------------------------------
-- | Binary protocol encoder
encodeBinaryField :: MySQLValue -> B.Builder ()
encodeBinaryField (MySQLDecimal    n) = encodeLenEncBytes $ B.build (B.scientific n)
encodeBinaryField (MySQLInt8U      n) = B.word8 n
encodeBinaryField (MySQLInt8       n) = B.word8 (fromIntegral n)
encodeBinaryField (MySQLInt16U     n) = B.encodePrimLE n
encodeBinaryField (MySQLInt16      n) = B.encodePrimLE n
encodeBinaryField (MySQLInt32U     n) = B.encodePrimLE n
encodeBinaryField (MySQLInt32      n) = B.encodePrimLE n
encodeBinaryField (MySQLInt64U     n) = B.encodePrimLE n
encodeBinaryField (MySQLInt64      n) = B.encodePrimLE n
encodeBinaryField (MySQLFloat      x) = B.encodePrimLE x
encodeBinaryField (MySQLDouble     x) = B.encodePrimLE x
encodeBinaryField (MySQLYear       n) = encodeLenEncBytes $ B.build (B.int n)
                                                            -- this's really weird, it's not documented anywhere
                                                            -- we must encode year into string in binary mode!
encodeBinaryField (MySQLTimeStamp (LocalTime date time)) = do B.word8 11    -- always encode full
                                                              encodeBinaryDay date
                                                              encodeBinaryTime' time
encodeBinaryField (MySQLDateTime  (LocalTime date time)) = do B.word8 11    -- always encode full
                                                              encodeBinaryDay date
                                                              encodeBinaryTime' time
encodeBinaryField (MySQLDate    d)    = do B.word8 4
                                           encodeBinaryDay d
encodeBinaryField (MySQLTime sign t)  = do B.word8 12    -- always encode full
                                           B.word8 sign
                                           encodeBinaryTime t
encodeBinaryField (MySQLGeometry bs)  = encodeLenEncBytes bs
encodeBinaryField (MySQLBytes  bs)    = encodeLenEncBytes bs
encodeBinaryField (MySQLBit    word)  = do B.word8 8     -- always encode full
                                           B.encodePrimBE word
encodeBinaryField (MySQLText    t)    = encodeLenEncBytes (T.getUTF8Bytes t)
encodeBinaryField MySQLNull           = return ()

encodeBinaryDay :: Day -> B.Builder ()
encodeBinaryDay d = do
    let (yyyy, mm, dd) = toGregorian d
    B.encodePrimLE @Word16 (fromIntegral yyyy)
    B.word8 (fromIntegral mm)
    B.word8 (fromIntegral dd)
{-# INLINE encodeBinaryDay #-}

encodeBinaryTime' :: TimeOfDay -> B.Builder ()
encodeBinaryTime' (TimeOfDay hh mm ss) = do
    let s = floor ss
        ms = floor $ (ss - realToFrac s) * 1000000
    B.word8 (fromIntegral hh)
    B.word8 (fromIntegral mm)
    B.word8 s
    B.encodePrimLE @Word32 ms
{-# INLINE encodeBinaryTime' #-}

encodeBinaryTime :: TimeOfDay -> B.Builder ()
encodeBinaryTime (TimeOfDay hh mm ss) = do
    let s = floor ss
        ms = floor $ (ss - realToFrac s) * 1000000
        (d, h) = hh `quotRem` 24  -- hour may exceed 24 here
    B.encodePrimLE @Word32 (fromIntegral d)
    B.word8 (fromIntegral h)
    B.word8 (fromIntegral mm)
    B.word8 s
    B.encodePrimLE @Word32 ms
{-# INLINE encodeBinaryTime #-}

--------------------------------------------------------------------------------
-- | Binary row decoder
--
-- MySQL use a special null bitmap without offset = 2 here.

decodeBinaryRow :: V.Vector ColumnDef -> Int -> P.Parser (V.Vector MySQLValue)
decodeBinaryRow fields flen = do
    P.skipWord8            -- 0x00
    let !maplen = (flen + 7 + 2) `unsafeShiftR` 3
    !nullmap <- BitMap <$> P.take maplen
    (`V.traverseWithIndex` fields) $ \ !pos !f ->
        if isColumnNull nullmap pos then return MySQLNull else decodeBinaryField f
{-# INLINABLE decodeBinaryRow #-}

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
newtype BitMap = BitMap { fromBitMap :: V.Bytes } deriving (Eq, Show, Generic, T.Print)

-- | Test if a column is set(binlog protocol).
--
-- The number counts from left to right.
--
isColumnSet :: BitMap -> Int -> Bool
{-# INLINE isColumnSet #-}
isColumnSet (BitMap bitmap) pos =
  let i = pos `unsafeShiftR` 3
      j = pos .&. 7
  in (bitmap `V.unsafeIndex` i) `testBit` j

-- | Test if a column is null(binary protocol).
--
-- The number counts from left to right.
--
isColumnNull :: BitMap -> Int -> Bool
{-# INLINE isColumnNull #-}
isColumnNull (BitMap nullmap) pos =
  let
    pos' = pos + 2
    i    = pos' `unsafeShiftR` 3
    j    = pos' .&. 7
  in (nullmap `V.unsafeIndex` i) `testBit` j

-- | Make a nullmap for params(binary protocol) without offset.
--
makeNullMap :: [MySQLValue] -> BitMap
{-# INLINE makeNullMap #-}
makeNullMap values = BitMap . V.pack $ go values 0x00 0
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
-- TODO: add helpers to parse MySQLTypeGEOMETRY
-- reference: https://github.com/felixge/node-mysql/blob/master/lib/protocol/Parser.js
