{-# LANGUAGE BangPatterns #-}

{-|
Module      : Database.MySQL.BinLogProtocol.BinLogValue
Description : Binlog protocol
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Binlog protocol

-}

module Database.MySQL.BinLogProtocol.BinLogValue where

import           Control.Applicative
import           Data.Bits
import           Data.Int
import           Data.Int.Int24
import           Data.Scientific
import           Data.Word
import           Database.MySQL.BinLogProtocol.BinLogMeta
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet
import           GHC.Generics                             (Generic)
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V

-- | Data type for representing binlog values.
--
-- This data type DOES NOT try to parse binlog values into detailed haskell values,
-- because you may not want to waste performance in situations like database middleware.
--
-- Due to the lack of signedness infomation in binlog meta, we cannot distinguish,
-- for example, between unsigned tiny 255 and tiny -1, so we use int to present
-- @TINY,SHORT,INT,LONG@. If you have unsigned columns, use 'fromIntegral' to convert it
-- to word to get real unsigned value back, for example, @fromIntegral (-1 :: Int) == 255 :: Word@
--
-- For above reason, we use 'Int24' to present MySQL's @INT24@ type, you can get back the
-- unsigned value using @word24@ package's 'Word24' type.
--
-- Timestamp types('BinLogTimeStamp' and 'BinLogTimeStamp2') are values converted into UTC already,
-- see 'MySQLVaule' 's note.
--
-- There's also no infomation about charset, so we use 'V.Bytes' to present both text
-- and blob types, if you want to get text representation back, you have to query column charset
-- infomation, and use icu or iconv to decode. IT MAY NOT BE UTF-8.
--
-- The @SET@ and @ENUM@ values are presented by their index's value and bitmap respectively,
-- if you need get the string value back, you have to perform a 'DESC tablename' to get the
-- set or enum table.
--
data BinLogValue
    = BinLogTiny       !Int8
    | BinLogShort      !Int16
    | BinLogInt24      !Int24
    | BinLogLong       !Int32
    | BinLogLongLong   !Int64
    | BinLogFloat      !Float
    | BinLogDouble     !Double
    | BinLogBit        !Word64          -- ^ a 64bit bitmap.
    | BinLogTimeStamp  !Word32          -- ^ a utc timestamp, note 0 doesn't mean @1970-01-01 00:00:00@,
                                        -- because mysql choose 0 to present '0000-00-00 00:00:00'
    | BinLogTimeStamp2 !Word32 !Word32  -- ^ like 'BinLogTimeStamp' with an addtional microseconds field.
    | BinLogDateTime   !Word16 !Word8 !Word8 !Word8 !Word8 !Word8         -- ^ YYYY MM DD hh mm ss
    | BinLogDateTime2  !Word16 !Word8 !Word8 !Word8 !Word8 !Word8 !Word32 -- ^ YYYY MM DD hh mm ss microsecond
    | BinLogDate       !Word16 !Word8 !Word8                   -- ^ YYYY MM DD
    | BinLogTime       !Word8  !Word16 !Word8 !Word8           -- ^ sign(1= non-negative, 0= negative) hh mm ss
    | BinLogTime2      !Word8  !Word16 !Word8 !Word8 !Word32   -- ^ sign(1= non-negative, 0= negative) hh mm ss microsecond
    | BinLogYear       !Word16                                 -- ^ year value, 0 stand for '0000'
    | BinLogNewDecimal !Scientific                             -- ^ sign(1= non-negative, 0= negative) integeral part, fractional part
    | BinLogEnum       !Word16                                 -- ^ enum indexing value
    | BinLogSet        !Word64                                 -- ^ set indexing 64bit bitmap.
    | BinLogBytes      !V.Bytes                                -- ^ all string and blob values.
    | BinLogGeometry   !V.Bytes
    | BinLogNull
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- | BinLog protocol decoder
--
decodeBinLogField :: BinLogMeta -> P.Parser BinLogValue
decodeBinLogField BINLOG_TYPE_TINY                = BinLogTiny         <$> P.decodePrim @Int8
decodeBinLogField BINLOG_TYPE_SHORT               = BinLogShort        <$> P.decodePrimLE
decodeBinLogField BINLOG_TYPE_INT24               = BinLogInt24 . fromIntegral <$> decodeWord24LE
decodeBinLogField BINLOG_TYPE_LONG                = BinLogLong         <$> P.decodePrimLE
decodeBinLogField BINLOG_TYPE_LONGLONG            = BinLogLongLong     <$> P.decodePrimLE
decodeBinLogField (BINLOG_TYPE_FLOAT  _         ) = BinLogFloat        <$> P.decodePrimLE
decodeBinLogField (BINLOG_TYPE_DOUBLE _         ) = BinLogDouble       <$> P.decodePrimLE
decodeBinLogField (BINLOG_TYPE_BIT    _    bytes) = BinLogBit          <$> decodeBits' bytes
decodeBinLogField BINLOG_TYPE_TIMESTAMP           = BinLogTimeStamp    <$> P.decodePrimLE

-- A integer in @YYYYMMDD@ format, for example:
-- 99991231 stand for @9999-12-31@
decodeBinLogField BINLOG_TYPE_DATE = do
    i <- decodeWord24LE
    let (i', dd) = i `quotRem` 32
        (yyyy, mm) = i' `quotRem` 16
    pure (BinLogDate (fromIntegral yyyy)
                     (fromIntegral mm)
                     (fromIntegral dd))

decodeBinLogField (BINLOG_TYPE_TIMESTAMP2  fsp) = do
    s <- P.decodePrimBE @Word32 -- big-endian here!
    ms <- fromIntegral <$> decodeMicroSecond fsp
    pure (BinLogTimeStamp2 s ms)

-- A integer in @YYYYMMDDhhmmss@, for example:
-- 99991231235959 stand for @9999-12-31 23:59:59@
decodeBinLogField BINLOG_TYPE_DATETIME = do
    i <- P.decodePrimLE @Word64
    let (yyyy, i')   = i      `quotRem` 10000000000
        (mm, i'')    = i'     `quotRem` 100000000
        (dd, i''')   = i''    `quotRem` 1000000
        (h, i'''')   = i'''   `quotRem` 10000
        (m, s)       = i''''  `quotRem` 100
    pure (BinLogDateTime (fromIntegral yyyy)
                         (fromIntegral mm)
                         (fromIntegral dd)
                         (fromIntegral h)
                         (fromIntegral m)
                         (fromIntegral s))

-- BINLOG_TYPE_DATETIME2(big endian)
--
-- 1 bit sign (used when on disk)
-- 17 bits year * 13 + month (year 0-9999, month 0-12)
-- 5 bits day (0-31)
-- 5 bits hour (0-23)
-- 6 bits minute (0-59)
-- 6 bits second (0-59)
-- (5 bytes in total)
--
-- fractional-seconds storage (size depends on meta)
--
decodeBinLogField (BINLOG_TYPE_DATETIME2 fsp) = do
    iPart <- decodeWord40BE
    let yyyymm = iPart `shiftR` 22 .&. 0x01FFFF -- 0b011111111111111111
        (yyyy, mm) = yyyymm `quotRem` 13
        yyyy' = fromIntegral yyyy
        mm' = fromIntegral mm
        dd = fromIntegral $ iPart `shiftR` 17 .&. 0x1F -- 0b00011111
        h =  fromIntegral $ iPart `shiftR` 12 .&. 0x1F -- 0b00011111
        m =  fromIntegral $ iPart `shiftR` 6 .&. 0x3F  -- 0b00111111
        s =  fromIntegral $ iPart .&. 0x3F             -- 0b00111111
    ms <- fromIntegral <$> decodeMicroSecond fsp
    pure (BinLogDateTime2 yyyy' mm' dd h m s ms)

-- A integer in @hhmmss@ format(can be negative), for example:
-- 8385959 stand for @838:59:59@
decodeBinLogField BINLOG_TYPE_TIME = do
    i <- decodeWord24LE
    let i' =  fromIntegral i :: Int24
        sign = if i' >= 0 then 1 else 0
    let (h, i'')     = i'     `quotRem` 10000
        (m, s)       = i''    `quotRem` 100
    pure (BinLogTime sign (fromIntegral (abs h))
                          (fromIntegral (abs m))
                          (fromIntegral (abs s)))

-- BINLOG_TYPE_TIME2(big endian)
--
-- 1 bit sign  (1= non-negative, 0= negative)
-- 1 bit unused (Reserved for wider hour range, e.g. for intervals)
-- 10 bit hour (0-836)
-- 6 bit minute (0-59)
-- 6 bit second (0-59)
-- (3 bytes in total)
--
-- fractional-seconds storage (size depends on meta)
--
decodeBinLogField (BINLOG_TYPE_TIME2 fsp) = do
    iPart <- decodeWord24BE
    let sign = fromIntegral $ iPart `shiftR` 23
        iPart' = if sign == 0 then 0x800000 - iPart - 1 else iPart
        h = fromIntegral (iPart' `shiftR` 12) .&. 0x03FF -- 0b0000001111111111
        m = fromIntegral (iPart' `shiftR` 6) .&. 0x3F    -- 0b00111111
        s = fromIntegral iPart' .&. 0x3F               -- 0b00111111
    ms <- abs <$> decodeMicroSecond fsp
    let ms' = abs (fromIntegral ms :: Int)
    pure (BinLogTime2 sign h m s (fromIntegral ms'))

decodeBinLogField BINLOG_TYPE_YEAR                = do
    y <- P.anyWord8
    pure $! if y == 0 then BinLogYear 0 else BinLogYear (1900 + fromIntegral y)

-- Decimal representation in binlog seems to be as follows:
--
-- 1st bit - sign such that set == +, unset == -
-- every 4 bytes represent 9 digits in big-endian order.
--
-- 80 00 00 05 1b 38 b0 60 00 means:
--
--   0x80 - positive
--   0x00000005 - 5
--   0x1b38b060 - 456700000
--   0x00       - 0
--
-- 54567000000 / 10^{10} = 5.4567
--
-- if there're < 9 digits at first, it will be compressed into suitable length words
-- following a simple lookup table.
--
decodeBinLogField (BINLOG_TYPE_NEWDECIMAL precision scale) = do
    let i = fromIntegral (precision - scale)
        (ucI, cI) = i `quotRem` digitsPerInteger
        (ucF, cF) = fromIntegral scale `quotRem` digitsPerInteger
        ucISize = (ucI `shiftL` 2)
        ucFSize = (ucF `shiftL` 2)
        cISize = (sizeTable `V.unsafeIndex` cI)
        cFSize = (sizeTable `V.unsafeIndex` cF)
        len = ucISize + cISize + ucFSize + cFSize

    buf <- P.take (fromIntegral len)

    let fb = buf `V.unsafeIndex` 0
        sign = if fb .&. 0x80 == 0x80 then 1 else 0 :: Word8
        buf' = (fb `xor` 0x80) `V.cons` V.tail buf
        buf'' = if sign == 1 then buf'
                            else V.map (xor 0xFF) buf'

        iPart = fromIntegral (decodeCompressed cISize (V.unsafeTake cISize buf'')) * (blockSize ^ ucI)
              + decodeUncompressed ucI (V.unsafeDrop cISize buf'')

    let buf''' = V.unsafeDrop (ucISize + cISize) buf''

        fPart = decodeUncompressed ucF (V.unsafeTake ucFSize buf''') * (10 ^ cF)
              + fromIntegral (decodeCompressed cFSize (V.unsafeDrop ucFSize buf'''))

    let sci = scientific (iPart * 10 ^ scale + fPart) (negate $ fromIntegral scale)
        sci' = if sign == 0 then negate sci else sci
    pure (BinLogNewDecimal sci')
  where
    digitsPerInteger = 9
    blockSize = fromIntegral $ (10 :: Int32) ^ (9 :: Int)
    sizeTable :: V.PrimVector Int
    sizeTable = V.pack [0, 1, 1, 2, 2, 3, 3, 4, 4, 4]

    decodeCompressed :: Int -> V.Bytes -> Word64
    decodeCompressed 0 _  = 0
    decodeCompressed x bs = let !fb = bs `V.unsafeIndex` 0
                                !x' = x - 1
                            in fromIntegral fb `shiftL` (8 * x') .|. decodeCompressed x' (V.unsafeDrop 1 bs)

    decodeUncompressed :: Int -> V.Bytes -> Integer
    decodeUncompressed 0 _ = 0
    decodeUncompressed x bs = let !v = decodeCompressed 4 (V.unsafeTake 4 bs)
                                  !x' = x - 1
                              in fromIntegral v * (blockSize ^ x') + decodeUncompressed x' (V.unsafeDrop 4 bs)


decodeBinLogField (BINLOG_TYPE_ENUM size) =
    if  | size == 1 -> BinLogEnum . fromIntegral <$> P.anyWord8
        | size == 2 -> BinLogEnum . fromIntegral <$> P.decodePrimLE @Word16
        | otherwise -> P.fail' $ "Database.MySQL.BinLogProtocol.BinLogValue: wrong \
                              \BINLOG_TYPE_ENUM size: " <> T.toText size


decodeBinLogField (BINLOG_TYPE_SET _ bytes) = BinLogSet <$> decodeBits' bytes
decodeBinLogField (BINLOG_TYPE_BLOB lensize) = do
    len <- if  | lensize == 1 -> fromIntegral <$> P.anyWord8
               | lensize == 2 -> fromIntegral <$> P.decodePrimLE @Word16
               | lensize == 3 -> fromIntegral <$> decodeWord24LE
               | lensize == 4 -> fromIntegral <$> P.decodePrimLE @Word32
               | otherwise    -> P.fail' $ "Database.MySQL.BinLogProtocol.BinLogValue: \
                                        \wrong BINLOG_TYPE_BLOB length size: " <> T.toText lensize
    BinLogBytes <$> P.take len

decodeBinLogField (BINLOG_TYPE_STRING size) = do
    len <- if | size < 256 -> fromIntegral <$> P.anyWord8
              | otherwise  -> fromIntegral <$> P.decodePrimLE @Word16
    BinLogBytes <$> P.take len

decodeBinLogField (BINLOG_TYPE_GEOMETRY lensize) = do
    len <- if | lensize == 1 -> fromIntegral <$> P.anyWord8
              | lensize == 2 -> fromIntegral <$> P.decodePrimLE @Word16
              | lensize == 3 -> fromIntegral <$> decodeWord24LE
              | lensize == 4 -> fromIntegral <$> P.decodePrimLE @Word32
              | otherwise    -> P.fail' $  "Database.MySQL.BinLogProtocol.BinLogValue: \
                                        \wrong BINLOG_TYPE_GEOMETRY length size: " <> T.toText lensize
    BinLogGeometry <$> P.take len

decodeMicroSecond :: Word8 -> P.Parser Int32
decodeMicroSecond 0 = pure 0
decodeMicroSecond 1 = (* 100000) . fromIntegral <$> P.decodePrim @Int8
decodeMicroSecond 2 = (* 10000) . fromIntegral  <$> P.decodePrim @Int8
decodeMicroSecond 3 = (* 1000) . fromIntegral   <$> P.decodePrimBE @Int16
decodeMicroSecond 4 = (* 100) . fromIntegral    <$> P.decodePrimBE @Int16
decodeMicroSecond 5 = (* 10) . fromIntegral     <$> decodeInt24BE
decodeMicroSecond 6 = fromIntegral              <$> decodeInt24BE
decodeMicroSecond _ = pure 0

decodeBits' :: Word8 -> P.Parser Word64
decodeBits' bytes = if bytes <= 8
    then decodeBits (fromIntegral bytes)
    else P.fail' $  "Database.MySQL.BinLogProtocol.BinLogValue: wrong bit length size: " <> T.toText bytes

--------------------------------------------------------------------------------
-- | BinLog row decoder
--
decodeBinLogRow :: V.Vector BinLogMeta -> BitMap -> P.Parser [BinLogValue]
decodeBinLogRow metas pmap = do
    let plen = V.foldl' (\acc word8 -> acc + popCount word8) 0 (fromBitMap pmap)
        maplen = (plen + 7) `shiftR` 3
    nullmap <- P.take maplen
    go (BitMap nullmap) (V.length metas) 0 0
  where
    go :: BitMap -> Int -> Int -> Int -> P.Parser [BinLogValue]
    go nullmap end !nullpos !ppos
        | ppos >= end = pure []
        | otherwise = do
            let f = metas `V.unsafeIndex` ppos
                ppos' = ppos + 1
            if isColumnSet pmap ppos
            then do
                r <- if isColumnSet nullmap nullpos
                        then return BinLogNull
                        else decodeBinLogField f
                let nullpos' = nullpos + 1
                rest <- go nullmap end nullpos' ppos'
                return (rest `seq` (r : rest))
            else go nullmap end nullpos ppos'

