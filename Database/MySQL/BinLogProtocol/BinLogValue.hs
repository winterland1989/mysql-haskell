{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Database.MySQL.Protocol.MySQLValue
Description : Text and binary protocol
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide both text and binary row decoder/encoder machinery.

-}


module Database.MySQL.BinLogProtocol.BinLogValue where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Bits
import Data.Word
import Data.Int
import Data.Binary.Get
import Data.Binary.Put
import Database.MySQL.BinLogProtocol.BinLogMeta
import Database.MySQL.Protocol.Packet
import Database.MySQL.Protocol.MySQLValue (isColumnSet, BitMap(..))
import Debug.Trace

-- | This data type DOES NOT try to parse binlog values into detailed haskell values.
-- Because you may not want to waste performance in situations like database middleware.
--
data BinLogValue
    = BinLogTiny       !Word8
    | BinLogShort      !Word16
    | BinLogInt24      !Word32
    | BinLogLong       !Word32
    | BinLogLongLong   !Word64
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
    | BinLogNewDecimal !Word8  !Integer !Integer               -- ^ sign(1= non-negative, 0= negative) integeral part, fractional part
    | BinLogEnum       !Word16                                 -- ^ enum indexing value
    | BinLogSet        !Word64                                 -- ^ set indexing 64bit bitmap.
    | BinLogBlob       !ByteString
    | BinLogString     !ByteString                             -- ^ no attempt to do charset decoding here
    | BinLogGeometry   !ByteString
    | BinLogNull
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | BinLog protocol decoder
--
getBinLogField :: BinLogMeta -> Get BinLogValue
getBinLogField BINLOG_TYPE_TINY                = BinLogTiny     <$> getWord8
getBinLogField BINLOG_TYPE_SHORT               = BinLogShort    <$> getWord16le
getBinLogField BINLOG_TYPE_INT24               = BinLogInt24    <$> getWord24le
getBinLogField BINLOG_TYPE_LONG                = BinLogLong     <$> getWord32le
getBinLogField BINLOG_TYPE_LONGLONG            = BinLogLongLong <$> getWord64le
getBinLogField (BINLOG_TYPE_FLOAT  _         ) = BinLogFloat <$> getFloatle
getBinLogField (BINLOG_TYPE_DOUBLE _         ) = BinLogDouble <$> getDoublele
getBinLogField (BINLOG_TYPE_BIT    bits bytes) = BinLogBit <$> getBits bits bytes
getBinLogField (BINLOG_TYPE_TIMESTAMP        ) = BinLogTimeStamp <$> getWord32le

-- a integer in @YYYYMMDDhhmmss@, for example:
-- 99991231235959 stand for @9999-12-31 23:59:59@
getBinLogField (BINLOG_TYPE_DATETIME         ) = do i <- getWord64le
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

-- ^ a integer in @YYYYMMDD@ format, for example:
-- 99991231 stand for @9999-12-31@
getBinLogField (BINLOG_TYPE_DATE             ) = do i <- getWord24le
                                                    let (yyyy, i')   = i      `quotRem` 10000
                                                        (mm, dd)     = i'     `quotRem` 100
                                                    pure (BinLogDate (fromIntegral yyyy)
                                                                     (fromIntegral mm)
                                                                     (fromIntegral dd))

-- ^ a integer in @hhmmss@ format(can be negative), for example:
-- 8385959 stand for @838:59:59@
getBinLogField (BINLOG_TYPE_TIME             ) = do i <- getWord24le
                                                    let i' =  fromIntegral i :: Int32
                                                        sign = if i' >= 0 then 1 else 0
                                                        ui = abs i
                                                    let (h, ui')     = ui     `quotRem` 10000
                                                        (m, s)       = ui'    `quotRem` 100
                                                    pure (BinLogTime sign (fromIntegral h)
                                                                          (fromIntegral m)
                                                                          (fromIntegral s))

getBinLogField (BINLOG_TYPE_TIMESTAMP2  fsp  ) = do s <- getWord32be -- big-endian here!
                                                    ms <- getMicroSecond fsp
                                                    pure (BinLogTimeStamp2 s ms)
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
getBinLogField (BINLOG_TYPE_DATETIME2   fsp  ) = do iPart <- getWord40be
                                                    let yyyymm = iPart `shiftR` 22 .&. 0x01FFFF -- 0b011111111111111111
                                                        (yyyy, mm) = yyyymm `quotRem` 13
                                                        yyyy' = fromIntegral yyyy
                                                        mm' = fromIntegral mm
                                                        dd = fromIntegral $ iPart `shiftR` 17 .&. 0x1F -- 0b00011111
                                                        h =  fromIntegral $ iPart `shiftR` 12 .&. 0x1F -- 0b00011111
                                                        m =  fromIntegral $ iPart `shiftR` 6 .&. 0x3F  -- 0b00111111
                                                        s =  fromIntegral $ iPart .&. 0x3F             -- 0b00111111
                                                    ms <- getMicroSecond fsp
                                                    pure (BinLogDateTime2 yyyy' mm' dd h m s ms)
  where
    getWord40be :: Get Word64
    getWord40be = do
        a <- getWord8
        b <- getWord32be
        pure (fromIntegral a `shiftL` 8  .|. fromIntegral b)

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
getBinLogField (BINLOG_TYPE_TIME2       fsp  ) = do iPart <- getWord24be
                                                    let sign = fromIntegral $ iPart `shiftR` 23
                                                        h = (fromIntegral $ iPart `shiftR` 12) .&. 0x03FF -- 0b0000001111111111
                                                        m = (fromIntegral $ iPart `shiftR` 6) .&. 0x3F    -- 0b00111111
                                                        s = (fromIntegral $ iPart) .&. 0x3F               -- 0b00111111
                                                    ms <- getMicroSecond fsp
                                                    pure (BinLogTime2 sign h m s ms)

getBinLogField (BINLOG_TYPE_YEAR             ) = do y <- getWord8
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
getBinLogField (BINLOG_TYPE_NEWDECIMAL precision scale ) = do
    let i = fromIntegral (precision - scale)
        (ucI, cI) = i `quotRem` digitsPerInteger
        (ucF, cF) = scale `quotRem` digitsPerInteger
        ucISize = fromIntegral (ucI `shiftL` 2)
        ucFSize = fromIntegral (ucF `shiftL` 2)
        cISize = fromIntegral (sizeTable `B.unsafeIndex` fromIntegral cI)
        cFSize = fromIntegral (sizeTable `B.unsafeIndex` fromIntegral cF)
        len = ucISize + cISize + ucFSize + cFSize

    buf <- getByteString (fromIntegral len)

    let fb = (trace (show buf) buf) `B.unsafeIndex` 0
        sign = if fb .&. 0x80 == 0x80 then 1 else 0
        buf' = (fb `xor` 0x80) `B.cons` B.tail buf
        buf'' = if sign == 1 then buf'
                            else B.map (xor 0xFF) buf'

        iPart = fromIntegral (getCompressed cISize (B.unsafeTake cISize buf'')) * (blockSize ^ ucI)
              + getUncompressed ucI (B.unsafeDrop cISize buf'')

    let buf''' = B.unsafeDrop (ucISize + cISize) buf''

        fPart = getUncompressed ucF (B.unsafeTake ucFSize buf''') * (10 ^ cF)
              + fromIntegral (getCompressed cFSize (B.unsafeDrop ucFSize buf'''))

    pure (BinLogNewDecimal sign iPart fPart)
  where
    digitsPerInteger = 9
    blockSize = fromIntegral $ (10 :: Int32) ^ (9 :: Int)
    sizeTable = B.pack [0, 1, 1, 2, 2, 3, 3, 4, 4, 4]

    getCompressed :: Int -> ByteString -> Word64
    getCompressed 0 _  = 0
    getCompressed x bs = let fb = bs `B.unsafeIndex` 0
                             x' = x - 1
                         in fromIntegral fb `shiftL` (8 * x') .|. getCompressed x' (B.unsafeDrop 1 bs)

    getUncompressed :: Word8 -> ByteString -> Integer
    getUncompressed 0 _ = 0
    getUncompressed x bs = let v = getCompressed 4 (B.unsafeTake 4 bs)
                               x' = x - 1
                           in fromIntegral v * (blockSize ^ x') + getUncompressed x' (B.unsafeDrop 4 bs)


getBinLogField (BINLOG_TYPE_ENUM     size    ) = if | size == 1 -> BinLogEnum . fromIntegral <$> getWord8
                                                    | size == 2 -> BinLogEnum . fromIntegral <$> getWord16be
                                                    | otherwise -> fail $ "Database.MySQL.BinLogProtocol.BinLogValue: wrong \
                                                                          \BINLOG_TYPE_ENUM size: " ++ show size


getBinLogField (BINLOG_TYPE_SET    bits bytes) = BinLogSet <$> getBits bits bytes
getBinLogField (BINLOG_TYPE_BLOB   lensize   ) = do len <- if | lensize == 1 -> fromIntegral <$> getWord8
                                                              | lensize == 2 -> fromIntegral <$> getWord16le
                                                              | lensize == 3 -> fromIntegral <$> getWord24le
                                                              | lensize == 4 -> fromIntegral <$> getWord32le
                                                              | otherwise    -> fail $  "Database.MySQL.BinLogProtocol.BinLogValue: \
                                                                                        \wrong BINLOG_TYPE_BLOB length size: "
                                                                                        ++ show lensize
                                                    BinLogBlob <$> getByteString len

getBinLogField (BINLOG_TYPE_STRING   size    ) = do len <- if | size < 256 -> fromIntegral <$> getWord8
                                                              | otherwise  -> fromIntegral <$> getWord16le
                                                    BinLogString <$> getByteString len

getBinLogField (BINLOG_TYPE_GEOMETRY lensize ) = do len <- if | lensize == 1 -> fromIntegral <$> getWord8
                                                              | lensize == 2 -> fromIntegral <$> getWord16le
                                                              | lensize == 3 -> fromIntegral <$> getWord24le
                                                              | lensize == 4 -> fromIntegral <$> getWord32le
                                                              | otherwise    -> fail $  "Database.MySQL.BinLogProtocol.BinLogValue: \
                                                                                        \wrong BINLOG_TYPE_GEOMETRY length size: "
                                                                                        ++ show lensize
                                                    BinLogGeometry <$> getByteString len

getMicroSecond :: Word8 -> Get Word32
getMicroSecond 0 = pure 0
getMicroSecond 1 = (* 100000) . fromIntegral <$> getWord8
getMicroSecond 2 = (* 10000) . fromIntegral <$> getWord8
getMicroSecond 3 = (* 1000) . fromIntegral <$> getWord16be
getMicroSecond 4 = (* 100) . fromIntegral <$> getWord16be
getMicroSecond 5 = (* 10) . fromIntegral <$> getWord24be
getMicroSecond 6 = fromIntegral <$> getWord24be
getMicroSecond _ = pure 0

getBits :: Word16 -> Word8 -> Get Word64
getBits bits bytes = do
    if bits == 0
    then fromIntegral <$> getWord8
    else if | bytes == 1 -> fromIntegral <$> getWord8
            | bytes == 2 -> fromIntegral <$> getWord16le
            | bytes == 3 -> fromIntegral <$> getWord24le
            | bytes == 4 -> fromIntegral <$> getWord32le
            | bytes == 5 -> fromIntegral <$> getWord40le
            | bytes == 6 -> fromIntegral <$> getWord48le
            | bytes == 7 -> fromIntegral <$> getWord56le
            | bytes == 7 -> fromIntegral <$> getWord64le
            | otherwise  -> fail $  "Database.MySQL.BinLogProtocol.BinLogValue: wrong bit length size: " ++ show bytes

  where
    getWord40le, getWord56le :: Get Word64
    getWord40le = do
        a <- fromIntegral <$> getWord32le
        b <- fromIntegral <$> getWord8
        return $! a .|. (b `shiftL` 32)
    getWord56le = do
        a <- fromIntegral <$> getWord32le
        b <- fromIntegral <$> getWord24le
        return $! a .|. (b `shiftL` 32)

--------------------------------------------------------------------------------
-- | BinLog row decoder
--
getBinLogRow :: [BinLogMeta] -> BitMap -> Get [BinLogValue]
getBinLogRow metas pmap = do
    let plen = B.foldl' (\acc word8 -> acc + popCount word8) 0 (fromBitMap pmap)
        maplen = (plen + 7) `shiftR` 3
    nullmap <- getByteString maplen
    go metas (BitMap nullmap) 0 pmap 0
  where
    go :: [BinLogMeta] -> BitMap -> Int -> BitMap -> Int -> Get [BinLogValue]
    go [] _       _   _          _       = pure []
    go (f:fs) nullmap !nullpos pmap' !ppos  =
        if isColumnSet pmap' ppos
        then do
            r <- if isColumnSet nullmap nullpos
                    then return BinLogNull
                    else getBinLogField f
            let nullpos' = nullpos + 1
                ppos' = ppos + 1
            rest <- go fs nullmap nullpos' pmap' ppos'
            return (rest `seq` (r : rest))
        else go fs nullmap nullpos pmap' (ppos + 1)
