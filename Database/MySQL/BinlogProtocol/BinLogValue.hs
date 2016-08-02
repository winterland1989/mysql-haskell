{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad
import qualified Data.ByteString.Lex.Integral   as LexInt
import qualified Data.ByteString.Lex.Fractional as LexFrac
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC
import Data.Scientific (Scientific)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import qualified Blaze.Text as Textual
import qualified Data.Bits as Bit
import Data.Bits ((.|.))
import Data.Fixed (Pico)
import Data.Word
import Data.Int
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Binary.Get
import Data.Binary.Put
import Database.MySQL.Protocol.ColumnDef
import Database.MySQL.Protocol.Packet
import Database.MySQL.Protocol.MySQLValue

--------------------------------------------------------------------------------
-- | BinLog protocol decoder
getBinLogField :: ColumnDef -> Get MySQLValue
getBinLogField f
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
        || t == MYSQL_TYPE_DATETIME     = do n <- getLenEncInt
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
                                                _ -> fail "Database.MySQL.MySQLValue: wrong TIMESTAMP/DATETIME length"

    | t == MYSQL_TYPE_TIMESTAMP2        = fail "Database.MySQL.MySQLValue: unexpected type MYSQL_TYPE_TIMESTAMP2"
    | t == MYSQL_TYPE_DATETIME2         = fail "Database.MySQL.MySQLValue: unexpected type MYSQL_TYPE_DATETIME2"
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = do n <- getLenEncInt
                                             case n of
                                                0 -> pure $ MySQLDate (fromGregorian 0 0 0)
                                                4 -> MySQLDate <$> (fromGregorian <$> getYear <*> getInt8' <*> getInt8')
                                                _ -> fail "Database.MySQL.MySQLValue: wrong DATE/NEWDATE length"

    | t == MYSQL_TYPE_TIME              = do n <- getLenEncInt
                                             case n of
                                                0 -> pure $ MySQLTime (TimeOfDay 0 0 0)
                                                8 -> do
                                                    _ <- getWord8  -- we ignore sign here because 'TimeOfDay' doesn't support,
                                                    _ <- getWord32le   -- we also ignore the day part
                                                    MySQLTime <$> (TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4)

                                                12 -> do
                                                    _ <- getWord8  -- we ignore sign here because 'TimeOfDay' doesn't support,
                                                    _ <- getWord32le   -- we also ignore the day part
                                                    MySQLTime <$> (TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8)
                                                _ -> fail "Database.MySQL.MySQLValue: wrong TIME length"

    | t == MYSQL_TYPE_TIME2             = fail "Database.MySQL.MySQLValue: unexpected type MYSQL_TYPE_TIME2"
    | t == MYSQL_TYPE_GEOMETRY          = MySQLBytes <$> getLenEncBytes
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_BIT
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = if isText then MySQLText . T.decodeUtf8 <$> getLenEncBytes
                                                    else MySQLBytes <$> getLenEncBytes
  where
    t = columnType f
    isUnsigned = flagUnsigned (columnFlags f)
    isText = columnCharSet f /= 63
    -- please check https://github.com/jeremycole/mysql_binlog
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
        pure $! (realToFrac s + realToFrac ms / 1000 :: Double)

    feedLenEncBytes typ con parser = do
        bs <- getLenEncBytes
        case parser bs of
            Just v -> return (con v)
            Nothing -> fail $ "Database.MySQL.MySQLValue: parsing " ++ show typ ++ " failed, input: " ++ BC.unpack bs

--------------------------------------------------------------------------------
-- | BinLog row decoder
--
getBinLogRow :: [ColumnDef] -> Int -> Get [MySQLValue]
getBinLogRow fields flen = do
    _ <- getWord8           -- 0x00
    let maplen = (flen + 7 + 2) `Bit.shiftR` 3
    nullmap <- getByteString maplen
    go fields nullmap (0 :: Int)
  where
    go [] _       _        = pure []
    go (f:fs) nullmap pos = do
        r <- if isNull nullmap pos
                then return MySQLNull
                else getBinLogField f
        let pos' = pos + 1
        rest <- pos' `seq` go fs nullmap pos'
        return (r `seq` rest `seq` (r : rest))

    isNull nullmap pos =
        let (i, j) = (pos + 2) `divMod` 8
        in (nullmap `B.unsafeIndex` i) `Bit.testBit` j

--------------------------------------------------------------------------------
-- | BinLog protocol encoder
--
putBinLogField :: MySQLValue -> Put
putBinLogField (MySQLDecimal    n) = putBuilder (scientificBuilder n)
putBinLogField (MySQLInt8U      n) = putWord8 n
putBinLogField (MySQLInt8       n) = putWord8 (fromIntegral n)
putBinLogField (MySQLInt16U     n) = putWord16le n
putBinLogField (MySQLInt16      n) = putInt16le n
putBinLogField (MySQLInt32U     n) = putWord32le n
putBinLogField (MySQLInt32      n) = putInt32le n
putBinLogField (MySQLInt64U     n) = putWord64le n
putBinLogField (MySQLInt64      n) = putInt64le n
putBinLogField (MySQLFloat      x) = putFloatle x
putBinLogField (MySQLDouble     x) = putDoublele x
putBinLogField (MySQLYear       n) = putWord16le n
putBinLogField (MySQLDateTime  (LocalTime date time)) = do putWord8 11    -- always put full
                                                           putBinLogDay date
                                                           putBinLogTime time
putBinLogField (MySQLDate   d)    = putBinLogDay d
putBinLogField (MySQLTime   t)    = putBinLogTime t
putBinLogField (MySQLBytes    bs) = putLenEncBytes bs
putBinLogField (MySQLText      t) = putLenEncBytes (T.encodeUtf8 t)
putBinLogField MySQLNull          = return ()

putBinLogDay :: Day -> Put
putBinLogDay d = do let (yyyy, mm, dd) = toGregorian d
                    putWord16le (fromIntegral yyyy)
                    putWord8 (fromIntegral mm)
                    putWord8 (fromIntegral dd)

putBinLogTime :: TimeOfDay -> Put
putBinLogTime (TimeOfDay hh mm ss) = do let sInt = floor ss
                                            sFrac = floor $ (ss - realToFrac sInt) * 1000000
                                        putWord8 (fromIntegral hh)
                                        putWord8 (fromIntegral mm)
                                        putWord8 sInt
                                        putWord64le sFrac

