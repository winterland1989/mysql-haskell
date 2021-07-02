{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.BinLogProtocol.BinLogMeta
Description : Binlog protocol column meta
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide column meta decoder for binlog protocol.

There're certain type won't appear in binlog event, and some types are compressed into 'mySQLTypeString'
, please take python version as a reference:  <https://github.com/noplay/python-mysql-replication>

You will not directly meet following 'FieldType' namely:

    * mySQLTypeDecimal
    * mySQLTypeNewdate
    * mySQLTypeEnum
    * mySQLTypeSet
    * mySQLTypeTinyBlob
    * mySQLTypeMediumBlOb
    * mySQLTypeLongBlob

-}

module Database.MySQL.BinLogProtocol.BinLogMeta where

import           Control.Applicative
import           Data.Bits
import           Data.Word
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V
import           Database.MySQL.Protocol.ColumnDef

-- | An intermedia date type for decoding row-based event's values.
--
data BinLogMeta
    = BINLOG_TYPE_TINY
    | BINLOG_TYPE_SHORT
    | BINLOG_TYPE_INT24
    | BINLOG_TYPE_LONG
    | BINLOG_TYPE_LONGLONG
    | BINLOG_TYPE_FLOAT       !Word8         -- ^ size
    | BINLOG_TYPE_DOUBLE      !Word8         -- ^ size
    | BINLOG_TYPE_BIT         !Word16 !Word8 -- ^ bits, bytes
    | BINLOG_TYPE_TIMESTAMP
    | BINLOG_TYPE_DATETIME
    | BINLOG_TYPE_DATE
    | BINLOG_TYPE_TIME
    | BINLOG_TYPE_TIMESTAMP2  !Word8         -- ^ fsp
    | BINLOG_TYPE_DATETIME2   !Word8         -- ^ fsp
    | BINLOG_TYPE_TIME2       !Word8         -- ^ fsp
    | BINLOG_TYPE_YEAR
    | BINLOG_TYPE_NEWDECIMAL  !Word8 !Word8  -- ^ precision, scale
    | BINLOG_TYPE_ENUM        !Word8         -- ^ 1 or 2('Word8' or 'Word16'), enum index size
    | BINLOG_TYPE_SET         !Word16 !Word8 -- ^ bitmap bits, bytes
    | BINLOG_TYPE_BLOB        !Word8         -- ^ length size
    | BINLOG_TYPE_STRING      !Word16        -- ^ meta length(if < 256, then length is 8bit,
                                             -- if > 256 then length is 16bit)
    | BINLOG_TYPE_GEOMETRY    !Word8         -- ^ length size
  deriving (Show, Eq)

decodeBinLogMeta :: FieldType -> P.Parser BinLogMeta
decodeBinLogMeta t
    | t == MySQLTypeTiny       = pure BINLOG_TYPE_TINY
    | t == MySQLTypeShort      = pure BINLOG_TYPE_SHORT
    | t == MySQLTypeInt24      = pure BINLOG_TYPE_INT24
    | t == MySQLTypeLong       = pure BINLOG_TYPE_LONG
    | t == MySQLTypeLongLong   = pure BINLOG_TYPE_LONGLONG
    | t == MySQLTypeFloat      = BINLOG_TYPE_FLOAT <$> P.anyWord8
    | t == MySQLTypeDouble     = BINLOG_TYPE_DOUBLE <$> P.anyWord8

    | t == MySQLTypeBit        = do
        byte0 <- P.anyWord8
        byte1 <- P.anyWord8
        let nbits = (fromIntegral byte1 `shiftL` 3) .|.  fromIntegral byte0
            nbytes = fromIntegral $ (nbits + 7) `shiftR` 3
        pure (BINLOG_TYPE_BIT nbits nbytes)

    | t == MySQLTypeTimestamp  = pure BINLOG_TYPE_TIMESTAMP
    | t == MySQLTypeDateTime   = pure BINLOG_TYPE_DATETIME
    | t == MySQLTypeDate       = pure BINLOG_TYPE_DATE
    | t == MySQLTypeTime       = pure BINLOG_TYPE_TIME
    | t == MySQLTypeTimestamp2 = BINLOG_TYPE_TIMESTAMP2 <$> P.anyWord8
    | t == MySQLTypeDateTime2  = BINLOG_TYPE_DATETIME2 <$> P.anyWord8
    | t == MySQLTypeTime2      = BINLOG_TYPE_TIME2 <$> P.anyWord8
    | t == MySQLTypeYear       = pure BINLOG_TYPE_YEAR
    | t == MySQLTypeNewDecimal = BINLOG_TYPE_NEWDECIMAL <$> P.anyWord8 <*> P.anyWord8
    | t == MySQLTypeVarChar    = BINLOG_TYPE_STRING <$> P.decodePrimLE
    | t == MySQLTypeVarString  = BINLOG_TYPE_STRING <$> P.decodePrimLE

    | t == MySQLTypeString     = do
        byte0 <- P.anyWord8
        byte1 <- P.anyWord8
        -- http://bugs.mysql.com/37426
        if  byte0 > 0
        then if (byte0 .&. 0x30) /= 0x30
             then if (byte0 .|. 0x30) == MySQLTypeString
                  then let len = fromIntegral $ (byte0 .&. 0x30) `xor` 0x30
                           len' = len `shiftL` 4 .|. fromIntegral byte1
                       in pure $! BINLOG_TYPE_STRING len'
                  else let len = fromIntegral byte0 `shiftL` 8 :: Word16
                           len' = len .|. fromIntegral byte1
                       in pure $! BINLOG_TYPE_STRING len'
             else let t' = byte0
                  in if | t' == MySQLTypeSet    -> let nbits = fromIntegral byte1 `shiftL` 3
                                                       nbytes = fromIntegral $ (nbits + 7) `shiftR` 8
                                                   in pure (BINLOG_TYPE_SET nbits nbytes)
                        | t' == MySQLTypeEnum   -> pure (BINLOG_TYPE_ENUM byte1)
                        | t' == MySQLTypeString -> pure (BINLOG_TYPE_STRING (fromIntegral byte1))
                        | otherwise             -> P.fail' $ "Database.MySQL.BinLogProtocol.BinLogMeta:\
                                                           \ impossible type inside binlog string: " <> T.toText t'
        else pure (BINLOG_TYPE_STRING (fromIntegral byte1))

    | t == MySQLTypeBlob       = BINLOG_TYPE_BLOB <$> P.anyWord8
    | t == MySQLTypeGeometry   = BINLOG_TYPE_GEOMETRY <$> P.anyWord8
    | otherwise                = P.fail' $ "Database.MySQL.BinLogProtocol.BinLogMeta:\
                                        \ impossible type in binlog: " <> T.toText t
