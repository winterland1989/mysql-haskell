{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.BinLogProtocol.BinLogMeta
Description : binlog protocol column meta
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide column meta decoder for binlog protocol, note this's different from 'BinLogDef'
used in binary result-set.

There're certain type won't appear in binlog event, and some types are compressed into 'MYSQL_TYPE_STRING'
, please take python version as a reference:  <https://github.com/noplay/python-mysql-replication>

You will not directly meet following 'FieldType' namely:

    * MYSQL_TYPE_DECIMAL
    * MYSQL_TYPE_NEWDATE
    * MYSQL_TYPE_ENUM
    * MYSQL_TYPE_SET
    * MYSQL_TYPE_TINY_BLOB
    * MYSQL_TYPE_MEDIUM_BLOB
    * MYSQL_TYPE_LONG_BLOB

-}

module Database.MySQL.BinLogProtocol.BinLogMeta where

import Data.Word
import Data.Bits
import Data.Binary.Get
import Database.MySQL.Protocol.ColumnDef

data BinLogMeta
    = BINLOG_TYPE_FLOAT       !Word8                -- size
    | BINLOG_TYPE_DOUBLE      !Word8                -- size
    | BINLOG_TYPE_BIT         !Word16 !Word8         -- bits, bytes
    | BINLOG_TYPE_TIMESTAMP
    | BINLOG_TYPE_DATETIME
    | BINLOG_TYPE_DATE
    | BINLOG_TYPE_TIME
    | BINLOG_TYPE_TIMESTAMP2  !Word8                -- fsp
    | BINLOG_TYPE_DATETIME2   !Word8                -- fsp
    | BINLOG_TYPE_TIME2       !Word8                -- fsp
    | BINLOG_TYPE_YEAR
    | BINLOG_TYPE_NEWDECIMAL  !Word8 !Word8         -- precision, scale
    | BINLOG_TYPE_ENUM        !Word8                -- 1 or 2('Word8' or 'Word16'), enum index size
    | BINLOG_TYPE_SET         !Word16 !Word8        -- bitmap bits, bytes
    | BINLOG_TYPE_BLOB        !Word8                -- length size
    | BINLOG_TYPE_STRING      !Word16               -- meta length(if < 256, then length is 8bit, if > 256 then length is 16bit)
    | BINLOG_TYPE_GEOMETRY    !Word8                -- length size

getBinLogMeta :: FieldType -> Get BinLogMeta
getBinLogMeta MYSQL_TYPE_FLOAT           = BINLOG_TYPE_FLOAT <$> getWord8
getBinLogMeta MYSQL_TYPE_DOUBLE          = BINLOG_TYPE_DOUBLE <$> getWord8
getBinLogMeta MYSQL_TYPE_BIT             = do nbits <- getWord8
                                              nbytes <- getWord8
                                              let nbits' = (fromIntegral nbytes * 8) + fromIntegral nbits
                                                  nbytes' = fromIntegral $ (nbits' + 7) `div` 8
                                              pure $! BINLOG_TYPE_BIT nbits' nbytes'

getBinLogMeta MYSQL_TYPE_TIMESTAMP       = pure BINLOG_TYPE_TIMESTAMP
getBinLogMeta MYSQL_TYPE_DATETIME        = pure BINLOG_TYPE_DATETIME
getBinLogMeta MYSQL_TYPE_DATE            = pure BINLOG_TYPE_DATE
getBinLogMeta MYSQL_TYPE_TIME            = pure BINLOG_TYPE_TIME
getBinLogMeta MYSQL_TYPE_TIMESTAMP2      = BINLOG_TYPE_TIMESTAMP2 <$> getWord8
getBinLogMeta MYSQL_TYPE_DATETIME2       = BINLOG_TYPE_DATETIME2 <$> getWord8
getBinLogMeta MYSQL_TYPE_TIME2           = BINLOG_TYPE_TIME2 <$> getWord8
getBinLogMeta MYSQL_TYPE_YEAR            = pure BINLOG_TYPE_YEAR
getBinLogMeta MYSQL_TYPE_NEWDECIMAL      = BINLOG_TYPE_NEWDECIMAL <$> getWord8 <*> getWord8
getBinLogMeta MYSQL_TYPE_VARCHAR         = BINLOG_TYPE_STRING <$> getWord16le
getBinLogMeta MYSQL_TYPE_VAR_STRING      = BINLOG_TYPE_STRING <$> getWord16le
getBinLogMeta MYSQL_TYPE_STRING          = do low <- getWord8
                                              high <- getWord8
                                              -- http://bugs.mysql.com/37426
                                              if high > 0
                                              then if (high .&. 0x30) /= 0x30
                                                  then case  word8ToFieldType (high .|. 0x30) of
                                                      MYSQL_TYPE_STRING ->
                                                          let len = fromIntegral $ (high .&. 0x30) `xor` 0x30
                                                              len' = len `shiftL` 4 .|. fromIntegral low
                                                          in pure $! BINLOG_TYPE_STRING len'
                                                      _                 ->
                                                          let len = fromIntegral high `shiftL` 8 :: Word16
                                                              len' = len .|. fromIntegral low
                                                          in pure $! BINLOG_TYPE_STRING len'
                                                  else case word8ToFieldType high of
                                                      MYSQL_TYPE_SET     -> let nbits = fromIntegral low `shiftL` 3
                                                                                nbytes = fromIntegral $ (nbits + 7) `shiftR` 8
                                                                            in pure (BINLOG_TYPE_SET nbits nbytes)
                                                      MYSQL_TYPE_ENUM    -> pure (BINLOG_TYPE_ENUM low)
                                                      MYSQL_TYPE_STRING  -> pure (BINLOG_TYPE_STRING (fromIntegral low))
                                              else pure (BINLOG_TYPE_STRING (fromIntegral low))

getBinLogMeta MYSQL_TYPE_BLOB            = BINLOG_TYPE_BLOB <$> getWord8
getBinLogMeta MYSQL_TYPE_GEOMETRY        = BINLOG_TYPE_GEOMETRY <$> getWord8
getBinLogMeta t                          = fail $ "Database.MySQL.BinLogProtocol.BinLogMeta: impossible type in binlog: " ++ show t
