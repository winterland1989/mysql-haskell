{-# LANGUAGE MultiWayIf #-}
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

There're certain type won't appear in binlog event, and some types are compressed into 'mySQLTypeString'
, please take python version as a reference:  <https://github.com/noplay/python-mysql-replication>

You will not directly meet following 'FieldType' namely:

    * mySQLTypeDECIMAL
    * mySQLTypeNEWDATE
    * mySQLTypeENUM
    * mySQLTypeSET
    * mySQLTypeTINY_BLOB
    * mySQLTypeMEDIUM_BLOB
    * mySQLTypeLONG_BLOB

-}

module Database.MySQL.BinLogProtocol.BinLogMeta where

import           Control.Applicative
import           Data.Binary.Get
import           Data.Bits
import           Data.Word
import           Database.MySQL.Protocol.ColumnDef

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

getBinLogMeta :: FieldType -> Get BinLogMeta
getBinLogMeta t
    | t == mySQLTypeTiny       = pure BINLOG_TYPE_TINY
    | t == mySQLTypeShort      = pure BINLOG_TYPE_SHORT
    | t == mySQLTypeInt24      = pure BINLOG_TYPE_INT24
    | t == mySQLTypeLong       = pure BINLOG_TYPE_LONG
    | t == mySQLTypeLongLong   = pure BINLOG_TYPE_LONGLONG
    | t == mySQLTypeFloat      = BINLOG_TYPE_FLOAT <$> getWord8
    | t == mySQLTypeDouble     = BINLOG_TYPE_DOUBLE <$> getWord8

    | t == mySQLTypeBit        = do
        nbits <- getWord8
        nbytes <- getWord8
        let nbits' = (fromIntegral nbytes * 8) + fromIntegral nbits
            nbytes' = fromIntegral $ (nbits' + 7) `div` 8
        pure $! BINLOG_TYPE_BIT nbits' nbytes'

    | t == mySQLTypeTimestamp  = pure BINLOG_TYPE_TIMESTAMP
    | t == mySQLTypeDateTime   = pure BINLOG_TYPE_DATETIME
    | t == mySQLTypeDate       = pure BINLOG_TYPE_DATE
    | t == mySQLTypeTime       = pure BINLOG_TYPE_TIME
    | t == mySQLTypeTimestamp2 = BINLOG_TYPE_TIMESTAMP2 <$> getWord8
    | t == mySQLTypeDateTime2  = BINLOG_TYPE_DATETIME2 <$> getWord8
    | t == mySQLTypeTime2      = BINLOG_TYPE_TIME2 <$> getWord8
    | t == mySQLTypeYear       = pure BINLOG_TYPE_YEAR
    | t == mySQLTypeNewDecimal = BINLOG_TYPE_NEWDECIMAL <$> getWord8 <*> getWord8
    | t == mySQLTypeVarChar    = BINLOG_TYPE_STRING <$> getWord16le
    | t == mySQLTypeVarString  = BINLOG_TYPE_STRING <$> getWord16le

    | t == mySQLTypeString     = do
        low <- getWord8
        high <- getWord8
        -- http://bugs.mysql.com/37426
        if high > 0
        then if (high .&. 0x30) /= 0x30
             then if word8ToFieldType (high .|. 0x30) == mySQLTypeString
                  then let len = fromIntegral $ (high .&. 0x30) `xor` 0x30
                           len' = len `shiftL` 4 .|. fromIntegral low
                       in pure $! BINLOG_TYPE_STRING len'
                  else let len = fromIntegral high `shiftL` 8 :: Word16
                           len' = len .|. fromIntegral low
                       in pure $! BINLOG_TYPE_STRING len'
             else let t' = word8ToFieldType high
                  in if | t' == mySQLTypeSet    -> let nbits = fromIntegral low `shiftL` 3
                                                       nbytes = fromIntegral $ (nbits + 7) `shiftR` 8
                                                   in pure (BINLOG_TYPE_SET nbits nbytes)
                        | t' == mySQLTypeEnum   -> pure (BINLOG_TYPE_ENUM low)
                        | t' == mySQLTypeString -> pure (BINLOG_TYPE_STRING (fromIntegral low))
                        | otherwise             -> fail $ "Database.MySQL.BinLogProtocol.BinLogMeta:\
                                                           \ impossible type inside binlog string: " ++ show t'
        else pure (BINLOG_TYPE_STRING (fromIntegral low))

    | t == mySQLTypeBlob       = BINLOG_TYPE_BLOB <$> getWord8
    | t == mySQLTypeGeometry   = BINLOG_TYPE_GEOMETRY <$> getWord8
    | otherwise                = fail $ "Database.MySQL.BinLogProtocol.BinLogMeta:\
                                        \ impossible type in binlog: " ++ show t
