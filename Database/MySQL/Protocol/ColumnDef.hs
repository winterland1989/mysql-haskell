{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.Protocol.ColumnDef
Description : MySQL field type
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Column definition(aka. field type).

-}

module Database.MySQL.Protocol.ColumnDef where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Parser
import           Data.Binary.Put
import           Data.Bits                      ((.&.))
import           Data.ByteString                (ByteString)
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
--  Resultset

-- | A description of a field (column) of a table.
data ColumnDef = ColumnDef
    { -- fieldCatalog :: !ByteString              -- ^ const 'def'
      columnDB        ::  !ByteString             -- ^ Database for table.
    , columnTable     ::  !ByteString             -- ^ Table of column, if column was a field.
    , columnOrigTable ::  !ByteString             -- ^ Original table name, if table was an alias.
    , columnName      ::  !ByteString             -- ^ Name of column.
    , columnOrigName  ::  !ByteString             -- ^ Original column name, if an alias.
    , columnCharSet   ::  !Word16                 -- ^ Character set number.
    , columnLength    ::  !Word32                 -- ^ Width of column (create length).
    , columnType      ::  !FieldType
    , columnFlags     ::  !Word16                 -- ^ Div flags.
    , columnDecimals  ::  !Word8                  -- ^ Number of decimals in field.
    } deriving (Show, Eq)

getField :: Get ColumnDef
getField = ColumnDef
        <$> (skipN 4                 -- const "def"
         *> getLenEncBytes)         -- db
        <*> getLenEncBytes          -- table
        <*> getLenEncBytes          -- origTable
        <*> getLenEncBytes          -- name
        <*> getLenEncBytes          -- origName
        <*  skipN 1                  -- const 0x0c
        <*> getWord16le             -- charset
        <*> getWord32le             -- length
        <*> getFieldType            -- type
        <*> getWord16le             -- flags
        <*> getWord8                -- decimals
        <* skipN 2                   -- const 0x00 0x00
{-# INLINE getField #-}

putField :: ColumnDef -> Put
putField (ColumnDef db tbl otbl name oname charset len typ flags dec) = do
    putLenEncBytes "def"
    putLenEncBytes db
    putLenEncBytes tbl
    putLenEncBytes otbl
    putLenEncBytes name
    putLenEncBytes oname
    putWord16le charset
    putWord32le len
    putFieldType typ
    putWord16le  flags
    putWord8 dec
    putWord16le 0X0000
{-# INLINE putField #-}

instance Binary ColumnDef where
    get = getField
    {-# INLINE get #-}
    put = putField
    {-# INLINE put #-}

-- | @newtype@ around 'Word8' for represent @MySQL_TYPE@, We don't use sum type here for speed reason.
--
newtype FieldType = FieldType Word8 deriving (Show, Eq)

mySQLTypeDecimal, mySQLTypeTiny, mySQLTypeShort, mySQLTypeLong, mySQLTypeFloat :: FieldType
mySQLTypeDouble, mySQLTypeNull, mySQLTypeTimestamp, mySQLTypeLongLong, mySQLTypeInt24 :: FieldType
mySQLTypeDate, mySQLTypeTime, mySQLTypeDateTime, mySQLTypeYear, mySQLTypeNewDate, mySQLTypeVarChar :: FieldType
mySQLTypeBit, mySQLTypeTimestamp2, mySQLTypeDateTime2, mySQLTypeTime2, mySQLTypeNewDecimal :: FieldType
mySQLTypeEnum, mySQLTypeSet, mySQLTypeTinyBlob, mySQLTypeMediumBlob, mySQLTypeLongBlob :: FieldType
mySQLTypeBlob, mySQLTypeVarString, mySQLTypeString, mySQLTypeGeometry :: FieldType

mySQLTypeDecimal        = FieldType 0x00
mySQLTypeTiny           = FieldType 0x01
mySQLTypeShort          = FieldType 0x02
mySQLTypeLong           = FieldType 0x03
mySQLTypeFloat          = FieldType 0x04
mySQLTypeDouble         = FieldType 0x05
mySQLTypeNull           = FieldType 0x06
mySQLTypeTimestamp      = FieldType 0x07
mySQLTypeLongLong       = FieldType 0x08
mySQLTypeInt24          = FieldType 0x09
mySQLTypeDate           = FieldType 0x0a
mySQLTypeTime           = FieldType 0x0b
mySQLTypeDateTime       = FieldType 0x0c
mySQLTypeYear           = FieldType 0x0d
mySQLTypeNewDate        = FieldType 0x0e
mySQLTypeVarChar        = FieldType 0x0f
mySQLTypeBit            = FieldType 0x10
mySQLTypeTimestamp2     = FieldType 0x11
mySQLTypeDateTime2      = FieldType 0x12
mySQLTypeTime2          = FieldType 0x13
mySQLTypeNewDecimal     = FieldType 0xf6
mySQLTypeEnum           = FieldType 0xf7
mySQLTypeSet            = FieldType 0xf8
mySQLTypeTinyBlob       = FieldType 0xf9
mySQLTypeMediumBlob     = FieldType 0xfa
mySQLTypeLongBlob       = FieldType 0xfb
mySQLTypeBlob           = FieldType 0xfc
mySQLTypeVarString      = FieldType 0xfd
mySQLTypeString         = FieldType 0xfe
mySQLTypeGeometry       = FieldType 0xff

getFieldType :: Get FieldType
getFieldType = FieldType <$> getWord8
{-# INLINE getFieldType #-}

putFieldType :: FieldType -> Put
putFieldType (FieldType t) = putWord8 t
{-# INLINE putFieldType #-}

instance Binary FieldType where
    get = getFieldType
    {-# INLINE get #-}
    put = putFieldType
    {-# INLINE put #-}

--------------------------------------------------------------------------------
--  Field flags

#define NOT_NULL_FLAG         1
#define PRI_KEY_FLAG          2
#define UNIQUE_KEY_FLAG       4
#define MULT_KEY_FLAG         8
#define BLOB_FLAG             16
#define UNSIGNED_FLAG         32
#define ZEROFILL_FLAG         64
#define BINARY_FLAG           128
#define ENUM_FLAG             256
#define AUTO_INCREMENT_FLAG   512
#define TIMESTAMP_FLAG        1024
#define SET_FLAG              2048
#define NO_DEFAULT_VALUE_FLAG 4096
#define PART_KEY_FLAG         16384
#define NUM_FLAG              32768

flagNotNull, flagPrimaryKey, flagUniqueKey, flagMultipleKey, flagBlob, flagUnsigned, flagZeroFill :: Word16 -> Bool
flagBinary, flagEnum, flagAutoIncrement, flagTimeStamp, flagSet, flagNoDefaultValue, flagPartKey, flagNumeric :: Word16 -> Bool
flagNotNull        flags = flags .&. NOT_NULL_FLAG         == NOT_NULL_FLAG
flagPrimaryKey     flags = flags .&. PRI_KEY_FLAG          == PRI_KEY_FLAG
flagUniqueKey      flags = flags .&. UNIQUE_KEY_FLAG       == UNIQUE_KEY_FLAG
flagMultipleKey    flags = flags .&. MULT_KEY_FLAG         == MULT_KEY_FLAG
flagBlob           flags = flags .&. BLOB_FLAG             == BLOB_FLAG
flagUnsigned       flags = flags .&. UNSIGNED_FLAG         == UNSIGNED_FLAG
flagZeroFill       flags = flags .&. ZEROFILL_FLAG         == ZEROFILL_FLAG
flagBinary         flags = flags .&. BINARY_FLAG           == BINARY_FLAG
flagEnum           flags = flags .&. ENUM_FLAG             == ENUM_FLAG
flagAutoIncrement  flags = flags .&. AUTO_INCREMENT_FLAG   == AUTO_INCREMENT_FLAG
flagTimeStamp      flags = flags .&. TIMESTAMP_FLAG        == TIMESTAMP_FLAG
flagSet            flags = flags .&. SET_FLAG              == SET_FLAG
flagNoDefaultValue flags = flags .&. NO_DEFAULT_VALUE_FLAG == NO_DEFAULT_VALUE_FLAG
flagPartKey        flags = flags .&. PART_KEY_FLAG         == PART_KEY_FLAG
flagNumeric        flags = flags .&. NUM_FLAG              == NUM_FLAG
