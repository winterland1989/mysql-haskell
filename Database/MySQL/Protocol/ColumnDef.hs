{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol.ColumnDef where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Char8 as BC
import           Data.Bits ((.&.))
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
--  Resultset

-- | A description of a field (column) of a table.
data ColumnDef = ColumnDef
    { -- fieldCatalog :: !ByteString            -- ^ const 'def'
      columnDB ::         !ByteString            -- ^ Database for table.
    , columnTable ::      !ByteString            -- ^ Table of column, if column was a field.
    , columnOrigTable ::  !ByteString            -- ^ Original table name, if table was an alias.
    , columnName ::       !ByteString            -- ^ Name of column.
    , columnOrigName ::   !ByteString            -- ^ Original column name, if an alias.
    -- columnFixedLen ::  !LenEncInt              -- ^ const '0x0C'
    , columnCharSet ::    !Word16                 -- ^ Character set number.
    , columnLength ::     !Word32                 -- ^ Width of column (create length).
    , columnType ::       !FieldType
    , columnFlags ::      !Word16                 -- ^ Div flags.
    , columnDecimals ::   !Word8                  -- ^ Number of decimals in field.
    -- columnfiller :: Word16                     -- const 0x00 0x00
    } deriving (Show, Eq)


getField :: Get ColumnDef
getField = ColumnDef
        <$> (skip 4                 -- const "def"
         *> getLenEncBytes)         -- db
        <*> getLenEncBytes          -- table
        <*> getLenEncBytes          -- origTable
        <*> getLenEncBytes          -- name
        <*> getLenEncBytes          -- origName
        <*  skip 1                  -- const 0x0c
        <*> getWord16le             -- charset,
        <*> getWord32le             -- length
        <*> getFieldType            -- type
        <*> getWord16le             -- flags
        <*> getWord8                -- decimals
        <* skip 2                   -- const 0x00 0x00

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

instance Binary ColumnDef where
    get = getField
    put = putField


data FieldType
    = MYSQL_TYPE_DECIMAL     -- 0x00
    | MYSQL_TYPE_TINY        -- 0x01
    | MYSQL_TYPE_SHORT       -- 0x02
    | MYSQL_TYPE_LONG        -- 0x03
    | MYSQL_TYPE_FLOAT       -- 0x04
    | MYSQL_TYPE_DOUBLE      -- 0x05
    | MYSQL_TYPE_NULL        -- 0x06
    | MYSQL_TYPE_TIMESTAMP   -- 0x07
    | MYSQL_TYPE_LONGLONG    -- 0x08
    | MYSQL_TYPE_INT24       -- 0x09
    | MYSQL_TYPE_DATE        -- 0x0a
    | MYSQL_TYPE_TIME        -- 0x0b
    | MYSQL_TYPE_DATETIME    -- 0x0c
    | MYSQL_TYPE_YEAR        -- 0x0d
    | MYSQL_TYPE_NEWDATE     -- 0x0e
    | MYSQL_TYPE_VARCHAR     -- 0x0f
    | MYSQL_TYPE_BIT         -- 0x10
    | MYSQL_TYPE_TIMESTAMP2  -- 0x11
    | MYSQL_TYPE_DATETIME2   -- 0x12
    | MYSQL_TYPE_TIME2       -- 0x13
    | MYSQL_TYPE_NEWDECIMAL  -- 0xf6
    | MYSQL_TYPE_ENUM        -- 0xf7
    | MYSQL_TYPE_SET         -- 0xf8
    | MYSQL_TYPE_TINY_BLOB   -- 0xf9
    | MYSQL_TYPE_MEDIUM_BLOB -- 0xfa
    | MYSQL_TYPE_LONG_BLOB   -- 0xfb
    | MYSQL_TYPE_BLOB        -- 0xfc
    | MYSQL_TYPE_VAR_STRING  -- 0xfd
    | MYSQL_TYPE_STRING      -- 0xfe
    | MYSQL_TYPE_GEOMETRY    -- 0xff
    | MYSQL_TYPE_UNKNOWN !Word8
  deriving (Show, Eq)

getFieldType :: Get FieldType
getFieldType = word8ToFieldType <$> getWord8

word8ToFieldType :: Word8 -> FieldType
word8ToFieldType 0x00 = MYSQL_TYPE_DECIMAL
word8ToFieldType 0x01 = MYSQL_TYPE_TINY
word8ToFieldType 0x02 = MYSQL_TYPE_SHORT
word8ToFieldType 0x03 = MYSQL_TYPE_LONG
word8ToFieldType 0x04 = MYSQL_TYPE_FLOAT
word8ToFieldType 0x05 = MYSQL_TYPE_DOUBLE
word8ToFieldType 0x06 = MYSQL_TYPE_NULL
word8ToFieldType 0x07 = MYSQL_TYPE_TIMESTAMP
word8ToFieldType 0x08 = MYSQL_TYPE_LONGLONG
word8ToFieldType 0x09 = MYSQL_TYPE_INT24
word8ToFieldType 0x0a = MYSQL_TYPE_DATE
word8ToFieldType 0x0b = MYSQL_TYPE_TIME
word8ToFieldType 0x0c = MYSQL_TYPE_DATETIME
word8ToFieldType 0x0d = MYSQL_TYPE_YEAR
word8ToFieldType 0x0e = MYSQL_TYPE_NEWDATE
word8ToFieldType 0x0f = MYSQL_TYPE_VARCHAR
word8ToFieldType 0x10 = MYSQL_TYPE_BIT
word8ToFieldType 0x11 = MYSQL_TYPE_TIMESTAMP2
word8ToFieldType 0x12 = MYSQL_TYPE_DATETIME2
word8ToFieldType 0x13 = MYSQL_TYPE_TIME2
word8ToFieldType 0xf6 = MYSQL_TYPE_NEWDECIMAL
word8ToFieldType 0xf7 = MYSQL_TYPE_ENUM
word8ToFieldType 0xf8 = MYSQL_TYPE_SET
word8ToFieldType 0xf9 = MYSQL_TYPE_TINY_BLOB
word8ToFieldType 0xfa = MYSQL_TYPE_MEDIUM_BLOB
word8ToFieldType 0xfb = MYSQL_TYPE_LONG_BLOB
word8ToFieldType 0xfc = MYSQL_TYPE_BLOB
word8ToFieldType 0xfd = MYSQL_TYPE_VAR_STRING
word8ToFieldType 0xfe = MYSQL_TYPE_STRING
word8ToFieldType 0xff = MYSQL_TYPE_GEOMETRY
word8ToFieldType x    = MYSQL_TYPE_UNKNOWN x

putFieldType :: FieldType -> Put
putFieldType = putWord8 . fieldTypeToWord8

fieldTypeToWord8 :: FieldType -> Word8
fieldTypeToWord8 MYSQL_TYPE_DECIMAL     = 0x00
fieldTypeToWord8 MYSQL_TYPE_TINY        = 0x01
fieldTypeToWord8 MYSQL_TYPE_SHORT       = 0x02
fieldTypeToWord8 MYSQL_TYPE_LONG        = 0x03
fieldTypeToWord8 MYSQL_TYPE_FLOAT       = 0x04
fieldTypeToWord8 MYSQL_TYPE_DOUBLE      = 0x05
fieldTypeToWord8 MYSQL_TYPE_NULL        = 0x06
fieldTypeToWord8 MYSQL_TYPE_TIMESTAMP   = 0x07
fieldTypeToWord8 MYSQL_TYPE_LONGLONG    = 0x08
fieldTypeToWord8 MYSQL_TYPE_INT24       = 0x09
fieldTypeToWord8 MYSQL_TYPE_DATE        = 0x0a
fieldTypeToWord8 MYSQL_TYPE_TIME        = 0x0b
fieldTypeToWord8 MYSQL_TYPE_DATETIME    = 0x0c
fieldTypeToWord8 MYSQL_TYPE_YEAR        = 0x0d
fieldTypeToWord8 MYSQL_TYPE_NEWDATE     = 0x0e
fieldTypeToWord8 MYSQL_TYPE_VARCHAR     = 0x0f
fieldTypeToWord8 MYSQL_TYPE_BIT         = 0x10
fieldTypeToWord8 MYSQL_TYPE_TIMESTAMP2  = 0x11
fieldTypeToWord8 MYSQL_TYPE_DATETIME2   = 0x12
fieldTypeToWord8 MYSQL_TYPE_TIME2       = 0x13
fieldTypeToWord8 MYSQL_TYPE_NEWDECIMAL  = 0xf6
fieldTypeToWord8 MYSQL_TYPE_ENUM        = 0xf7
fieldTypeToWord8 MYSQL_TYPE_SET         = 0xf8
fieldTypeToWord8 MYSQL_TYPE_TINY_BLOB   = 0xf9
fieldTypeToWord8 MYSQL_TYPE_MEDIUM_BLOB = 0xfa
fieldTypeToWord8 MYSQL_TYPE_LONG_BLOB   = 0xfb
fieldTypeToWord8 MYSQL_TYPE_BLOB        = 0xfc
fieldTypeToWord8 MYSQL_TYPE_VAR_STRING  = 0xfd
fieldTypeToWord8 MYSQL_TYPE_STRING      = 0xfe
fieldTypeToWord8 MYSQL_TYPE_GEOMETRY    = 0xff
fieldTypeToWord8 (MYSQL_TYPE_UNKNOWN x) = x

instance Binary FieldType where
    get = getFieldType
    put = putFieldType

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
