{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

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

import           Data.Word
import           Data.Bits
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Builder                 as B
import qualified Z.Data.Vector                  as V
import qualified Z.Data.Text.Base               as T
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
--  Resultset

-- | A description of a field (column) of a table.
data ColumnDef = ColumnDef
    { -- fieldCatalog :: !V.Bytes                       -- ^ const 'def'
      columnDB        ::  {-# UNPACK #-} !T.Text        -- ^ Database for table.
    , columnTable     ::  {-# UNPACK #-} !T.Text        -- ^ Table of column, if column was a field.
    , columnOrigTable ::  {-# UNPACK #-} !T.Text        -- ^ Original table name, if table was an alias.
    , columnName      ::  {-# UNPACK #-} !T.Text        -- ^ Name of column.
    , columnOrigName  ::  {-# UNPACK #-} !T.Text        -- ^ Original column name, if an alias.
    , columnCharSet   ::  {-# UNPACK #-} !Word16        -- ^ Character set number.
    , columnLength    ::  {-# UNPACK #-} !Word32        -- ^ Width of column (create length).
    , columnType      ::  {-# UNPACK #-} !FieldType
    , columnFlags     ::  {-# UNPACK #-} !Word16        -- ^ Div flags.
    , columnDecimals  ::  {-# UNPACK #-} !Word8         -- ^ Number of decimals in field.
    } deriving (Show, Eq)

decodeField :: P.Parser ColumnDef
{-# INLINE decodeField #-}
decodeField = do
    P.skip 4               -- const "def"
    db <- decodeLenEncBytes
    table <- decodeLenEncBytes
    origTable <- decodeLenEncBytes
    name <- decodeLenEncBytes
    origName <- decodeLenEncBytes
    P.skipWord8             -- const 0x0c
    charset <- P.decodePrimLE
    len <- P.decodePrimLE
    typ <- P.decodePrim
    flags <- P.decodePrimLE
    decimals <- P.decodePrim
    P.skip 2                -- const 0x00 0x00
    return (ColumnDef (T.Text db) (T.Text table) (T.Text origTable) (T.Text name) (T.Text origName) charset len typ flags decimals)

encodeField :: ColumnDef -> B.Builder ()
{-# INLINE encodeField #-}
encodeField (ColumnDef db tbl otbl name oname charset len typ flags dec) = do
    encodeLenEncBytes "def"
    encodeLenEncBytes (T.getUTF8Bytes db)
    encodeLenEncBytes (T.getUTF8Bytes tbl)
    encodeLenEncBytes (T.getUTF8Bytes otbl)
    encodeLenEncBytes (T.getUTF8Bytes name)
    encodeLenEncBytes (T.getUTF8Bytes oname)
    B.encodePrimLE charset
    B.encodePrimLE len
    B.encodePrim typ
    B.encodePrimLE  flags
    B.encodePrim dec
    B.encodePrimLE @Word16 0X0000

-- | MySQL_TYPE
type FieldType = Word8

pattern MySQLTypeDecimal    :: FieldType
pattern MySQLTypeTiny       :: FieldType
pattern MySQLTypeShort      :: FieldType
pattern MySQLTypeLong       :: FieldType
pattern MySQLTypeFloat      :: FieldType
pattern MySQLTypeDouble     :: FieldType
pattern MySQLTypeNull       :: FieldType
pattern MySQLTypeTimestamp  :: FieldType
pattern MySQLTypeLongLong   :: FieldType
pattern MySQLTypeInt24      :: FieldType
pattern MySQLTypeDate       :: FieldType
pattern MySQLTypeTime       :: FieldType
pattern MySQLTypeDateTime   :: FieldType
pattern MySQLTypeYear       :: FieldType
pattern MySQLTypeNewDate    :: FieldType
pattern MySQLTypeVarChar    :: FieldType
pattern MySQLTypeBit        :: FieldType
pattern MySQLTypeTimestamp2 :: FieldType
pattern MySQLTypeDateTime2  :: FieldType
pattern MySQLTypeTime2      :: FieldType
pattern MySQLTypeNewDecimal :: FieldType
pattern MySQLTypeEnum       :: FieldType
pattern MySQLTypeSet        :: FieldType
pattern MySQLTypeTinyBlob   :: FieldType
pattern MySQLTypeMediumBlob :: FieldType
pattern MySQLTypeLongBlob   :: FieldType
pattern MySQLTypeBlob       :: FieldType
pattern MySQLTypeVarString  :: FieldType
pattern MySQLTypeString     :: FieldType
pattern MySQLTypeGeometry   :: FieldType

pattern MySQLTypeDecimal        = 0x00
pattern MySQLTypeTiny           = 0x01
pattern MySQLTypeShort          = 0x02
pattern MySQLTypeLong           = 0x03
pattern MySQLTypeFloat          = 0x04
pattern MySQLTypeDouble         = 0x05
pattern MySQLTypeNull           = 0x06
pattern MySQLTypeTimestamp      = 0x07
pattern MySQLTypeLongLong       = 0x08
pattern MySQLTypeInt24          = 0x09
pattern MySQLTypeDate           = 0x0a
pattern MySQLTypeTime           = 0x0b
pattern MySQLTypeDateTime       = 0x0c
pattern MySQLTypeYear           = 0x0d
pattern MySQLTypeNewDate        = 0x0e
pattern MySQLTypeVarChar        = 0x0f
pattern MySQLTypeBit            = 0x10
pattern MySQLTypeTimestamp2     = 0x11
pattern MySQLTypeDateTime2      = 0x12
pattern MySQLTypeTime2          = 0x13
pattern MySQLTypeNewDecimal     = 0xf6
pattern MySQLTypeEnum           = 0xf7
pattern MySQLTypeSet            = 0xf8
pattern MySQLTypeTinyBlob       = 0xf9
pattern MySQLTypeMediumBlob     = 0xfa
pattern MySQLTypeLongBlob       = 0xfb
pattern MySQLTypeBlob           = 0xfc
pattern MySQLTypeVarString      = 0xfd
pattern MySQLTypeString         = 0xfe
pattern MySQLTypeGeometry       = 0xff

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
