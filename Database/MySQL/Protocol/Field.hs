{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol.Field where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Char8 as BC
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
--  Resultset

-- | A description of a field (column) of a table.
data Field = Field
    { -- fieldCatalog :: !ByteString            -- ^ const 'def'
      fieldDB ::         !ByteString            -- ^ Database for table.
    , fieldTable ::      !ByteString            -- ^ Table of column, if column was a field.
    , fieldOrigTable ::  !ByteString            -- ^ Original table name, if table was an alias.
    , fieldName ::       !ByteString            -- ^ Name of column.
    , fieldOrigName ::   !ByteString            -- ^ Original column name, if an alias.
    -- fieldFixedLen ::  !LenEncInt              -- ^ const '0x0C'
    , fieldCharSet ::    !Word16                 -- ^ Character set number.
    , fieldLength ::     !Word32                 -- ^ Width of column (create length).
    , fieldType ::       !FieldType
    , fieldFlags ::      !Word16                 -- ^ Div flags.
    , fieldDecimals ::   !Word8                  -- ^ Number of decimals in field.
    -- fieldfiller :: Word16                     -- const 0x00 0x00
    } deriving (Show, Eq)


getField :: Get Field
getField = Field
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

putField :: Field -> Put
putField (Field db tbl otbl name oname charset len typ flags dec) = do
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

instance Binary Field where
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
  deriving (Show, Eq, Enum)

getFieldType :: Get FieldType
getFieldType = do
    w <- getWord8
    case w of
        0x00 -> pure MYSQL_TYPE_DECIMAL
        0x01 -> pure MYSQL_TYPE_TINY
        0x02 -> pure MYSQL_TYPE_SHORT
        0x03 -> pure MYSQL_TYPE_LONG
        0x04 -> pure MYSQL_TYPE_FLOAT
        0x05 -> pure MYSQL_TYPE_DOUBLE
        0x06 -> pure MYSQL_TYPE_NULL
        0x07 -> pure MYSQL_TYPE_TIMESTAMP
        0x08 -> pure MYSQL_TYPE_LONGLONG
        0x09 -> pure MYSQL_TYPE_INT24
        0x0a -> pure MYSQL_TYPE_DATE
        0x0b -> pure MYSQL_TYPE_TIME
        0x0c -> pure MYSQL_TYPE_DATETIME
        0x0d -> pure MYSQL_TYPE_YEAR
        0x0e -> pure MYSQL_TYPE_NEWDATE
        0x0f -> pure MYSQL_TYPE_VARCHAR
        0x10 -> pure MYSQL_TYPE_BIT
        0x11 -> pure MYSQL_TYPE_TIMESTAMP2
        0x12 -> pure MYSQL_TYPE_DATETIME2
        0x13 -> pure MYSQL_TYPE_TIME2
        0xf6 -> pure MYSQL_TYPE_NEWDECIMAL
        0xf7 -> pure MYSQL_TYPE_ENUM
        0xf8 -> pure MYSQL_TYPE_SET
        0xf9 -> pure MYSQL_TYPE_TINY_BLOB
        0xfa -> pure MYSQL_TYPE_MEDIUM_BLOB
        0xfb -> pure MYSQL_TYPE_LONG_BLOB
        0xfc -> pure MYSQL_TYPE_BLOB
        0xfd -> pure MYSQL_TYPE_VAR_STRING
        0xfe -> pure MYSQL_TYPE_STRING
        0xff -> pure MYSQL_TYPE_GEOMETRY
        _    -> fail $ "wrong FieldType: " ++ show w

putFieldType :: FieldType -> Put
putFieldType MYSQL_TYPE_DECIMAL    = putWord8 0x00
putFieldType MYSQL_TYPE_TINY       = putWord8 0x01
putFieldType MYSQL_TYPE_SHORT      = putWord8 0x02
putFieldType MYSQL_TYPE_LONG       = putWord8 0x03
putFieldType MYSQL_TYPE_FLOAT      = putWord8 0x04
putFieldType MYSQL_TYPE_DOUBLE     = putWord8 0x05
putFieldType MYSQL_TYPE_NULL       = putWord8 0x06
putFieldType MYSQL_TYPE_TIMESTAMP  = putWord8 0x07
putFieldType MYSQL_TYPE_LONGLONG   = putWord8 0x08
putFieldType MYSQL_TYPE_INT24      = putWord8 0x09
putFieldType MYSQL_TYPE_DATE       = putWord8 0x0a
putFieldType MYSQL_TYPE_TIME       = putWord8 0x0b
putFieldType MYSQL_TYPE_DATETIME   = putWord8 0x0c
putFieldType MYSQL_TYPE_YEAR       = putWord8 0x0d
putFieldType MYSQL_TYPE_NEWDATE    = putWord8 0x0e
putFieldType MYSQL_TYPE_VARCHAR    = putWord8 0x0f
putFieldType MYSQL_TYPE_BIT        = putWord8 0x10
putFieldType MYSQL_TYPE_TIMESTAMP2 = putWord8 0x11
putFieldType MYSQL_TYPE_DATETIME2  = putWord8 0x12
putFieldType MYSQL_TYPE_TIME2      = putWord8 0x13
putFieldType MYSQL_TYPE_NEWDECIMAL = putWord8 0xf6
putFieldType MYSQL_TYPE_ENUM       = putWord8 0xf7
putFieldType MYSQL_TYPE_SET        = putWord8 0xf8
putFieldType MYSQL_TYPE_TINY_BLOB  = putWord8 0xf9
putFieldType MYSQL_TYPE_MEDIUM_BLOB= putWord8 0xfa
putFieldType MYSQL_TYPE_LONG_BLOB  = putWord8 0xfb
putFieldType MYSQL_TYPE_BLOB       = putWord8 0xfc
putFieldType MYSQL_TYPE_VAR_STRING = putWord8 0xfd
putFieldType MYSQL_TYPE_STRING     = putWord8 0xfe
putFieldType MYSQL_TYPE_GEOMETRY   = putWord8 0xff

instance Binary FieldType where
    get = getFieldType
    put = putFieldType

