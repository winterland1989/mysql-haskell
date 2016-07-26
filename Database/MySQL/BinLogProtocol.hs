{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.BinLogProtocol where

import           Control.Monad
import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Database.MySQL.Protocol
import           Data.Vector.Unboxed (Vector)


import Control.Exception (throwIO, Exception, SomeException, Exception(..))
import Data.Typeable (Typeable, cast)

--------------------------------------------------------------------------------
-- | binlog tyoe
--
data BinLogType
    = UNKNOWN_EVENT
    | START_EVENT_V3
    | QUERY_EVENT
    | STOP_EVENT
    | ROTATE_EVENT
    | INTVAR_EVENT
    | LOAD_EVENT
    | SLAVE_EVENT
    | CREATE_FILE_EVENT
    | APPEND_BLOCK_EVENT
    | EXEC_LOAD_EVENT
    | DELETE_FILE_EVENT
    | NEW_LOAD_EVENT
    | RAND_EVENT
    | USER_VAR_EVENT
    | FORMAT_DESCRIPTION_EVENT
    | XID_EVENT
    | BEGIN_LOAD_QUERY_EVENT
    | EXECUTE_LOAD_QUERY_EVENT
    | TABLE_MAP_EVENT
    | WRITE_ROWS_EVENTv0
    | UPDATE_ROWS_EVENTv0
    | DELETE_ROWS_EVENTv0
    | WRITE_ROWS_EVENTv1
    | UPDATE_ROWS_EVENTv1
    | DELETE_ROWS_EVENTv1
    | INCIDENT_EVENT
    | HEARTBEAT_EVENT
    | IGNORABLE_EVENT
    | ROWS_QUERY_EVENT
    | WRITE_ROWS_EVENTv2
    | UPDATE_ROWS_EVENTv2
    | DELETE_ROWS_EVENTv2
    | GTID_EVENT
    | ANONYMOUS_GTID_EVENT
    | PREVIOUS_GTIDS_EVENT
  deriving (Show, Eq, Enum)

data BinLogEvent = BinLogEvent
    { timestamp :: !Word32
    , eventType :: !Word8
    , serverId  :: !Word32
    , eventSize :: !Word32
    , logPos    :: !Word32
    , flags     :: !Word16
    , body      :: !L.ByteString
    } deriving (Show, Eq)

getBinLogEvent :: Bool -> Bool -> Get BinLogEvent
getBinLogEvent checksum semi = do
    _  <- getWord8     -- OK byte
    ts <- getWord32le
    typ <- getWord8
    sid <- getWord32le
    size <- getWord32le
    pos <- getWord32le
    flgs <- getWord16le
    body <- getLazyByteString (fromIntegral size - if checksum then 23 else 19)
    return (BinLogEvent ts typ sid size pos flgs body)

data FormatDescription = FormatDescription
    { fdVersion      :: !Word16
    , fdMySQLVersion :: !ByteString
    , fdCreateTime   :: !Word32
    -- , eventHeaderLen :: !Word8  -- const 19
    , eventHeaderLenVector :: Vector Word8
    } deriving Show

