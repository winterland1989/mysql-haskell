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
import qualified Data.ByteString         as B
import qualified Data.ByteString.Unsafe  as B
import           Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as L
import           Database.MySQL.Protocol
import           Data.Vector.Unboxed (Vector)


import Control.Exception (throwIO, Exception(..))
import Data.Typeable (Typeable)
import Debug.Trace

--------------------------------------------------------------------------------
-- | binlog tyoe
--
data BinLogEventType
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
    { bleTimestamp :: !Word32
    , bleEventType :: !BinLogEventType
    , bleServerId  :: !Word32
    , bleEventSize :: !Word32
    , bleLogPos    :: !Word32
    , bleFlags     :: !Word16
    , bleBody      :: !L.ByteString
    , bleSemiAck   :: !Bool
    } deriving (Show, Eq)

putSemiAckResp :: Word32 -> ByteString -> Put
putSemiAckResp pos fn = put pos >> put fn

isFakeBinLogEvent :: BinLogEvent -> Bool
isFakeBinLogEvent (BinLogEvent ts _ _ _ _ _ _ _) = ts == 0

getBinLogEvent :: Bool -> Bool -> Get BinLogEvent
getBinLogEvent checksum semi = do
    _  <- getWord8     -- OK byte
    ack <- if semi
        then getWord8 >> (== 0x01) <$> getWord8
        else return False
    ts <- getWord32le
    typ <- toEnum . fromIntegral <$> getWord8
    sid <- getWord32le
    size <- getWord32le
    pos <- getWord32le
    flgs <- getWord16le
    body <- getLazyByteString (fromIntegral size - if checksum then 23 else 19)
    return (BinLogEvent ts typ sid size pos flgs body ack)

getFromBinLogEvent :: Get a -> BinLogEvent -> IO a
getFromBinLogEvent g (BinLogEvent _ _ _ _ __ _ body _ ) =
    case runGetOrFail g body of
        Left (buf, offset, errmsg) -> throwIO (DecodeBinLogEventException (L.toStrict buf) offset errmsg)
        Right (_, _, r)            -> return r

--------------------------------------------------------------------------------
-- | BinLogEvent item type



data FormatDescription = FormatDescription
    { fdVersion      :: !Word16
    , fdMySQLVersion :: !ByteString
    , fdCreateTime   :: !Word32
    -- , eventHeaderLen :: !Word8  -- const 19
    , eventHeaderLenVector :: !ByteString
    } deriving (Show, Eq)

getFormatDescription :: Get FormatDescription
getFormatDescription = FormatDescription <$> getWord16le
                                         <*> getByteString 50
                                         <*> getWord32le
                                         <*  getWord8
                                         <*> (L.toStrict <$> getRemainingLazyByteString)

eventHeaderLen :: FormatDescription -> BinLogEventType -> Word8
eventHeaderLen fmt typ = B.unsafeIndex (eventHeaderLenVector fmt) (fromEnum typ)

data RotateEvent = RotateEvent
    { rePos :: !Word64, reFileName :: !ByteString } deriving (Show, Eq)

getRotateEvent :: Get RotateEvent
getRotateEvent = RotateEvent <$> getWord64le <*> getRemainingByteString


data QueryEvent = QueryEvent
    { qeSlaveProxyId :: !Word32
    , qeExecTime     :: !Word32
    , qeErrCode      :: !Word16
    , qeStatusVars   :: !ByteString
    , qeSchemaName   :: !ByteString
    , qeQuery        :: !L.ByteString
    } deriving (Show, Eq)

getQueryEvent :: Get QueryEvent
getQueryEvent = do
    pid <- getWord32le
    tim <- getWord32le
    slen <- getWord8
    ecode <- getWord16le
    vlen <- getWord16le
    svar <- getByteString (fromIntegral vlen)
    schema <- getByteString (fromIntegral slen)
    _ <- getWord8
    qry <- getRemainingLazyByteString
    return (QueryEvent pid tim ecode svar schema qry)

data TableMapEvent = TableMapEvent
    { tmeTableId    :: !Word64
    , tmeFlags      :: !Word16
    , tmeSchemaName :: !ByteString
    , tmeTableName  :: !ByteString
    , tmeColumnCnt  :: !Int
    , tmeColumnDef  :: !ByteString
    , tmeColumeMeta :: !ByteString
    , tmeNullMask   :: !ByteString
    } deriving (Show, Eq)

getTableMapEvent :: FormatDescription -> Get TableMapEvent
getTableMapEvent fmt = do
    let hlen = eventHeaderLen fmt TABLE_MAP_EVENT
    tid <- if hlen == 6 then fromIntegral <$> getWord32le else getWord48le
    flgs <- getWord16le
    slen <- getWord8
    schema <- getByteString (fromIntegral slen)
    _ <- getWord8 -- 0x00
    tlen <- getWord8
    table <- getByteString (fromIntegral tlen)
    _ <- getWord8 -- 0x00
    cc <- getLenEncInt
    columnDef <- getByteString cc
    columnMeta <- getLenEncBytes
    nullmap <- getByteString ((cc + 7) `div` 8)
    return (TableMapEvent tid flgs schema table cc columnDef columnMeta nullmap)

--------------------------------------------------------------------------------
-- | exception tyoe

data UnexpectedBinLogEvent = UnexpectedBinLogEvent BinLogEvent
    deriving (Show, Typeable)
instance Exception UnexpectedBinLogEvent where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data DecodeBinLogEventException = DecodeBinLogEventException ByteString ByteOffset String
    deriving (Show, Typeable)
instance Exception DecodeBinLogEventException where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException
