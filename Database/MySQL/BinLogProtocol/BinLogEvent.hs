{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.BinLogProtocol.BinLogEvent
Description : Binlog event
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Binlog event type
-}

module Database.MySQL.BinLogProtocol.BinLogEvent where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Loops                       (untilM)
import           Data.Binary
import           Data.Binary.Parser
import           Data.Bits
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Lazy                      as L
import qualified Data.ByteString.Unsafe                    as B
import           Database.MySQL.BinLogProtocol.BinLogMeta
import           Database.MySQL.BinLogProtocol.BinLogValue
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.ColumnDef

import           Control.Exception                         (throwIO)
import           Database.MySQL.Query
import           GHC.Generics                              (Generic)

--------------------------------------------------------------------------------
-- | binlog tyoe
--
data BinLogEventType
    = BINLOG_UNKNOWN_EVENT
    | BINLOG_START_EVENT_V3
    | BINLOG_QUERY_EVENT
    | BINLOG_STOP_EVENT
    | BINLOG_ROTATE_EVENT
    | BINLOG_INTVAR_EVENT
    | BINLOG_LOAD_EVENT
    | BINLOG_SLAVE_EVENT
    | BINLOG_CREATE_FILE_EVENT
    | BINLOG_APPEND_BLOCK_EVENT
    | BINLOG_EXEC_LOAD_EVENT
    | BINLOG_DELETE_FILE_EVENT
    | BINLOG_NEW_LOAD_EVENT
    | BINLOG_RAND_EVENT
    | BINLOG_USER_VAR_EVENT
    | BINLOG_FORMAT_DESCRIPTION_EVENT
    | BINLOG_XID_EVENT
    | BINLOG_BEGIN_LOAD_QUERY_EVENT
    | BINLOG_EXECUTE_LOAD_QUERY_EVENT
    | BINLOG_TABLE_MAP_EVENT
    | BINLOG_WRITE_ROWS_EVENTv0
    | BINLOG_UPDATE_ROWS_EVENTv0
    | BINLOG_DELETE_ROWS_EVENTv0
    | BINLOG_WRITE_ROWS_EVENTv1
    | BINLOG_UPDATE_ROWS_EVENTv1
    | BINLOG_DELETE_ROWS_EVENTv1
    | BINLOG_INCIDENT_EVENT
    | BINLOG_HEARTBEAT_EVENT
    | BINLOG_IGNORABLE_EVENT
    | BINLOG_ROWS_QUERY_EVENT
    | BINLOG_WRITE_ROWS_EVENTv2
    | BINLOG_UPDATE_ROWS_EVENTv2
    | BINLOG_DELETE_ROWS_EVENTv2
    | BINLOG_GTID_EVENT
    | BINLOG_ANONYMOUS_GTID_EVENT
    | BINLOG_PREVIOUS_GTIDS_EVENT
  deriving (Show, Eq, Enum)

data BinLogPacket = BinLogPacket
    { blTimestamp :: !Word32
    , blEventType :: !BinLogEventType
    , blServerId  :: !Word32
    , blEventSize :: !Word32
    , blLogPos    :: !Word64   -- ^ for future GTID compatibility
    , blFlags     :: !Word16
    , blBody      :: !L.ByteString
    , blSemiAck   :: !Bool
    } deriving (Show, Eq)

putSemiAckResp :: Word32 -> ByteString -> Put
putSemiAckResp pos fn = put pos >> put fn

getBinLogPacket :: Bool -> Bool -> Get BinLogPacket
getBinLogPacket checksum semi = do
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
    return (BinLogPacket ts typ sid size (fromIntegral pos) flgs body ack)

getFromBinLogPacket :: Get a -> BinLogPacket -> IO a
getFromBinLogPacket g (BinLogPacket _ _ _ _ _ _ body _ ) =
    case parseDetailLazy g body  of
        Left  (buf, offset, errmsg) -> throwIO (DecodePacketFailed buf offset errmsg)
        Right (_,   _,      r     ) -> return r

getFromBinLogPacket' :: (BinLogEventType -> Get a) -> BinLogPacket -> IO a
getFromBinLogPacket' g (BinLogPacket _ typ _ _ _ _ body _ ) =
    case parseDetailLazy (g typ) body  of
        Left  (buf, offset, errmsg) -> throwIO (DecodePacketFailed buf offset errmsg)
        Right (_,   _,      r     ) -> return r

--------------------------------------------------------------------------------

data FormatDescription = FormatDescription
    { fdVersion              :: !Word16
    , fdMySQLVersion         :: !ByteString
    , fdCreateTime           :: !Word32
    -- , eventHeaderLen :: !Word8  -- const 19
    , fdEventHeaderLenVector :: !ByteString  -- ^ a array indexed by Binlog Event Type - 1
                                             -- to extract the length of the event specific header.
    } deriving (Show, Eq, Generic)

getFormatDescription :: Get FormatDescription
getFormatDescription = FormatDescription <$> getWord16le
                                         <*> getByteString 50
                                         <*> getWord32le
                                         <*  getWord8
                                         <*> (L.toStrict <$> getRemainingLazyByteString)

eventHeaderLen :: FormatDescription -> BinLogEventType -> Word8
eventHeaderLen fd typ = B.unsafeIndex (fdEventHeaderLenVector fd) (fromEnum typ - 1)

data RotateEvent = RotateEvent
    { rPos :: !Word64, rFileName :: !ByteString } deriving (Show, Eq)

getRotateEvent :: Get RotateEvent
getRotateEvent = RotateEvent <$> getWord64le <*> getRemainingByteString

-- | This's query parser for statement based binlog's query event, it's actually
-- not used in row based binlog.
--
data QueryEvent = QueryEvent
    { qSlaveProxyId :: !Word32
    , qExecTime     :: !Word32
    , qErrCode      :: !Word16
    , qStatusVars   :: !ByteString
    , qSchemaName   :: !ByteString
    , qQuery        :: !Query
    } deriving (Show, Eq, Generic)

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
    return (QueryEvent pid tim ecode svar schema (Query qry))

-- | This's the query event in row based binlog.
--
data QueryEvent' = QueryEvent' { qQuery' :: !Query } deriving (Show, Eq)

getQueryEvent' :: Get QueryEvent'
getQueryEvent' = do
    _ <- getWord8
    QueryEvent' . Query <$> getRemainingLazyByteString

data TableMapEvent = TableMapEvent
    { tmTableId    :: !Word64
    , tmFlags      :: !Word16
    , tmSchemaName :: !ByteString
    , tmTableName  :: !ByteString
    , tmColumnCnt  :: !Int
    , tmColumnType :: ![FieldType]
    , tmColumnMeta :: ![BinLogMeta]
    , tmNullMap    :: !ByteString
    } deriving (Show, Eq, Generic)

getTableMapEvent :: FormatDescription -> Get TableMapEvent
getTableMapEvent fd = do
    let hlen = eventHeaderLen fd BINLOG_TABLE_MAP_EVENT
    tid <- if hlen == 6 then fromIntegral <$> getWord32le else getWord48le
    flgs <- getWord16le
    slen <- getWord8
    schema <- getByteString (fromIntegral slen)
    _ <- getWord8 -- 0x00
    tlen <- getWord8
    table <- getByteString (fromIntegral tlen)
    _ <- getWord8 -- 0x00
    cc <- getLenEncInt
    colTypBS <- getByteString cc
    let typs = map FieldType (B.unpack colTypBS)
    colMetaBS <- getLenEncBytes

    metas <- case runGetOrFail (forM typs getBinLogMeta) (L.fromStrict colMetaBS) of
        Left (_, _, errmsg) -> fail errmsg
        Right (_, _, r)     -> return r

    nullmap <- getByteString ((cc + 7) `div` 8)
    return (TableMapEvent tid flgs schema table cc typs metas nullmap)

data DeleteRowsEvent = DeleteRowsEvent
    { deleteTableId    :: !Word64
    , deleteFlags      :: !Word16
    -- , deleteExtraData   :: !RowsEventExtraData
    , deleteColumnCnt  :: !Int
    , deletePresentMap :: !BitMap
    , deleteRowData    :: ![[BinLogValue]]
    } deriving (Show, Eq, Generic)

getDeleteRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> Get DeleteRowsEvent
getDeleteRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> getWord32le else getWord48le
    flgs <- getWord16le
    when (typ == BINLOG_DELETE_ROWS_EVENTv2) $ do
        extraLen <- getWord16le
        void $ getByteString (fromIntegral extraLen - 2)
    colCnt <- getLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- getPresentMap plen poffset
    DeleteRowsEvent tid flgs colCnt pmap <$> untilM (getBinLogRow (tmColumnMeta tme) pmap) isEmpty

data WriteRowsEvent = WriteRowsEvent
    { writeTableId    :: !Word64
    , writeFlags      :: !Word16
    -- , writeExtraData   :: !RowsEventExtraData
    , writeColumnCnt  :: !Int
    , writePresentMap :: !BitMap
    , writeRowData    :: ![[BinLogValue]]
    } deriving (Show, Eq, Generic)

getWriteRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> Get WriteRowsEvent
getWriteRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> getWord32le else getWord48le
    flgs <- getWord16le
    when (typ == BINLOG_WRITE_ROWS_EVENTv2) $ do
        extraLen <- getWord16le
        void $ getByteString (fromIntegral extraLen - 2)
    colCnt <- getLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- getPresentMap plen poffset
    WriteRowsEvent tid flgs colCnt pmap <$> untilM (getBinLogRow (tmColumnMeta tme) pmap) isEmpty

data UpdateRowsEvent = UpdateRowsEvent
    { updateTableId    :: !Word64
    , updateFlags      :: !Word16
    -- , updateExtraData   :: !RowsEventExtraData
    , updateColumnCnt  :: !Int
    , updatePresentMap :: !(BitMap, BitMap)
    , updateRowData    :: ![ ([BinLogValue], [BinLogValue]) ]
    } deriving (Show, Eq, Generic)

getUpdateRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> Get UpdateRowsEvent
getUpdateRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> getWord32le else getWord48le
    flgs <- getWord16le
    when (typ == BINLOG_UPDATE_ROWS_EVENTv2) $ do
        extraLen <- getWord16le
        void $ getByteString (fromIntegral extraLen - 2)
    colCnt <- getLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- getPresentMap plen poffset
    pmap' <- getPresentMap plen poffset
    UpdateRowsEvent tid flgs colCnt (pmap, pmap') <$>
        untilM ((,) <$> getBinLogRow (tmColumnMeta tme) pmap <*> getBinLogRow (tmColumnMeta tme) pmap')
               isEmpty

getPresentMap :: Int -> Int -> Get BitMap
getPresentMap plen poffset = do
    pmap <- getByteString plen
    let pmap' = if B.null pmap
                then B.empty
                else B.init pmap `B.snoc` (B.last pmap .&. 0xFF `shiftR` (7 - poffset))
    pure (BitMap pmap')

