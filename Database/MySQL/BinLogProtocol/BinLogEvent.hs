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

import           Control.Monad
import           Control.Monad.Loops                       (untilM)
import           Data.Bits
import           Data.Word
import           Database.MySQL.BinLogProtocol.BinLogMeta
import           Database.MySQL.BinLogProtocol.BinLogValue
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Query
import           GHC.Generics                              (Generic)
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V
import           Z.IO.Exception

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
    { blTimestamp :: {-# UNPACK #-} !Word32
    , blEventType :: {-# UNPACK #-} !BinLogEventType
    , blServerId  :: {-# UNPACK #-} !Word32
    , blEventSize :: {-# UNPACK #-} !Word32
    , blLogPos    :: {-# UNPACK #-} !Word64   -- ^ for future GTID compatibility
    , blFlags     :: {-# UNPACK #-} !Word16
    , blBody      :: {-# UNPACK #-} !V.Bytes
    , blSemiAck   :: !Bool
    } deriving (Show, Eq)

encodeSemiAckResp :: Word32 -> V.Bytes -> B.Builder ()
encodeSemiAckResp pos fn = B.encodePrim pos >> B.bytes fn

decodeBinLogPacket :: Bool -> Bool -> P.Parser BinLogPacket
decodeBinLogPacket checksum semi = do
    _  <- P.anyWord8     -- OK byte
    ack <- if semi
        then P.anyWord8 >> (== 0x01) <$> P.anyWord8
        else return False
    ts <- P.decodePrimLE @Word32
    typ <- toEnum . fromIntegral <$> P.anyWord8
    sid <- P.decodePrimLE @Word32
    size <- P.decodePrimLE @Word32
    pos <- P.decodePrimLE @Word32
    flgs <- P.decodePrimLE @Word16
    body <- P.take (fromIntegral size - if checksum then 23 else 19)
    return (BinLogPacket ts typ sid size (fromIntegral pos) flgs body ack)

decodeFromBinLogPacket :: HasCallStack => P.Parser a -> BinLogPacket -> IO a
decodeFromBinLogPacket g (BinLogPacket _ _ _ _ _ _ body _ ) = unwrap "EPARSE" $ P.parse' g body

decodeFromBinLogPacket' :: HasCallStack => (BinLogEventType -> P.Parser a) -> BinLogPacket -> IO a
decodeFromBinLogPacket' g (BinLogPacket _ typ _ _ _ _ body _ ) = unwrap "EPARSE" $ P.parse' (g typ) body

--------------------------------------------------------------------------------

data FormatDescription = FormatDescription
    { fdVersion              :: !Word16
    , fdMySQLVersion         :: !V.Bytes
    , fdCreateTime           :: !Word32
    -- , eventHeaderLen :: !Word8  -- const 19
    , fdEventHeaderLenVector :: !V.Bytes  -- ^ a array indexed by Binlog Event Type - 1
                                             -- to extract the length of the event specific header.
    } deriving (Show, Eq, Generic)

decodeFormatDescription :: P.Parser FormatDescription
decodeFormatDescription = FormatDescription <$> P.decodePrimLE @Word16
                                         <*> P.take 50
                                         <*> P.decodePrimLE @Word32
                                         <*  P.anyWord8
                                         <*> P.takeRemaining

eventHeaderLen :: FormatDescription -> BinLogEventType -> Word8
eventHeaderLen fd typ = V.unsafeIndex (fdEventHeaderLenVector fd) (fromEnum typ - 1)

data RotateEvent = RotateEvent
    { rPos :: !Word64, rFileName :: !V.Bytes } deriving (Show, Eq)

decodeRotateEvent :: P.Parser RotateEvent
decodeRotateEvent = RotateEvent <$> P.decodePrimLE <*> P.takeRemaining

-- | This's query parser for statement based binlog's query event, it's actually
-- not used in row based binlog.
--
data QueryEvent = QueryEvent
    { qSlaveProxyId :: !Word32
    , qExecTime     :: !Word32
    , qErrCode      :: !Word16
    , qStatusVars   :: !V.Bytes
    , qSchemaName   :: !V.Bytes
    , qQuery        :: !Query
    } deriving (Show, Eq, Generic)

decodeQueryEvent :: P.Parser QueryEvent
decodeQueryEvent = do
    pid <- P.decodePrimLE @Word32
    tim <- P.decodePrimLE @Word32
    slen <- P.anyWord8
    ecode <- P.decodePrimLE @Word16
    vlen <- P.decodePrimLE @Word16
    svar <- P.take (fromIntegral vlen)
    schema <- P.take (fromIntegral slen)
    _ <- P.anyWord8
    qry <- P.takeRemaining
    return (QueryEvent pid tim ecode svar schema (Query qry))

-- | This's the query event in row based binlog.
--
data QueryEvent' = QueryEvent' { qQuery' :: !Query } deriving (Show, Eq)

decodeQueryEvent' :: P.Parser QueryEvent'
decodeQueryEvent' = do
    _ <- P.anyWord8
    QueryEvent' . Query <$> P.takeRemaining

data TableMapEvent = TableMapEvent
    { tmTableId    :: !Word64
    , tmFlags      :: !Word16
    , tmSchemaName :: !V.Bytes
    , tmTableName  :: !V.Bytes
    , tmColumnCnt  :: !Int
    , tmColumnType :: !(V.PrimVector FieldType)
    , tmColumnMeta :: !(V.Vector BinLogMeta)
    , tmNullMap    :: !V.Bytes
    } deriving (Show, Eq, Generic)

decodeTableMapEvent :: FormatDescription -> P.Parser TableMapEvent
decodeTableMapEvent fd = do
    let hlen = eventHeaderLen fd BINLOG_TABLE_MAP_EVENT
    tid <- if hlen == 6 then fromIntegral <$> P.decodePrimLE @Word32 else decodeWord48LE
    flgs <- P.decodePrimLE @Word16
    slen <- P.anyWord8
    schema <- P.take (fromIntegral slen)
    _ <- P.anyWord8 -- 0x00
    tlen <- P.anyWord8
    table <- P.take (fromIntegral tlen)
    _ <- P.anyWord8 -- 0x00
    cc <- decodeLenEncInt
    typs <- P.take cc
    colMetaBS <- decodeLenEncBytes

    metas <- case P.parse' (V.traverseVec decodeBinLogMeta typs) colMetaBS of
        Left errmsg  -> P.fail' (T.concat errmsg)
        Right r      -> return r

    nullmap <- P.take ((cc + 7) `div` 8)
    return (TableMapEvent tid flgs schema table cc typs metas nullmap)

data DeleteRowsEvent = DeleteRowsEvent
    { deleteTableId    :: !Word64
    , deleteFlags      :: !Word16
    -- , deleteExtraData   :: !RowsEventExtraData
    , deleteColumnCnt  :: !Int
    , deletePresentMap :: !BitMap
    , deleteRowData    :: ![[BinLogValue]]
    } deriving (Show, Eq, Generic)

decodeDeleteRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> P.Parser DeleteRowsEvent
decodeDeleteRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> P.decodePrimLE @Word32 else decodeWord48LE
    flgs <- P.decodePrimLE @Word16
    when (typ == BINLOG_DELETE_ROWS_EVENTv2) $ do
        extraLen <- P.decodePrimLE @Word16
        void $ P.take (fromIntegral extraLen - 2)
    colCnt <- decodeLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- decodePresentMap plen poffset
    DeleteRowsEvent tid flgs colCnt pmap <$> untilM (decodeBinLogRow (tmColumnMeta tme) pmap) P.atEnd

data WriteRowsEvent = WriteRowsEvent
    { writeTableId    :: !Word64
    , writeFlags      :: !Word16
    -- , writeExtraData   :: !RowsEventExtraData
    , writeColumnCnt  :: !Int
    , writePresentMap :: !BitMap
    , writeRowData    :: ![[BinLogValue]]
    } deriving (Show, Eq, Generic)

decodeWriteRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> P.Parser WriteRowsEvent
decodeWriteRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> P.decodePrimLE @Word32 else decodeWord48LE
    flgs <- P.decodePrimLE @Word16
    when (typ == BINLOG_WRITE_ROWS_EVENTv2) $ do
        extraLen <- P.decodePrimLE @Word16
        void $ P.take (fromIntegral extraLen - 2)
    colCnt <- decodeLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- decodePresentMap plen poffset
    WriteRowsEvent tid flgs colCnt pmap <$> untilM (decodeBinLogRow (tmColumnMeta tme) pmap) P.atEnd

data UpdateRowsEvent = UpdateRowsEvent
    { updateTableId    :: !Word64
    , updateFlags      :: !Word16
    -- , updateExtraData   :: !RowsEventExtraData
    , updateColumnCnt  :: !Int
    , updatePresentMap :: !(BitMap, BitMap)
    , updateRowData    :: ![ ([BinLogValue], [BinLogValue]) ]
    } deriving (Show, Eq, Generic)

decodeUpdateRowEvent :: FormatDescription -> TableMapEvent -> BinLogEventType -> P.Parser UpdateRowsEvent
decodeUpdateRowEvent fd tme typ = do
    let hlen = eventHeaderLen fd typ
    tid <- if hlen == 6 then fromIntegral <$> P.decodePrimLE @Word32 else decodeWord48LE
    flgs <- P.decodePrimLE @Word16
    when (typ == BINLOG_UPDATE_ROWS_EVENTv2) $ do
        extraLen <- P.decodePrimLE @Word16
        void $ P.take (fromIntegral extraLen - 2)
    colCnt <- decodeLenEncInt
    let (plen, poffset) = (fromIntegral colCnt + 7) `quotRem` 8
    pmap <- decodePresentMap plen poffset
    pmap' <- decodePresentMap plen poffset
    UpdateRowsEvent tid flgs colCnt (pmap, pmap') <$>
        untilM ((,) <$> decodeBinLogRow (tmColumnMeta tme) pmap <*> decodeBinLogRow (tmColumnMeta tme) pmap')
               P.atEnd

decodePresentMap :: Int -> Int -> P.Parser BitMap
decodePresentMap plen poffset = do
    pmap <- P.take plen
    let pmap' = if V.null pmap
                then V.empty
                else V.init pmap `V.snoc` (V.last pmap .&. 0xFF `shiftR` (7 - poffset))
    pure (BitMap pmap')

