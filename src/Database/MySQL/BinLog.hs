{-|
Module      : Database.MySQL.BinLog
Description : Binary protocol toolkit
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide tools for binlog listening and row based binlog decoding.
-}

module Database.MySQL.BinLog
    ( -- * binlog utilities
      SlaveID
    , BinLogTracker(..)
    , registerPesudoSlave
    , dumpBinLog
    , RowBinLogEvent(..)
    , decodeRowBinLogEvent
    , RowEventDecodeException(..)
    , decodeBinLogPacket
    , BinLogChecksumMismatch(..)
    , BinLogChecksum(..)
    -- * helpers
    , getLastBinLogTracker
    , isCheckSumEnabled
    , isSemiSyncEnabled
    -- * re-export
    , module Database.MySQL.BinLogProtocol.BinLogEvent
    , module Database.MySQL.BinLogProtocol.BinLogValue
    , module Database.MySQL.BinLogProtocol.BinLogMeta
    ) where

import           Control.Exception                         (Exception (..),
                                                            throwIO)
import           Control.Monad
import           Data.Binary.Put
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as L
import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)
import           Data.Bits                                 (shiftL, (.&.), (.|.))
import           Data.Text.Encoding                        (encodeUtf8)
import           Data.Word
import           Database.MySQL.Base
import           Database.MySQL.BinLogProtocol.BinLogEvent
import           Database.MySQL.BinLogProtocol.BinLogMeta
import           Database.MySQL.BinLogProtocol.BinLogValue
import           Database.MySQL.BinLogProtocol.CRC32       (crc32)
import           Database.MySQL.Connection
import           GHC.Generics                              (Generic)
import           Numeric                                   (showHex)
import           System.IO.Streams                         (InputStream)
import qualified System.IO.Streams                         as Stream
import qualified Unwitch.Convert.Word8                     as Word8

type SlaveID = Word32

-- | binlog filename and position to start listening.
--
data BinLogTracker = BinLogTracker
    { btFileName :: {-# UNPACK #-} !ByteString
    , btNextPos  :: {-# UNPACK #-} !Word32
    } deriving (Show, Eq, Generic)

-- | Register a pesudo slave to master, although MySQL document suggests you should call this
-- before calling 'dumpBinLog', but it seems it's not really necessary.
--
registerPesudoSlave :: MySQLConn -> SlaveID -> IO OK
registerPesudoSlave conn sid = command conn (COM_REGISTER_SLAVE sid "" "" "" 0 0 0)

-- | Setup binlog listening on given connection, during listening
-- the connection CAN NOT be used to do query, or an 'UnconsumedResultSet' will be thrown.
--
dumpBinLog :: MySQLConn               -- ^ connection to be listened
           -> SlaveID                 -- ^ a number for our pesudo slave.
           -> BinLogTracker           -- ^ binlog position
           -> Bool                    -- ^ if master support semi-ack, do we want to enable it?
                                      -- if master doesn't support, this parameter will be ignored.
           -> IO (FormatDescription, IORef ByteString, InputStream BinLogPacket)
                -- ^ 'FormatDescription', 'IORef' contains current binlog filename, 'BinLogPacket' stream.
dumpBinLog conn@(MySQLConn is wp _ state) sid (BinLogTracker initfn initpos) wantAck = do
    guardUnconsumed conn
    checksum <- isCheckSumEnabled conn
    when checksum $ void $ execute_ conn "SET @master_binlog_checksum = @@global.binlog_checksum"
    semiAck  <- isSemiSyncEnabled conn
    let needAck = semiAck && wantAck
    when needAck . void $ execute_ conn "SET @rpl_semi_sync_slave = 1"
    writeCommand (COM_BINLOG_DUMP initpos 0x00 sid initfn) wp
    writeIORef state RowsUnread

    fref <- newIORef initfn
    rp <- skipToPacketT (readBinLogPacket checksum needAck fref is) BINLOG_ROTATE_EVENT
    re <- getFromBinLogPacket getRotateEvent rp
    writeIORef fref (rFileName re)

    p <- skipToPacketT (readBinLogPacket checksum needAck fref is) BINLOG_FORMAT_DESCRIPTION_EVENT
    replyAck needAck p fref wp
    fmt <- getFromBinLogPacket getFormatDescription p

    es <- Stream.makeInputStream $ do
        q <- readBinLogPacket checksum needAck fref is
        case q of
            Nothing   -> writeIORef state RepliesRead >> return Nothing
            Just q' -> do when (blEventType q' == BINLOG_ROTATE_EVENT) $ do
                                e <- getFromBinLogPacket getRotateEvent q'
                                writeIORef' fref (rFileName e)
                          replyAck needAck q' fref wp
                          return q
    return (fmt, fref, es)
  where
    skipToPacketT iop typ = do
        p <- iop
        case p of
            Just p' -> do
                if blEventType p' == typ then return p' else skipToPacketT iop typ
            Nothing -> throwIO NetworkException

    replyAck needAck p fref wp' = when (needAck && blSemiAck p) $ do
        fn <- readIORef fref
        wp' (makeSemiAckPacket (blLogPos p) fn)

    makeSemiAckPacket pos fn = putToPacket 0 $ do
        putWord8 0xEF      -- semi-ack
        putWord64le pos
        putByteString fn

    readBinLogPacket checksum needAck fref' is' = do
        p <- readPacket is'
        if  | isOK p -> do
                fileName <- readIORef fref'
                Just <$> decodeBinLogPacket checksum needAck fileName p
            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException
            | otherwise -> error ("unkown package" <> show p)

-- | Decode a packet of a binlog dump as 'getBinLogPacket' does with the same
-- two flags, and check the CRC32 checksum of the event in it when the event
-- carries one. 'dumpBinLog' reads every packet this way.
--
-- Throws 'BinLogChecksumMismatch' when the checksum does not match the
-- event's bytes, rather than decode a corrupted event, and
-- 'DecodePacketException' when the packet holds no binlog event. See
-- Note [Binlog event checksums] for which events carry a checksum.
--
-- @since 1.5.0
decodeBinLogPacket :: Bool        -- ^ whether the dump's events carry checksums, as 'isCheckSumEnabled' says
                   -> Bool        -- ^ whether the packet has a semi-sync header
                   -> ByteString  -- ^ the binlog file the dump is reading, for the exception
                   -> Packet
                   -> IO BinLogPacket
decodeBinLogPacket checksum semiAck fileName packet = do
    event <- getFromPacket (getBinLogPacket checksum semiAck) packet
    let eventType = blEventType event
        eventBytes = L.drop (if semiAck then 3 else 1) (pBody packet)
        expected = carriedChecksum eventBytes
        actual = computedChecksum eventType eventBytes
    when (carriesChecksum checksum eventType eventBytes && expected /= actual) $
        throwIO (BinLogChecksumMismatch eventType fileName (blLogPos event) expected actual)
    pure event

-- Decision: an event other than a format description event carries a checksum
-- when the dump's setting says so, the setting 'getBinLogPacket' strips four
-- bytes by. A replica follows the algorithm the last format description event
-- names instead, which differs for a binlog file written while binlog_checksum
-- was NONE; such a file now fails its checks, where it used to lose the last
-- four bytes of each event without a word.

-- | See Note [Binlog event checksums].
carriesChecksum :: Bool -> BinLogEventType -> L.ByteString -> Bool
carriesChecksum checksum eventType eventBytes =
    if eventType == BINLOG_FORMAT_DESCRIPTION_EVENT
        then L.indexMaybe eventBytes (L.length eventBytes - 5) == Just checksumAlgorithmCRC32
        else checksum

-- | @BINLOG_CHECKSUM_ALG_CRC32@, as a format description event names it.
checksumAlgorithmCRC32 :: Word8
checksumAlgorithmCRC32 = 1

-- | The checksum in an event's last four bytes.
carriedChecksum :: L.ByteString -> BinLogChecksum
carriedChecksum eventBytes =
    BinLogChecksum (L.foldr prependByte 0 (L.drop (L.length eventBytes - 4) eventBytes))

prependByte :: Word8 -> Word32 -> Word32
prependByte byte rest = Word8.toWord32 byte .|. (rest `shiftL` 8)

-- | The CRC32 of an event's bytes before its checksum. See
-- Note [Binlog event checksums].
computedChecksum :: BinLogEventType -> L.ByteString -> BinLogChecksum
computedChecksum eventType eventBytes =
    let checked = L.take (L.length eventBytes - 4) eventBytes
    in BinLogChecksum
        (crc32 (if eventType == BINLOG_FORMAT_DESCRIPTION_EVENT then clearInUseFlag checked else checked))

-- | Clear @LOG_EVENT_BINLOG_IN_USE_F@ (0x01) in the low byte of an event's
-- flags, which is its 18th byte.
clearInUseFlag :: L.ByteString -> L.ByteString
clearInUseFlag bytes =
    L.take 17 bytes <> L.map (.&. 0xFE) (L.take 1 (L.drop 17 bytes)) <> L.drop 18 bytes

-- | A binlog event whose CRC32 checksum does not match its bytes, as when it
-- was corrupted between the server's binlog and this client.
--
-- @since 1.5.0
data BinLogChecksumMismatch = BinLogChecksumMismatch
    { mismatchEventType :: !BinLogEventType
    , mismatchFileName  :: !ByteString
      -- ^ The binlog file the dump was reading when the event arrived.
    , mismatchLogPos    :: !Word64
      -- ^ The position after the event that its header gives, as 'blLogPos';
      -- 0 for an event the server makes up for the dump, such as its first
      -- rotate event.
    , mismatchExpected  :: !BinLogChecksum
      -- ^ The checksum the event carries.
    , mismatchActual    :: !BinLogChecksum
      -- ^ The CRC32 of the event's bytes.
    } deriving (Show, Eq)

-- | The CRC32 checksum of a binlog event.
--
-- @since 1.5.0
newtype BinLogChecksum = BinLogChecksum Word32
    deriving (Show, Eq)

instance Exception BinLogChecksumMismatch where
    displayException mismatch =
        "mysql-haskell: the binlog event of type "
        ++ show (binLogEventTypeCode (mismatchEventType mismatch))
        ++ " (" ++ show (mismatchEventType mismatch) ++ ") ending at position "
        ++ show (mismatchLogPos mismatch) ++ " of " ++ B8.unpack (mismatchFileName mismatch)
        ++ " fails its CRC32 checksum: it carries " ++ showChecksum (mismatchExpected mismatch)
        ++ ", its bytes give " ++ showChecksum (mismatchActual mismatch)
        ++ ". The event was corrupted on its way from the server's binlog, so it is not"
        ++ " decoded. If the binlog file was written while binlog_checksum was NONE, its"
        ++ " events have no checksum, and a dump from a server whose binlog_checksum is"
        ++ " now CRC32 cannot read that file."

showChecksum :: BinLogChecksum -> String
showChecksum (BinLogChecksum checksum) =
    let digits = showHex checksum ""
    in "0x" ++ replicate (8 - length digits) '0' ++ digits

{- Note [Binlog event checksums]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With binlog_checksum = CRC32, MySQL (5.6.1 and up) and MariaDB end each event
with the CRC32 of the event's bytes before it, header and body alike. A
replica checks it over the event as the packet delivers it, not over the
length the event's header gives (MySQL's Log_event_footer::event_checksum_test
on read_event's packet length, MariaDB's event_checksum_test in
sql/rpl_utility.cc), and so does 'decodeBinLogPacket'.

The format description event differs twice. Its checksum is computed with
LOG_EVENT_BINLOG_IN_USE_F cleared, since the server clears that flag in place
when it closes the binlog file, so a check clears it as well. And the byte
before its checksum names the algorithm of the events after it, yet the event
always ends in four checksum bytes; when that byte says off (0), the server
does not recompute them after rewriting the event's position for a dump. A
replica therefore checks a format description event only when its own byte
says CRC32 (1), whatever the dump's setting.
-}

-- | Row based binlog event type.
--
-- It's recommended to enable row query event before 'dumpBinLog', so that you can get
-- 'RowQueryEvent' in row based binlog(it's important for detect a table change for example),
-- more information please refer <http://dev.mysql.com/doc/refman/5.7/en/replication-options-binary-log.html#sysvar_binlog_rows_query_log_events sysvar_binlog_rows_query_log_events>
--
-- A 'BinLogTracker' is included so that you can roll up your own HA solutions,
-- for example, writing the tracker to zookeeper when you done with an event.
--
-- The first 'Word32' field is a timestamp present when this event is logged.
--
data RowBinLogEvent
    = RowQueryEvent  {-# UNPACK #-} !Word32 !BinLogTracker !QueryEvent'
    | RowDeleteEvent {-# UNPACK #-} !Word32 !BinLogTracker !TableMapEvent !DeleteRowsEvent
    | RowWriteEvent  {-# UNPACK #-} !Word32 !BinLogTracker !TableMapEvent !WriteRowsEvent
    | RowUpdateEvent {-# UNPACK #-} !Word32 !BinLogTracker !TableMapEvent !UpdateRowsEvent
  deriving (Show, Eq, Generic)

-- | decode row based event from 'BinLogPacket' stream.
--
-- Throws 'RowEventDecodeException' on an event it cannot decode that may
-- carry row changes, rather than skip it and lose them.
decodeRowBinLogEvent :: (FormatDescription, IORef ByteString, InputStream BinLogPacket)
                     -> IO (InputStream RowBinLogEvent)
decodeRowBinLogEvent (fd, fref, is) = do
    tableMaps <- newIORef []
    Stream.makeInputStream (nextRowEvent fd fref tableMaps is)

-- | The table maps of the statement being read, by table ID. A statement maps
-- every table it changes before its rows events, which then name the table by
-- ID; a long statement splits its rows over several events.
type TableMaps = IORef [(Word64, TableMapEvent)]

nextRowEvent :: FormatDescription -> IORef ByteString -> TableMaps -> InputStream BinLogPacket
             -> IO (Maybe RowBinLogEvent)
nextRowEvent fd fref tableMaps is = do
    p <- Stream.read is
    case p of
        Nothing -> return Nothing
        Just packet -> case blEventType packet of
            BINLOG_ROWS_QUERY_EVENT -> do
                tr <- track packet fref
                e <- getFromBinLogPacket getQueryEvent' packet
                pure (Just (RowQueryEvent (blTimestamp packet) tr e))
            BINLOG_TABLE_MAP_EVENT -> do
                tme <- getFromBinLogPacket (getTableMapEvent fd) packet
                maps <- readIORef tableMaps
                writeIORef tableMaps ((tmTableId tme, tme) : maps)
                nextRowEvent fd fref tableMaps is
            BINLOG_WRITE_ROWS_EVENTv1 -> writeRows fd fref tableMaps packet
            BINLOG_WRITE_ROWS_EVENTv2 -> writeRows fd fref tableMaps packet
            BINLOG_DELETE_ROWS_EVENTv1 -> deleteRows fd fref tableMaps packet
            BINLOG_DELETE_ROWS_EVENTv2 -> deleteRows fd fref tableMaps packet
            BINLOG_UPDATE_ROWS_EVENTv1 -> updateRows fd fref tableMaps packet
            BINLOG_UPDATE_ROWS_EVENTv2 -> updateRows fd fref tableMaps packet
            BINLOG_UNRECOGNIZED_EVENT _ -> if isIgnorableEvent packet
                then nextRowEvent fd fref tableMaps is
                else unsupported packet fref
            -- The rows events of MySQL 5.1.0 to 5.1.15, which this decoder never read.
            BINLOG_WRITE_ROWS_EVENTv0 -> unsupported packet fref
            BINLOG_UPDATE_ROWS_EVENTv0 -> unsupported packet fref
            BINLOG_DELETE_ROWS_EVENTv0 -> unsupported packet fref
            -- Events without row changes.
            BINLOG_UNKNOWN_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_START_EVENT_V3 -> nextRowEvent fd fref tableMaps is
            BINLOG_QUERY_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_STOP_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_ROTATE_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_INTVAR_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_LOAD_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_SLAVE_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_CREATE_FILE_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_APPEND_BLOCK_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_EXEC_LOAD_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_DELETE_FILE_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_NEW_LOAD_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_RAND_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_USER_VAR_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_FORMAT_DESCRIPTION_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_XID_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_BEGIN_LOAD_QUERY_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_EXECUTE_LOAD_QUERY_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_INCIDENT_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_HEARTBEAT_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_IGNORABLE_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_GTID_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_ANONYMOUS_GTID_EVENT -> nextRowEvent fd fref tableMaps is
            BINLOG_PREVIOUS_GTIDS_EVENT -> nextRowEvent fd fref tableMaps is

unsupported :: BinLogPacket -> IORef ByteString -> IO a
unsupported packet fref = do
    tr <- track packet fref
    throwIO (UnsupportedEventType (binLogEventTypeCode (blEventType packet)) tr)

writeRows, deleteRows, updateRows
    :: FormatDescription -> IORef ByteString -> TableMaps -> BinLogPacket -> IO (Maybe RowBinLogEvent)
writeRows fd fref tableMaps packet = do
    (tr, tme) <- rowsEventContext fd fref tableMaps packet
    e <- getFromBinLogPacket' (getWriteRowEvent fd tme) packet
    endStatement tableMaps (writeFlags e)
    pure (Just (RowWriteEvent (blTimestamp packet) tr tme e))
deleteRows fd fref tableMaps packet = do
    (tr, tme) <- rowsEventContext fd fref tableMaps packet
    e <- getFromBinLogPacket' (getDeleteRowEvent fd tme) packet
    endStatement tableMaps (deleteFlags e)
    pure (Just (RowDeleteEvent (blTimestamp packet) tr tme e))
updateRows fd fref tableMaps packet = do
    (tr, tme) <- rowsEventContext fd fref tableMaps packet
    e <- getFromBinLogPacket' (getUpdateRowEvent fd tme) packet
    endStatement tableMaps (updateFlags e)
    pure (Just (RowUpdateEvent (blTimestamp packet) tr tme e))

-- | The position of a rows event and the table map its table ID refers to.
rowsEventContext :: FormatDescription -> IORef ByteString -> TableMaps -> BinLogPacket
                 -> IO (BinLogTracker, TableMapEvent)
rowsEventContext fd fref tableMaps packet = do
    tr <- track packet fref
    tableId <- getFromBinLogPacket (getRowsEventTableId fd (blEventType packet)) packet
    maps <- readIORef tableMaps
    case lookup tableId maps of
        Just tme -> pure (tr, tme)
        Nothing -> throwIO (MissingTableMap tableId tr)

-- | @STMT_END_F@ marks a statement's last rows event; its table maps are not
-- used after it, and table IDs may be reused for other tables later.
endStatement :: TableMaps -> Word16 -> IO ()
endStatement tableMaps rowsFlags =
    when (rowsFlags .&. 0x0001 /= 0) (writeIORef tableMaps [])

track :: BinLogPacket -> IORef ByteString -> IO BinLogTracker
track p fref = BinLogTracker <$> readIORef fref <*> (pure . fromIntegral . blLogPos) p

-- | Why 'decodeRowBinLogEvent' stopped.
data RowEventDecodeException
    = UnsupportedEventType !Word8 !BinLogTracker
      -- ^ An event type mysql-haskell cannot decode that the server did not
      -- mark ignorable, such as MySQL's compressed @TRANSACTION_PAYLOAD_EVENT@
      -- (40), so it may carry row changes.
    | MissingTableMap !Word64 !BinLogTracker
      -- ^ A rows event for a table ID whose table map was not read, as when
      -- the dump started in the middle of a statement.
    deriving (Show, Eq)

instance Exception RowEventDecodeException where
    displayException failure = case failure of
        UnsupportedEventType code tracker ->
            "mysql-haskell: binlog event type " ++ show code ++ " at " ++ show tracker
            ++ " cannot be decoded and is not marked ignorable, so it may hold row"
            ++ " changes. For MySQL's TRANSACTION_PAYLOAD_EVENT (40), turn off"
            ++ " binlog_transaction_compression."
        MissingTableMap tableId tracker ->
            "mysql-haskell: the rows event at " ++ show tracker ++ " is for table ID "
            ++ show tableId ++ ", whose table map event was not read. Start the dump"
            ++ " at a transaction boundary, such as getLastBinLogTracker's position."

-- | Get latest master's binlog filename and position.
--
getLastBinLogTracker :: MySQLConn -> IO (Maybe BinLogTracker)
getLastBinLogTracker conn = do
    (_, is) <- query_ conn "SHOW MASTER STATUS"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just (MySQLText fn : MySQLInt64U pos : _) -> return . Just $ BinLogTracker (encodeUtf8 fn) (fromIntegral pos)
        _                                         -> return Nothing

-- | Return True if binlog_checksum = CRC32. Only for MySQL > 5.6
--
isCheckSumEnabled :: MySQLConn -> IO Bool
isCheckSumEnabled conn = do
    (_, is) <- query_ conn "SHOW GLOBAL VARIABLES LIKE 'binlog_checksum'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "CRC32"] -> return True
        _                           -> return False

-- | Return True if rpl_semi_sync_master_enabled = ON. Only for MySQL > 5.5
--
isSemiSyncEnabled :: MySQLConn -> IO Bool
isSemiSyncEnabled conn = do
    (_, is) <- query_ conn "SHOW VARIABLES LIKE 'rpl_semi_sync_master_enabled'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "ON"] -> return True
        _                        -> return False
