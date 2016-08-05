{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.MySQL.BinLog where

import           Control.Exception                         (Exception, throwIO)
import           Control.Monad
import           Data.Binary.Put
import           Data.ByteString                           (ByteString)
import           Data.IORef                                (IORef, newIORef, readIORef, writeIORef)
import           Data.Typeable
import           Data.Word
import           Data.Text.Encoding                        (encodeUtf8)
import           Database.MySQL.Base
import           Database.MySQL.BinLogProtocol.BinLogEvent
import           Database.MySQL.Connection
import           Database.MySQL.Protocol
import           System.IO.Streams                         (InputStream, OutputStream)
import qualified System.IO.Streams                         as Stream


type SlaveID = Word32

-- | This is a mutable object tracking current consuming binlog's filename and position.
-- It aslo provide an IO action to automatically reply semi-ack packet to master,
-- If 'dumpBinLog' doesn't sucessfully enable semi-ack, this will be a no-op, otherwise
-- you must call it when you decide you have done with current binlog item
-- and ready for next one.
--
data BinLogTracker = BinLogTracker
    { btCurrentFile :: {-# UNPACK #-} !(IORef ByteString)
    , btCurrentPos  :: {-# UNPACK #-} !(IORef Word64)
    , btReplyAck    :: IO ()
    }

-- | Register a pesudo slave to master, although MySQL document suggests you should call this
-- before calling 'dumpBinLog', but it seems it's not really necessary.
--
registerPesudoSlave :: MySQLConn -> SlaveID -> IO OK
registerPesudoSlave conn sid = command conn (COM_REGISTER_SLAVE sid "" "" "" 0 0 0)

-- | Setup binlog listening on given connection, during listening
-- the connection *can not* be used to do query, or an 'UnconsumedResultSet' will be thrown.
--
dumpBinLog :: MySQLConn               -- ^ connection to be listened
           -> SlaveID                 -- ^ a number for our pesudo slave.
           -> (ByteString, Word32)    -- ^ binlog position
           -> Bool                    -- ^ if master support semi-ack, do we want to enable it?
                                      -- if master doesn't support, this parameter will be ignored.
           -> IO (FormatDescription, BinLogTracker, InputStream BinLogPacket)
dumpBinLog conn@(MySQLConn is os _ consumed) sid (initfn, initpos) wantAck = do
    guardUnconsumed conn
    checksum <- isCheckSumEnabled conn
    when checksum $ void $ executeRaw conn "SET @master_binlog_checksum = @@global.binlog_checksum"
    semiAck  <- isSemiSyncEnabled conn
    let needAck = semiAck && wantAck
    when needAck . void $ executeRaw conn "SET @rpl_semi_sync_slave = 1"
    writeCommand (COM_BINLOG_DUMP initpos 0x00 sid initfn) os
    writeIORef consumed False
    p <- readBinLogPacket checksum needAck is
    case p of
        Just p' -> do
            fileRef <- newIORef initfn
            logPosRef <- newIORef (blLogPos p')
            semiAckRef <- newIORef (blSemiAck p')

            let replyAck = if needAck then do ack <- readIORef semiAckRef
                                              when ack $ do
                                                fn <- readIORef fileRef
                                                pos <- readIORef logPosRef
                                                Stream.write (Just (makeSemiAckPacket pos fn)) os
                                      else return ()
                tracker = BinLogTracker fileRef logPosRef replyAck

            if blEventType p' == BINLOG_FORMAT_DESCRIPTION_EVENT
            then do
                fmt <- getFromBinLogPacket getFormatDescription p'
                es <- Stream.makeInputStream $ do
                    p'' <- readBinLogPacket checksum needAck is
                    case p'' of
                        Nothing   -> writeIORef consumed True >> return p''
                        Just p''' -> do if blEventType p''' == BINLOG_ROTATE_EVENT
                                        then do
                                            rotateE <- getFromBinLogPacket getRotateEvent p'''
                                            writeIORef' logPosRef (rPos rotateE)
                                            writeIORef' semiAckRef (blSemiAck p''')
                                            writeIORef' fileRef (rFileName rotateE)
                                        else do
                                            writeIORef' logPosRef (blLogPos p''')
                                            writeIORef' semiAckRef (blSemiAck p''')
                                        return p''
                return (fmt, tracker, es)
            else throwIO (UnexpectedBinLogEvent p')
        Nothing -> throwIO NetworkException
  where
    makeSemiAckPacket pos fn = putToPacket 0 $ do
        putWord8 0xEF      -- semi-ack
        putWord64le pos
        putByteString fn

    readBinLogPacket checksum needAck is' = do
        p <- readPacket is'
        if  | isOK p -> do
                p' <- getFromPacket (getBinLogPacket checksum needAck) p
                if  | isFakeBinLogEvent p'            -> readBinLogPacket checksum needAck is'
                    | otherwise                       -> return (Just p')
            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException

-- | Row based biblog event type.
-- It's recommended to call 'enableRowQueryEvent' before 'dumpBinLog', so that you can get
-- query event in row based binlog(for exampleit's important for detect a table change).
--
data RowBinLogEvent
    = RowQueryEvent    {-# UNPACK #-} !QueryEvent
    | RowDeleteEvent   {-# UNPACK #-} !TableMapEvent !DeleteRowsEvent
    | RowWriteEvent    {-# UNPACK #-} !TableMapEvent !WriteRowsEvent
    | RowUpdateEvent   {-# UNPACK #-} !TableMapEvent !UpdateRowsEvent
  deriving (Show, Eq)

-- | decode row based event from 'BinLogPacket' stream.
decodeRowBinLogEvent :: FormatDescription -> InputStream BinLogPacket -> IO (InputStream RowBinLogEvent)
decodeRowBinLogEvent fd' is' = Stream.makeInputStream (loop fd' is')
  where
    loop fd is = do
        p <- Stream.read is
        case p of
            Nothing -> return Nothing
            Just p' -> do
                let t = blEventType p'
                if  | t == BINLOG_QUERY_EVENT -> Just . RowQueryEvent <$> getFromBinLogPacket getQueryEvent p'
                    | t == BINLOG_TABLE_MAP_EVENT -> do
                        tme <- getFromBinLogPacket (getTableMapEvent fd) p'
                        q <- Stream.read is
                        case q of
                            Nothing -> return Nothing
                            Just q' -> do
                                let u = blEventType q'
                                if  | u == BINLOG_WRITE_ROWS_EVENTv1 || u == BINLOG_WRITE_ROWS_EVENTv2 ->
                                         Just . RowWriteEvent tme <$>
                                            getFromBinLogPacket' (getWriteRowEvent fd tme) q'
                                    | u == BINLOG_DELETE_ROWS_EVENTv1 || u == BINLOG_DELETE_ROWS_EVENTv2 ->
                                         Just . RowDeleteEvent tme <$>
                                            getFromBinLogPacket' (getDeleteRowEvent fd tme) q'
                                    | u == BINLOG_UPDATE_ROWS_EVENTv1 || u == BINLOG_UPDATE_ROWS_EVENTv2 ->
                                         Just . RowUpdateEvent tme <$>
                                            getFromBinLogPacket' (getUpdateRowEvent fd tme) q'
                                    | otherwise -> loop fd is
                    | otherwise -> loop fd is

-- | Get latest master's binlog filename and position.
--
getLastBinLogInfo :: MySQLConn -> IO (ByteString, Word64)
getLastBinLogInfo conn = do
    (_, is) <- queryRaw conn "SHOW MASTER STATUS;"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just (MySQLText fn : MySQLInt64U pos : _) -> return (encodeUtf8 fn, pos)
        row'                                       -> throwIO (GetLastBinLogInfoFail row')

data GetLastBinLogInfoFail = GetLastBinLogInfoFail (Maybe [MySQLValue]) deriving (Show, Typeable)
instance Exception GetLastBinLogInfoFail

-- | Return True if binlog_checksum = CRC32. Only for MySQL > 5.6
--
isCheckSumEnabled :: MySQLConn -> IO Bool
isCheckSumEnabled conn = do
    (_, is) <- queryRaw conn "SHOW GLOBAL VARIABLES LIKE 'binlog_checksum';"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "CRC32"] -> return True
        _                           -> return False

-- | Return True if rpl_semi_sync_master_enabled = ON. Only for MySQL > 5.5
--
isSemiSyncEnabled :: MySQLConn -> IO Bool
isSemiSyncEnabled conn = do
    (_, is) <- queryRaw conn "SHOW VARIABLES LIKE 'rpl_semi_sync_master_enabled';"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "ON"] -> return True
        _                        -> return False

-- | Set session varible @binlog_rows_query_log_events@ to @ON@, so that
-- we can get query event in row based binlog.
--
enableRowQueryEvent :: MySQLConn -> IO ()
enableRowQueryEvent conn = void $ executeRaw conn "SET @binlog_rows_query_log_events = ON;"

