{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.MySQL.BinLog where

import           Control.Exception             (throwIO)
import           Control.Monad
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import           Data.IORef                    (writeIORef)
import           Data.Typeable                 (Typeable)
import           Data.Word                     (Word32)
import           Database.MySQL.Base
import           Database.MySQL.BinLogProtocol
import           Database.MySQL.Protocol
import           System.IO.Streams             (InputStream, OutputStream)
import qualified System.IO.Streams             as Stream
import qualified System.IO.Streams.Binary      as Binary
import qualified System.IO.Streams.TCP         as TCP


type SlaveID = Word32


registerPesudoSlave :: MySQLConn -> SlaveID -> IO OK
registerPesudoSlave conn sid = command conn (COM_REGISTER_SLAVE sid "" "" "" 0 0 0)

dumpBinLog :: MySQLConn -> SlaveID -> Word32 -> ByteString -> IO (FormatDescription, InputStream BinLogEvent)
dumpBinLog conn@(MySQLConn is os _ consumed) sid pos fn = do
    guardUnconsumed conn
    checksum <- isCheckSumEnabled conn
    writeCommand (COM_BINLOG_DUMP pos 0x00 sid fn) os
    writeIORef consumed False
    p <- loop checksum is
    case p of
        Just p' -> do
            if bleEventType p' == FORMAT_DESCRIPTION_EVENT
            then do
                fmt <- getFromBinLogEvent getFormatDescription p'
                es <- Stream.makeInputStream (loop checksum is)
                return (fmt, es)
            else throwIO (UnexpectedBinLogEvent p')
        Nothing -> throwIO NetworkException
  where
    loop checksum is = do
        p <- readPacket is
        if  | isOK p -> do
                p' <- getFromPacket (getBinLogEvent checksum False) p
                if  | isFakeBinLogEvent p'            -> loop checksum is
                    | otherwise                       -> return (Just p')
            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException
{-
    loop checksum semi is os pos fn = do
        p <- readPacket is
        if  | isOK p -> do
                p' <- getFromPacket (getBinLogEvent checksum semi) p

                when (bleSemiAck p') $ do
                    let ackResp = putToPacket (putSemiAckResp pos fn) (pSeqN p)
                    Stream.write (Just ackResp) os

                if  | isFakeBinLogEvent p' -> loop checksum semi is os pos fn
                    | bleEventType p' == ROTATE_EVENT -> do
                        loop checksum semi is os pos fn

                    | otherwise   -> return (Just p')

            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException
-}
-- | Return True if binlog-checksum = CRC32. Only for MySQL > 5.6
--
isCheckSumEnabled :: MySQLConn -> IO Bool
isCheckSumEnabled conn = do
    (_, is) <- query conn "SHOW GLOBAL VARIABLES LIKE 'BINLOG_CHECKSUM'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just (TextRow rows) ->
            if last rows == Just "CRC32"
            then do
                query_ conn "SET @master_binlog_checksum= @@global.binlog_checksum"
                return True
            else return False
        Nothing -> return False

-- | Return True if rpl_semi_sync_master_enabled = ON. Only for MySQL > 5.5
--
isSemiSyncEnabled :: MySQLConn -> IO Bool
isSemiSyncEnabled conn = do
    (_, is) <- query conn "SHOW VARIABLES LIKE 'rpl_semi_sync_master_enabled'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just (TextRow rows) ->
            if last rows == Just "ON"
            then do
                query_ conn "SET @rpl_semi_sync_slave = 1"
                return True
            else return False
        Nothing -> return False
