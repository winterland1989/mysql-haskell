{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.MySQL.BinLog where

import           Control.Exception             (throwIO)
import           Control.Monad
import           Data.ByteString               (ByteString)
import           Data.IORef                    (writeIORef)
import           Data.Word                     (Word32)
import           Database.MySQL.Base
import           Database.MySQL.Connection
import           Database.MySQL.BinLogProtocol.BinLogEvent
import           Database.MySQL.Protocol
import           System.IO.Streams             (InputStream, OutputStream)
import qualified System.IO.Streams             as Stream


type SlaveID = Word32

registerPesudoSlave :: MySQLConn -> SlaveID -> IO OK
registerPesudoSlave conn sid = command conn (COM_REGISTER_SLAVE sid "" "" "" 0 0 0)

dumpBinLog :: MySQLConn -> SlaveID -> Word32 -> ByteString -> IO (FormatDescription, InputStream BinLogPacket)
dumpBinLog conn@(MySQLConn is os _ consumed) sid pos fn = do
    guardUnconsumed conn
    checksum <- isCheckSumEnabled conn
    writeCommand (COM_BINLOG_DUMP pos 0x00 sid fn) os
    writeIORef consumed False
    p <- loop checksum is
    case p of
        Just p' -> do
            if blEventType p' == FORMAT_DESCRIPTION_EVENT
            then do
                fmt <- getFromBinLogPacket getFormatDescription p'
                es <- Stream.makeInputStream (loop checksum is)
                return (fmt, es)
            else throwIO (UnexpectedBinLogEvent p')
        Nothing -> throwIO NetworkException
  where
    loop checksum is = do
        p <- readPacket is
        if  | isOK p -> do
                p' <- getFromPacket (getBinLogPacket checksum False) p
                if  | isFakeBinLogEvent p'            -> loop checksum is
                    | otherwise                       -> return (Just p')
            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException

isCheckSumEnabled :: MySQLConn -> IO Bool
isCheckSumEnabled conn = do
    (_, is) <- query conn "SHOW GLOBAL VARIABLES LIKE 'BINLOG_CHECKSUM'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "CRC32"] -> do
                execute conn "SET @master_binlog_checksum= @@global.binlog_checksum"
                return True
        _ -> return False

-- | Return True if rpl_semi_sync_master_enabled = ON. Only for MySQL > 5.5
--
isSemiSyncEnabled :: MySQLConn -> IO Bool
isSemiSyncEnabled conn = do
    (_, is) <- query conn "SHOW VARIABLES LIKE 'rpl_semi_sync_master_enabled'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just [_, MySQLText "ON"] -> do
            execute conn "SET @rpl_semi_sync_slave = 1"
            return True
        _ -> return False
