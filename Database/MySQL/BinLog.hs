{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE LambdaCase          #-}

module Database.MySQL.BinLog where

import           Control.Exception        (Exception, bracketOnError, throw, throwIO)
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as L
import qualified Crypto.Hash              as Crypto
import qualified Data.ByteArray           as BA
import qualified Data.Bits                as Bit
import           Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word32)
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified System.IO.Streams.TCP    as TCP
import qualified Network.Socket as N
import           Network.Socket  (HostName, PortNumber)
import           Database.MySQL.Protocol
import           Database.MySQL.BinLogProtocol
import qualified Data.Binary as Binary
import           Database.MySQL.Base


type SlaveID = Word32


registerPesudoSlave :: MySQLConn -> SlaveID -> IO OK
registerPesudoSlave conn sid = command conn (COM_REGISTER_SLAVE sid "" "" "" 0 0 0)

dumpBinLog :: MySQLConn -> SlaveID -> Word32 -> ByteString -> IO (InputStream BinLogEvent)
dumpBinLog conn@(MySQLConn is os sock isConsumed) sid pos fn = do
    guardUnconsumed conn
    checksum <- isCheckSumEnabled conn
    semi <- isSemiSyncEnabled conn
    writeCommand (COM_BINLOG_DUMP pos 0x00 sid fn) os
    writeIORef isConsumed False
    Stream.makeInputStream $ do
        p <- readPacket is
        if  | isOK  p -> do
                binevt <- getFromPacket (getBinLogEvent checksum semi) p
                return (Just binevt)
            | isEOF p -> return Nothing
            | isERR p -> decodeFromPacket p >>= throwIO . ERRException


-- | Return True if binlog-checksum = CRC32. Only for MySQL > 5.6
--
isCheckSumEnabled :: MySQLConn -> IO Bool
isCheckSumEnabled conn = do
    (_, is) <- query conn "SHOW GLOBAL VARIABLES LIKE 'BINLOG_CHECKSUM'"
    row <- Stream.read is
    Stream.skipToEof is
    case row of
        Just (TextRow bs) -> do
            let tail = L.drop (L.length bs - 5) bs
            if tail == "CRC32"
            then do
                query_ conn "set @master_binlog_checksum= @@global.binlog_checksum"
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
        Just (TextRow bs) -> do
            let tail = L.drop (L.length bs - 2) bs
            if tail == "ON" then return True else return False
        Nothing -> return False

