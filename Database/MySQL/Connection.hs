{-# LANGUAGE BangPatterns #-}
{-|
Module      : Database.MySQL.Connection
Description : Connection managment
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This is an internal module, the 'MySQLConn' type should not directly acessed to user.

-}

module Database.MySQL.Connection where

import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Word
import           GHC.Generics
import qualified Z.Crypto.Hash                  as Crypto
import           Z.Crypto.SafeMem
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Builder                 as B
import qualified Z.Data.Text                    as T
import qualified Z.Data.Vector                  as V
import qualified Z.Data.Vector.Extra            as V
import           Z.IO.Network
import           Z.IO
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------

-- | 'MySQLConn' wrap both 'InputStream' and 'OutputStream' for MySQL 'Packet'.
--
-- You shouldn't use one 'MySQLConn' in different thread, if you do that,
-- consider protecting it with a @MVar@.
--
data MySQLConn = MySQLConn
    { mysqlRead        :: !BufferedInput
    , mysqlWrite       :: !BufferedOutput
    , isConsumed       :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Everything you need to establish a MySQL connection.
--
--
data ConnectInfo = ConnectInfo
    { ciConfig   :: Either IPCClientConfig TCPClientConfig
    , ciRecvBufSiz :: Int
    , ciSendBufSiz :: Int
    , ciDatabase :: T.Text
    , ciUser     :: T.Text
    , ciPassword :: T.Text
    , ciCharset  :: Word8
    } deriving Show

-- | A simple 'ConnectInfo' targeting localhost with @user=root@ and empty password.
--
--  Default charset is set to @utf8_general_ci@ to support older(< 5.5.3) MySQL versions,
--  but be aware this is a partial utf8 encoding, you may want to use 'defaultConnectInfoMB4'
--  instead to support full utf8 charset(emoji, etc.). You can query your server's support
--  with @SELECT id, collation_name FROM information_schema.collations ORDER BY id;@
--
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo
    (Right defaultTCPClientConfig{ tcpRemoteAddr = SocketAddrIPv4 ipv4Loopback 3306 })
    defaultChunkSize defaultChunkSize
    "" "root" "" utf8_general_ci

-- | 'defaultConnectInfo' with charset set to @utf8mb4_unicode_ci@
--
-- This is recommanded on any MySQL server version >= 5.5.3.
--
defaultConnectInfoMB4 :: ConnectInfo
defaultConnectInfoMB4 = ConnectInfo
    (Right defaultTCPClientConfig{ tcpRemoteAddr = SocketAddrIPv4 ipv4Loopback 3306 })
    defaultChunkSize defaultChunkSize
    "" "root" "" utf8mb4_unicode_ci

utf8_general_ci :: Word8
utf8_general_ci = 33

utf8mb4_unicode_ci :: Word8
utf8mb4_unicode_ci = 224

--------------------------------------------------------------------------------

-- | Socket buffer size.
--
-- maybe exposed to 'ConnectInfo' laster?
--
bUFSIZE :: Int
bUFSIZE = 16384

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> Resource MySQLConn
connect = fmap snd . connectDetail

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: HasCallStack => ConnectInfo -> Resource (Greeting, MySQLConn)
connectDetail (ConnectInfo conf recvBufSiz sendBufSiz db user pass charset) = do
    uvs <- either initIPCClient initTCPClient conf
    initResource (do
        (bi, bo) <- newBufferedIO' uvs recvBufSiz sendBufSiz
        p <- readPacket bi
        greet <- decodeFromPacket decodeGreeting p
        let auth = mkAuth (T.getUTF8Bytes db) (T.getUTF8Bytes user) (T.getUTF8Bytes pass) charset greet
        writeBuilder bo $ encodePacket (encodeToPacket 1 (encodeAuth auth))
        flushBuffer bo
        _ <- readPacket bi -- OK
        consumed <- newIORef True
        return (greet, MySQLConn bi bo consumed))
        (\ (_, MySQLConn bi bo _) -> writeCommand COM_QUIT bo >> waitNotMandatoryOK bi)
  where
    waitNotMandatoryOK bi = catch
        (void (waitCommandReply bi))           -- server will either reply an OK packet
        ((\ _ -> return ()) :: SomeException -> IO ())   -- or directy close the connection

mkAuth :: V.Bytes -> V.Bytes -> V.Bytes -> Word8 -> Greeting -> Auth
mkAuth db user pass charset greet =
    let salt = greetingSalt1 greet `V.append` greetingSalt2 greet
        plugin = greetingAuthPlugin greet
        scambleBuf = scramble plugin salt pass
    in Auth clientCap clientMaxPacketSize charset user scambleBuf db plugin
  where
    scramble :: V.Bytes -> V.Bytes -> V.Bytes -> V.Bytes
    scramble plugin salt pass'
        | V.null pass' = V.empty
        | otherwise   = case plugin of
            "caching_sha2_password" -> V.zipWith' xor sha2pass salt2
            "mysql_native_password" -> V.zipWith' xor sha1pass salt1
            _ -> ""
        where sha1pass = sha1 pass'
              salt1 = sha1 (salt `V.append` sha1 sha1pass)
              sha2pass = sha2 pass'
              salt2 = sha1 (salt `V.append` sha2 sha2pass)

    sha1 = unCEBytes . Crypto.hash Crypto.SHA160
    sha2 = unCEBytes . Crypto.hash Crypto.SHA256


-- | Send a 'COM_PING'.
--
ping :: MySQLConn -> IO OK
ping = flip command COM_PING

--------------------------------------------------------------------------------
-- helpers

-- | Send a 'Command' which don't return a resultSet.
--
command :: MySQLConn -> Command -> IO OK
command conn@(MySQLConn is os _) cmd = do
    guardUnconsumed conn
    writeCommand cmd os
    waitCommandReply is
{-# INLINE command #-}

waitCommandReply :: HasCallStack => BufferedInput -> IO OK
waitCommandReply is = do
    p <- readPacket is
    if isOK p
    then decodeFromPacket decodeOK p
    else throwIO (UnexpectedPacket p callStack)
{-# INLINE waitCommandReply #-}

waitCommandReplys :: HasCallStack => BufferedInput -> IO [OK]
waitCommandReplys is = do
    p <- readPacket is
    if isOK p
    then do ok <- decodeFromPacket decodeOK p
            if isThereMore ok
            then (ok :) <$> waitCommandReplys is
            else return [ok]
    else throwIO (UnexpectedPacket p callStack)
{-# INLINE waitCommandReplys #-}

-- | Read a 'Packet' from input.
--
-- This function will raise 'ERRException' if 'ERR' packet is met.
readPacket :: HasCallStack => BufferedInput -> IO Packet
readPacket bi = do
    bs <- readExactly 4 bi
    let len =  fromIntegral (bs `V.unsafeIndex` 0)
           .|. fromIntegral (bs `V.unsafeIndex` 1) `unsafeShiftL` 8
           .|. fromIntegral (bs `V.unsafeIndex` 2) `unsafeShiftL` 16
        seqN = bs `V.unsafeIndex` 3
    body <- readExactly len bi
    let p = Packet len seqN body
    when (isERR p) $ do
        err <- decodeFromPacket decodeERR p
        throwIO (ERRException err callStack)
    return p
{-# INLINE readPacket #-}

writeCommand :: Command -> BufferedOutput -> IO ()
writeCommand a bo = do
    let bs = B.buildWith V.smallChunkSize (encodeCommand a)
    go (V.length bs) 0 bs
    flushBuffer bo
  where
    go !len !seqN !bs = do
        if len < 16777215
        then writeBuilder bo $ encodePacket (Packet len seqN bs)
        else do
            let (bs', rest) = V.splitAt 16777215 bs
                seqN' = seqN + 1
                len'  = len - 16777215
            writeBuilder bo $ encodePacket  (Packet 16777215 seqN bs')
            go len' seqN' rest
{-# INLINE writeCommand #-}

guardUnconsumed :: HasCallStack => MySQLConn -> IO ()
guardUnconsumed (MySQLConn _ _ consumed) = do
    c <- readIORef consumed
    unless c (throwIO (UnconsumedResultSet callStack))
{-# INLINE guardUnconsumed #-}

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = x `seq` writeIORef ref x
{-# INLINE writeIORef' #-}

--------------------------------------------------------------------------------
-- Exceptions

data UnconsumedResultSet = UnconsumedResultSet CallStack deriving (Show)
instance Exception UnconsumedResultSet

data ERRException = ERRException ERR CallStack deriving (Show)
instance Exception ERRException

data UnexpectedPacket = UnexpectedPacket Packet CallStack deriving (Show)
instance Exception UnexpectedPacket

