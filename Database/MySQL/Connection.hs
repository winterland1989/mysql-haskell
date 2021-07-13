{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Z.Crypto.PubKey
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
    { ciConnConfig :: Either IPCClientConfig TCPClientConfig
    , ciRecvBufSiz :: Int
    , ciSendBufSiz :: Int
    , ciDatabase :: T.Text
    , ciUser     :: T.Text
    , ciPassword :: T.Text
    , ciCharset  :: Word8
    , ciSecureConf :: Maybe PubKey      -- ^ MySQL 8.0.2 and afterward, the default authentication plugin is @caching_sha2_password@,
                                        -- set this field to the server's public key if you're not using TLS connection and don't want
                                        -- request public key from server over non-secure wire.
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
    V.defaultChunkSize V.defaultChunkSize
    "" "root" "" utf8_general_ci Nothing

-- | 'defaultConnectInfo' with charset set to @utf8mb4_unicode_ci@
--
-- This is recommanded on any MySQL server version >= 5.5.3.
--
defaultConnectInfoMB4 :: ConnectInfo
defaultConnectInfoMB4 = ConnectInfo
    (Right defaultTCPClientConfig{ tcpRemoteAddr = SocketAddrIPv4 ipv4Loopback 3306 })
    V.defaultChunkSize V.defaultChunkSize
    "" "root" "" utf8mb4_unicode_ci Nothing

utf8_general_ci :: Word8
utf8_general_ci = 33

utf8mb4_unicode_ci :: Word8
utf8mb4_unicode_ci = 224

--------------------------------------------------------------------------------

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> Resource MySQLConn
connect = fmap snd . connectDetail

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: HasCallStack => ConnectInfo -> Resource (Greeting, MySQLConn)
connectDetail (ConnectInfo conf recvBufSiz sendBufSiz db user (T.getUTF8Bytes -> pass) charset mpubkey) = do
    uvs <- either initIPCClient initTCPClient conf
    initResource (do
        (bi, bo) <- newBufferedIO' uvs recvBufSiz sendBufSiz
        greet <- decodeFromPacket decodeGreeting =<< readPacket bi
        writePacket bo (encodeToPacket 1 (encodeHandshakeResponse41 (mkAuth41 greet)))
        handleAuthRes greet bi bo
        consumed <- newIORef True
        return (greet, MySQLConn bi bo consumed))
        (\ (_, MySQLConn bi bo _) -> writeCommand COM_QUIT bo >> waitNotMandatoryOK bi)
  where
    waitNotMandatoryOK bi = catch
        (void (waitCommandReply bi))           -- server will either reply an OK packet
        ((\ _ -> return ()) :: SomeException -> IO ())   -- or directy close the connection

    -- | Make a HandshakeResponse41 packet
    mkAuth41 greet =
        let salt = greetingSalt1 greet `V.append` greetingSalt2 greet
            plugin = greetingAuthPlugin greet
            scambleBuf = scramble plugin salt
        in HandshakeResponse41 clientCap clientMaxPacketSize charset user scambleBuf db plugin

    handleAuthRes greet bi bo = do
        authRes <- readPacket bi

        when (isAuthSwitchRequest authRes) $ do
            -- TODO, update greet packet?
            (AuthSwitchRequest plugin salt) <- decodeFromPacket decodeAuthSwitchRequest authRes
            let scambleBuf = scramble plugin salt
            writePacket bo (Packet (V.length scambleBuf) 3 scambleBuf)
            handleAuthRes greet bi bo

        when (isAuthResponse authRes) $ do
            case decodeAuthResponse authRes of
                PerformFullAuthentication -> do
                    -- Request pubkey from server, see https://mariadb.com/kb/en/caching_sha2_password-authentication-plugin/#public-key-request
                    pubkey <- maybe
                        (do let pubkeyRequest = Packet 1 3 (V.singleton 2)
                                decodePubKey = do
                                    P.skipWord8 -- 0x01
                                    P.takeRemaining
                            writePacket bo pubkeyRequest
                            loadPubKey =<< decodeFromPacket decodePubKey =<< readPacket bi) return mpubkey
                    rng <- getRNG
                    let salt = greetingSalt1 greet `V.append` greetingSalt2 greet
                    encryptedPass <- if V.null pass
                        then return V.empty
                        else pkEncrypt pubkey (EME_OAEP SHA160 "") rng (V.zipWith' xor (pass `V.snoc` 0) salt)
                    writePacket bo (Packet 256 5 encryptedPass)
                _ -> return ()
            handleAuthRes greet bi bo

        -- Now by pass OK packet

    scramble :: T.Text -> V.Bytes -> V.Bytes
    scramble plugin salt
        | V.null pass = V.empty
        | otherwise   = case plugin of
            "mysql_native_password" -> let salt1 = sha1 (salt `V.append` sha1 sha1pass) in V.zipWith' xor sha1pass salt1
            "caching_sha2_password" -> let salt2 = sha2 (salt `V.append` sha2 sha2pass) in V.zipWith' xor sha2pass salt2
            _ -> throw (UnsupportedAuthPlugin plugin callStack)

    sha1pass = sha1 pass
    sha2pass = sha2 pass
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
    p@(Packet l _ body) <- packet
    if l < 16777215
    then pure p
    else loopRead l [body]
  where
    loopRead !l acc = do
        (Packet l' seqN body) <- packet
        if l' < 16777215
        then pure (Packet (l + l') seqN (V.concat . reverse $ body:acc))
        else loopRead (l + l') (body:acc)
    packet = do
        bs <- readExactly 4 bi
        let !len =  fromIntegral (bs `V.unsafeIndex` 0)
               .|. fromIntegral (bs `V.unsafeIndex` 1) `unsafeShiftL` 8
               .|. fromIntegral (bs `V.unsafeIndex` 2) `unsafeShiftL` 16
            !seqN = bs `V.unsafeIndex` 3
        body <- readExactly len bi
        let p = Packet len seqN body
        if (isERR p)
        then do
            err <- decodeFromPacket decodeERR p
            throwIO (ERRException err callStack)
        else pure p
{-# INLINE readPacket #-}

writePacket :: HasCallStack => BufferedOutput -> Packet -> IO ()
writePacket bo p = do
    writeBuilder bo (encodePacket p)
    flushBuffer bo

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

data UnsupportedAuthPlugin = UnsupportedAuthPlugin T.Text CallStack deriving (Show)
instance Exception UnsupportedAuthPlugin

