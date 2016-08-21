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

import           Control.Exception               (Exception, bracketOnError,
                                                  throwIO)
import           Control.Monad
import qualified Crypto.Hash                     as Crypto
import qualified Data.Binary                     as Binary
import qualified Data.Binary.Put                 as Binary
import           Data.Bits
import qualified Data.ByteArray                  as BA
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Unsafe          as B
import           Data.IORef                      (IORef, newIORef, readIORef,
                                                  writeIORef)
import qualified Data.TLSSetting                 as TLS
import           Data.Typeable
import           Data.Word
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.Packet
import           Network.Socket                  (HostName, PortNumber)
import qualified Network.Socket                  as N
import qualified OpenSSL.Session                 as Session
import           System.IO.Streams               (InputStream, OutputStream)
import qualified System.IO.Streams               as Stream
import qualified System.IO.Streams.Binary        as Binary
import qualified System.IO.Streams.TCP           as TCP
import qualified System.IO.Streams.OpenSSL       as SSL
import Data.Maybe

--------------------------------------------------------------------------------

data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: {-# UNPACK #-} !(OutputStream Packet)
    ,   mysqlCloseSocket :: IO ()
    ,   isConsumed       :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Everything you need to establish a MySQL connection.
--
-- You may want some helpers in "System.IO.Streams.TLS" to setup TLS connection.
--
data ConnectInfo = ConnectInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciTLSInfo  :: Maybe Session.SSLContext -- ^ If 'Session.SSLContext' is provided, and server support
                                             -- TLS, then TLS connection will be used, currently the only
                                             -- TLS implementation supported is openssl's binding, If someone
                                             -- can get tls package work, please send me a patch.
    }

-- | A simple 'ConnectInfo' targeting localhost with @user=root@ and empty password.
--
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo "127.0.0.1" 3306 "" "root" "" Nothing

--------------------------------------------------------------------------------

-- | Socket buffer size.
--
-- maybe exposed to 'ConnectInfo' laster?
--
bUFSIZE :: Int
bUFSIZE = 16384

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> IO MySQLConn
connect = fmap snd . connectDetail

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: ConnectInfo -> IO (Greeting, MySQLConn)
connectDetail ci@(ConnectInfo host port _ _ _ tls) =
    bracketOnError (TCP.connectWithBufferSize host port bUFSIZE)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            if supportTLS (greetingCaps greet) && isJust tls
            then SSL.withOpenSSL $ do
                let Just ctx = tls
                Stream.write (Just (encodeToPacket 1 sslRequest)) os'
                bracketOnError (Session.connection ctx sock) SSL.close $ \ ssl -> do
                    Session.connect ssl
                    (sslIs, sslOs) <- SSL.sslToStreams ssl
                    sslIs' <- decodeInputStream sslIs
                    sslOs' <- Binary.encodeOutputStream sslOs
                    let auth = mkAuth ci greet
                    Stream.write (Just (encodeToPacket 2 auth)) sslOs'
                    q <- readPacket sslIs'
                    if isOK q
                    then do
                        consumed <- newIORef True
                        let conn = (MySQLConn sslIs' sslOs' (SSL.close ssl) consumed)
                        return (greet, conn)
                    else Stream.write Nothing sslOs' >> decodeFromPacket q >>= throwIO . ERRException
            else do
                let auth = mkAuth ci greet
                Stream.write (Just (encodeToPacket 1 auth)) os'
                q <- readPacket is'
                if isOK q
                then do
                    consumed <- newIORef True
                    let conn = (MySQLConn is' os' (N.close sock) consumed)
                    return (greet, conn)
                else Stream.write Nothing os' >> decodeFromPacket q >>= throwIO . ERRException
  where
    mkAuth :: ConnectInfo -> Greeting -> Auth
    mkAuth (ConnectInfo _ _ db user pass _) greet =
        let salt = greetingSalt1 greet `B.append` greetingSalt2 greet
            scambleBuf = scramble salt pass
        in Auth clientCap clientMaxPacketSize clientCharset user scambleBuf db

    scramble :: ByteString -> ByteString -> ByteString
    scramble salt pass
        | B.null pass = B.empty
        | otherwise   = B.pack (B.zipWith xor sha1pass withSalt)
        where sha1pass = sha1 pass
              withSalt = sha1 (salt `B.append` sha1 sha1pass)

    sha1 :: ByteString -> ByteString
    sha1 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA1)

    -- | A specialized 'decodeInputStream' here for speed
    decodeInputStream :: InputStream ByteString -> IO (InputStream Packet)
    decodeInputStream is = Stream.makeInputStream $ do
        bs <- Stream.readExactly 4 is
        let len =  fromIntegral (bs `B.unsafeIndex` 0)
               .|. fromIntegral (bs `B.unsafeIndex` 1) `shiftL` 8
               .|. fromIntegral (bs `B.unsafeIndex` 2) `shiftL` 16
            seqN = bs `B.unsafeIndex` 3
        body <- loopRead [] len is
        return . Just $ Packet len seqN body

    loopRead acc 0 _  = return $! L.fromChunks (reverse acc)
    loopRead acc k is = do
        bs <- Stream.read is
        case bs of Nothing -> throwIO NetworkException
                   Just bs' -> do let l = fromIntegral (B.length bs')
                                  if l >= k
                                  then do
                                      let (a, rest) = B.splitAt (fromIntegral k) bs'
                                      unless (B.null rest) (Stream.unRead rest is)
                                      return $! L.fromChunks (reverse (a:acc))
                                  else do
                                      let k' = k - l
                                      k' `seq` loopRead (bs':acc) k' is

-- | Close a MySQL connection.
--
close :: MySQLConn -> IO ()
close (MySQLConn _ os closeSocket _) = do
    Stream.write Nothing os
    closeSocket

-- | Send a 'COM_PING'.
--
ping :: MySQLConn -> IO OK
ping = flip command COM_PING

--------------------------------------------------------------------------------
-- helpers

-- | Send a 'Command' which don't return a resultSet.
--
command :: MySQLConn -> Command -> IO OK
command conn@(MySQLConn is os _ _) cmd = do
    guardUnconsumed conn
    writeCommand cmd os
    waitCommandReply is
{-# INLINE command #-}

waitCommandReply :: InputStream Packet -> IO OK
waitCommandReply is = do
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> decodeFromPacket p
        | otherwise -> throwIO (UnexpectedPacket p)
{-# INLINE waitCommandReply #-}

readPacket :: InputStream Packet -> IO Packet
readPacket is = Stream.read is >>= maybe
    (throwIO NetworkException)
    (\ p@(Packet len _ bs) -> if len < 16777215 then return p else go len [bs])
  where
    go len acc = Stream.read is >>= maybe
        (throwIO NetworkException)
        (\ (Packet len' seqN bs) -> do
            let len'' = len + len'
                acc' = bs:acc
            if len' < 16777215
            then return (Packet len'' seqN (L.concat . reverse $ acc'))
            else len'' `seq` go len'' acc'
        )
{-# INLINE readPacket #-}

writeCommand :: Command -> OutputStream Packet -> IO ()
writeCommand a os = let bs = Binary.runPut (putCommand a) in
    go (fromIntegral (L.length bs)) 0 bs os
  where
    go len seqN bs os' = do
        if len < 16777215
        then Stream.write (Just (Packet len seqN bs)) os'
        else do
            let (bs', rest) = L.splitAt 16777215 bs
                seqN' = seqN + 1
                len'  = len - 16777215

            Stream.write (Just (Packet 16777215 seqN bs')) os'
            seqN' `seq` len' `seq` go len' seqN' rest os'
{-# INLINE writeCommand #-}

guardUnconsumed :: MySQLConn -> IO ()
guardUnconsumed (MySQLConn _ _ _ consumed) = do
    c <- readIORef consumed
    unless c (throwIO UnconsumedResultSet)
{-# INLINE guardUnconsumed #-}

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = x `seq` writeIORef ref x
{-# INLINE writeIORef' #-}

--------------------------------------------------------------------------------
-- Exceptions

data NetworkException = NetworkException deriving (Typeable, Show)
instance Exception NetworkException

data UnconsumedResultSet = UnconsumedResultSet deriving (Typeable, Show)
instance Exception UnconsumedResultSet

data ERRException = ERRException ERR deriving (Typeable, Show)
instance Exception ERRException

data UnexpectedPacket = UnexpectedPacket Packet deriving (Typeable, Show)
instance Exception UnexpectedPacket
