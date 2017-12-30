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
import           Data.Typeable
import           Data.Word
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.Packet
import           Network.Socket                  (HostName, PortNumber)
import           System.IO.Streams               (InputStream)
import qualified System.IO.Streams               as Stream
import qualified System.IO.Streams.TCP           as TCP
import qualified Data.Connection                 as TCP

--------------------------------------------------------------------------------

-- | 'MySQLConn' wrap both 'InputStream' and 'OutputStream' for MySQL 'Packet'.
--
-- You shouldn't use one 'MySQLConn' in different thread, if you do that,
-- consider protecting it with a @MVar@.
--
data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: (Packet -> IO ())
    ,   mysqlCloseSocket :: IO ()
    ,   isConsumed       :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Everything you need to establish a MySQL connection.
--
-- To setup a TLS connection, use module "Database.MySQL.TLS" or "Database.MySQL.OpenSSL".
--
data ConnectInfo = ConnectInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
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
defaultConnectInfo = ConnectInfo "127.0.0.1" 3306 "" "root" "" utf8_general_ci

-- | 'defaultConnectInfo' with charset set to @utf8mb4_unicode_ci@
--
-- This is recommanded on any MySQL server version >= 5.5.3.
--
defaultConnectInfoMB4 :: ConnectInfo
defaultConnectInfoMB4 = ConnectInfo "127.0.0.1" 3306 "" "root" "" utf8mb4_unicode_ci

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
connect :: ConnectInfo -> IO MySQLConn
connect = fmap snd . connectDetail

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: ConnectInfo -> IO (Greeting, MySQLConn)
connectDetail (ConnectInfo host port db user pass charset)
  = bracketOnError open TCP.close go
  where
    open  = connectWithBufferSize host port bUFSIZE
    go c  = do
      let is = TCP.source c
      is' <- decodeInputStream is
      p <- readPacket is'
      greet <- decodeFromPacket p
      let auth = mkAuth db user pass charset greet
      write c $ encodeToPacket 1 auth
      q <- readPacket is'
      if isOK q
      then do
          consumed <- newIORef True
          let conn = MySQLConn is' (write c) (writeCommand COM_QUIT (write c) >> TCP.close c) consumed
          return (greet, conn)
      else TCP.close c >> decodeFromPacket q >>= throwIO . ERRException

    connectWithBufferSize h p bs = TCP.connectSocket h p >>= TCP.socketToConnection bs
    write c a = TCP.send c $ Binary.runPut . Binary.put $ a

mkAuth :: ByteString -> ByteString -> ByteString -> Word8 -> Greeting -> Auth
mkAuth db user pass charset greet =
    let salt = greetingSalt1 greet `B.append` greetingSalt2 greet
        scambleBuf = scramble salt pass
    in Auth clientCap clientMaxPacketSize charset user scambleBuf db
  where
    scramble :: ByteString -> ByteString -> ByteString
    scramble salt pass'
        | B.null pass' = B.empty
        | otherwise   = B.pack (B.zipWith xor sha1pass withSalt)
        where sha1pass = sha1 pass'
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
  where
    loopRead acc 0 _  = return $! L.fromChunks (reverse acc)
    loopRead acc k is' = do
        bs <- Stream.read is'
        case bs of Nothing -> throwIO NetworkException
                   Just bs' -> do let l = fromIntegral (B.length bs')
                                  if l >= k
                                  then do
                                      let (a, rest) = B.splitAt (fromIntegral k) bs'
                                      unless (B.null rest) (Stream.unRead rest is')
                                      return $! L.fromChunks (reverse (a:acc))
                                  else do
                                      let k' = k - l
                                      k' `seq` loopRead (bs':acc) k' is'

-- | Close a MySQL connection.
--
close :: MySQLConn -> IO ()
close (MySQLConn _ _ closeSocket _) = closeSocket

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

writeCommand :: Command -> (Packet -> IO ()) -> IO ()
writeCommand a writePacket = let bs = Binary.runPut (putCommand a) in
    go (fromIntegral (L.length bs)) 0 bs writePacket
  where
    go len seqN bs writePacket' = do
        if len < 16777215
        then writePacket (Packet len seqN bs)
        else do
            let (bs', rest) = L.splitAt 16777215 bs
                seqN' = seqN + 1
                len'  = len - 16777215

            writePacket (Packet 16777215 seqN bs')
            seqN' `seq` len' `seq` go len' seqN' rest writePacket'
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

