{-# LANGUAGE CPP #-}

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

import           Control.Exception        (Exception, bracketOnError, throwIO)
import           Control.Monad            (unless)
import qualified Crypto.Hash              as Crypto
import qualified Data.Binary              as Binary
import qualified Data.Binary.Put          as Binary
import           Data.Bits
import qualified Data.ByteArray           as BA
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Typeable
import           Data.Word
import           Database.MySQL.Protocol
import           Network.Socket           (HostName, PortNumber)
import qualified Network.Socket           as N
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified System.IO.Streams.TCP    as TCP

--------------------------------------------------------------------------------

data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: {-# UNPACK #-} !(OutputStream Packet)
    ,   mysqlCloseSocket :: IO ()
    ,   isConsumed       :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Everything you need to establish a MySQL connection.
--
data ConnectInfo = ConnectInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciTLSInfo  :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving Show

-- | A simple 'ConnectInfo' targeting localhost with @user=root@ and empty password.
--
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo "127.0.0.1" 3306 "" "root" "" Nothing

--------------------------------------------------------------------------------

-- | socket buffer size.
--
-- maybe exposed to 'ConnectInfo' laster?
--
bUFSIZE :: Int
bUFSIZE = 16384

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> IO MySQLConn
connect ci@(ConnectInfo host port _ _ _ tls) =
    bracketOnError (TCP.connectWithBufferSize host port bUFSIZE)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- Binary.decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            let auth = mkAuth ci greet
            Binary.putToStream (Just (encodeToPacket 1 auth)) os
            q <- readPacket is'
            if isOK q
            then do
                consumed <- newIORef True
                let conn = (MySQLConn is' os' (N.close sock) consumed)
                return conn
            else Stream.write Nothing os' >> decodeFromPacket q >>= throwIO . AuthException
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
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> decodeFromPacket p
        | otherwise -> throwIO (UnexpectedPacket p)
{-# INLINE command #-}

readPacket :: InputStream Packet -> IO Packet
readPacket is = Stream.read is >>= maybe (throwIO NetworkException) (\ p@(Packet len _ bs) ->
        if len < 16777215 then return p else go len [bs]
    )
  where
    go len acc = Stream.read is >>= maybe (throwIO NetworkException) (\ (Packet len' seqN bs) -> do
            let len'' = len + len'
                acc' = bs:acc
            if len' < 16777215
            then return (Packet len'' seqN (L.concat . reverse $ acc'))
            else len'' `seq` go len'' acc'
        )
{-# INLINE readPacket #-}

writeCommand :: Command -> OutputStream Packet -> IO ()
writeCommand a = let bs = Binary.runPut (Binary.put a) in
    go (fromIntegral (L.length bs)) 0 bs
  where
    go len seqN bs =
        if len < 16777215
        then Stream.write (Just (Packet len seqN bs))
        else go (len - 16777215) (seqN + 1) (L.drop 16777215 bs)

guardUnconsumed :: MySQLConn -> IO ()
guardUnconsumed (MySQLConn _ _ _ consumed) = do
    c <- readIORef consumed
    unless c (throwIO UnconsumedResultSet)
{-# INLINE guardUnconsumed #-}

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = x `seq` writeIORef ref x
{-# INLINE writeIORef' #-}

--------------------------------------------------------------------------------
-- default Capability Flags

#define CLIENT_LONG_PASSWORD                  0x00000001
#define CLIENT_FOUND_ROWS                     0x00000002
#define CLIENT_LONG_FLAG                      0x00000004
#define CLIENT_CONNECT_WITH_DB                0x00000008
#define CLIENT_NO_SCHEMA                      0x00000010
#define CLIENT_COMPRESS                       0x00000020
#define CLIENT_ODBC                           0x00000040
#define CLIENT_LOCAL_FILES                    0x00000080
#define CLIENT_IGNORE_SPACE                   0x00000100
#define CLIENT_PROTOCOL_41                    0x00000200
#define CLIENT_INTERACTIVE                    0x00000400
#define CLIENT_SSL                            0x00000800
#define CLIENT_IGNORE_SIGPIPE                 0x00001000
#define CLIENT_TRANSACTIONS                   0x00002000
#define CLIENT_RESERVED                       0x00004000
#define CLIENT_SECURE_CONNECTION              0x00008000
#define CLIENT_MULTI_STATEMENTS               0x00010000
#define CLIENT_MULTI_RESULTS                  0x00020000
#define CLIENT_PS_MULTI_RESULTS               0x00040000
#define CLIENT_PLUGIN_AUTH                    0x00080000
#define CLIENT_CONNECT_ATTRS                  0x00100000
#define CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA 0x00200000

clientCap :: Word32
clientCap =  CLIENT_LONG_PASSWORD
                .|. CLIENT_LONG_FLAG
                .|. CLIENT_CONNECT_WITH_DB
                .|. CLIENT_IGNORE_SPACE
                .|. CLIENT_PROTOCOL_41
                .|. CLIENT_TRANSACTIONS
                .|. CLIENT_SECURE_CONNECTION

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32

-- | Always use @utf8_general_ci@ when connecting mysql server,
-- since this will simplify string decoding.
clientCharset :: Word8
clientCharset = 0x21 :: Word8

--------------------------------------------------------------------------------
-- Exceptions

data NetworkException = NetworkException deriving (Typeable, Show)
instance Exception NetworkException

data UnconsumedResultSet = UnconsumedResultSet deriving (Typeable, Show)
instance Exception UnconsumedResultSet

data ERRException = ERRException ERR deriving (Typeable, Show)
instance Exception ERRException

data AuthException = AuthException ERR deriving (Typeable, Show)
instance Exception AuthException

data UnexpectedPacket = UnexpectedPacket Packet deriving (Typeable, Show)
instance Exception UnexpectedPacket
