{-# LANGUAGE OverloadedStrings #-}

module Database.MySQL.Connection where

import           Control.Monad            (unless)
import           Control.Exception        (bracketOnError, throwIO)
import           System.IO.Streams        (InputStream, OutputStream)
import           Data.IORef               (IORef, readIORef, newIORef)
import           Database.MySQL.Protocol
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteArray           as BA
import qualified Data.Bits                as Bit
import qualified Crypto.Hash              as Crypto
import           Network.Socket           (HostName, PortNumber)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified Data.Binary.Put          as Binary
import qualified Data.Binary              as Binary
import qualified System.IO.Streams.TCP    as TCP
import qualified Network.Socket           as N

--------------------------------------------------------------------------------

data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: {-# UNPACK #-} !(OutputStream Packet)
    ,   mysqlCloseSocket :: IO ()
    ,   isConsumed       :: {-# UNPACK #-} !(IORef Bool)
    }

-- | Everything you need to establish a MySQL connection.
--
data ConnInfo = ConnInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciTLSInfo  :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving Show

-- | A simple 'ConnInfo' targeting localhost with @user=root@ and empty password.
--
defaultConnectInfo :: ConnInfo
defaultConnectInfo = ConnInfo "127.0.0.1" 3306 "" "root" "" Nothing

--------------------------------------------------------------------------------

-- | Establish a MySQL connection.
--
connect :: ConnInfo -> IO MySQLConn
connect ci@(ConnInfo host port _ _ _ tls) =
    bracketOnError (TCP.connect host port)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- Binary.decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            let auth = mkAuth ci greet
            Binary.putToStream (Just (encodeToPacket auth 1)) os
            p <- readPacket is'
            if isOK p
            then do
                consumed <- newIORef True
                return (MySQLConn is' os' (N.close sock) consumed)
            else Stream.write Nothing os' >> decodeFromPacket p >>= throwIO . AuthException
  where
    mkAuth :: ConnInfo -> Greeting -> Auth
    mkAuth (ConnInfo _ _ db user pass _) greet =
        let salt = greetingSalt1 greet `B.append` greetingSalt2 greet
            scambleBuf = scramble salt pass
        in Auth clientCap clientMaxPacketSize clientCharset user scambleBuf db

    scramble :: ByteString -> ByteString -> ByteString
    scramble salt pass
        | B.null pass = B.empty
        | otherwise   = B.pack (B.zipWith Bit.xor sha1pass withSalt)
        where sha1pass = sha1 pass
              withSalt = sha1 (salt `B.append` sha1 sha1pass)

    sha1 :: ByteString -> ByteString
    sha1 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA1)

-- | Close a MySQL connection.
--
close :: MySQLConn -> IO ()
close (MySQLConn _ os closeSocket _) = Stream.write Nothing os >> closeSocket

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
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else decodeFromPacket p

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
            else go len'' acc'
        )

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
