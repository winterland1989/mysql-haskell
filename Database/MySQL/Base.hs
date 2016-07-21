{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.MySQL.Base where

import           Control.Exception        (Exception, bracketOnError, throw, throwIO)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as L
import qualified Crypto.Hash              as Crypto
import qualified Data.ByteArray           as BA
import qualified Data.Bits                as Bit
import           Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word8)
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified System.IO.Streams.TCP    as TCP
import qualified Network.Socket as N
import           Network.Socket  (HostName, PortNumber)
import           Database.MySQL.Protocol
import qualified Data.Binary as Binary

--------------------------------------------------------------------------------

data MySqlConn = MySqlConn {
        mysqlRead  :: InputStream  ByteString
    ,   mysqlWrite :: OutputStream ByteString
    ,   mysqlClose :: IO ()
    ,   seqNum     :: IORef Word8
    }

data ConnInfo = ConnInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciTLSInfo  :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving Show

defaultConnectInfo :: ConnInfo
defaultConnectInfo = ConnInfo "localhost" 3306 "" "root" "" Nothing

--------------------------------------------------------------------------------

resetSeqNum :: MySqlConn -> IO ()
resetSeqNum c = writeIORef (seqNum c) 0

connect :: ConnInfo -> IO MySqlConn
connect ci@(ConnInfo host port _ _ _ tls) =
    bracketOnError (TCP.connect host port)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            p <- Binary.decodeFromStream is
            case p of
                Just p' -> do
                    let greet = getFromPacket p' :: Greeting
                        auth = mkAuth ci greet
                    Binary.putToStream (Just (putToPacket auth 1)) os
                    p <- Binary.decodeFromStream is
                    if maybe False isOK p
                    then do
                        ref <- newIORef 0
                        return (MySqlConn is os (N.close sock) ref)
                    else throwIO AuthFail
                Nothing -> throwIO GetPacketFail
  where
    mkAuth :: ConnInfo -> Greeting -> Auth
    mkAuth (ConnInfo _ _ db user pass _) greet =
        let salt = salt1 greet `B.append` salt2 greet
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


close :: MySqlConn -> IO ()
close (MySqlConn _ os close _) = Stream.write Nothing os >> close

data ConnFail = GetPacketFail | AuthFail deriving (Typeable, Show)
instance Exception ConnFail
