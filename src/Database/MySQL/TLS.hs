{-|
Module      : Database.MySQL.Connection
Description : TLS support for mysql-haskell via @tls@ package.
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provides secure MySQL connection using 'tls' package, please make sure your certificate is v3 extension enabled.

-}

module Database.MySQL.TLS (
      connect
    , connectDetail
    , module Data.TLSSetting
    ) where

import           Control.Exception              (bracketOnError, throwIO, catch, SomeException)
import           Control.Monad                  (void)
import qualified Data.Binary                    as Binary
import qualified Data.Binary.Put                as Binary
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Word                      (Word8)
import qualified Data.Connection                as Conn
import           Data.IORef                     (newIORef)
import           Data.TLSSetting
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.Packet
import           System.IO.Streams               (InputStream)
import qualified Network.TLS                    as TLS
import qualified System.IO.Streams.TCP          as TCP
import qualified Data.Connection                as TCP
import qualified System.IO.Streams.TLS          as TLS

--------------------------------------------------------------------------------

-- | Provide a 'TLS.ClientParams' and a subject name to establish a TLS connection.
--
connect :: ConnectInfo -> (TLS.ClientParams, String) -> IO MySQLConn
connect c cp = fmap snd (connectDetail c cp)

connectDetail :: ConnectInfo -> (TLS.ClientParams, String) -> IO (Greeting, MySQLConn)
connectDetail (ConnectInfo host port db user pass charset) (cparams, subName) =
    bracketOnError (connectWithBufferSize host port bUFSIZE)
       (TCP.close) $ \ c -> do
            let is = TCP.source c
            is' <- decodeInputStream is
            p <- readPacket is'
            greet <- decodeFromPacket p
            if supportTLS (greetingCaps greet)
            then do
                let cparams' = cparams {
                            TLS.clientUseServerNameIndication = False
                        ,   TLS.clientServerIdentification = (subName, "")
                        }
                let (sock, sockAddr) = Conn.connExtraInfo c
                write c (encodeToPacket 1 $ sslRequest charset)
                bracketOnError (TLS.contextNew sock cparams')
                               ( \ ctx -> TLS.bye ctx >> TCP.close c ) $ \ ctx -> do
                    TLS.handshake ctx
                    tc <- TLS.tLsToConnection (ctx, sockAddr)
                    let tlsIs = TCP.source tc
                    tlsIs' <- decodeInputStream tlsIs
                    let auth = mkAuth db user pass charset greet
                    write tc (encodeToPacket 2 auth)
                    q <- readPacket tlsIs'
                    completeAuth tlsIs' (write tc) pass q tlsFullAuth
                    consumed <- newIORef True
                    let waitNotMandatoryOK = catch
                            (void (waitCommandReply tlsIs'))
                            ((\ _ -> return ()) :: SomeException -> IO ())
                        conn = MySQLConn tlsIs' (write tc)
                            (writeCommand COM_QUIT (write tc) >> waitNotMandatoryOK >> TCP.close tc)
                            consumed
                    return (greet, conn)
            else error "Database.MySQL.TLS: server doesn't support TLS connection"
  where
    connectWithBufferSize h p bs = TCP.connectSocket h p >>= TCP.socketToConnection bs
    write c a = TCP.send c $ Binary.runPut . Binary.put $ a

-- | Full auth handler for TLS connections: sends the cleartext password
-- as a NUL-terminated packet, which MySQL accepts over encrypted connections.
tlsFullAuth :: Word8 -> ByteString -> (Packet -> IO ()) -> InputStream Packet -> IO ()
tlsFullAuth seqN pass writePacket is = do
    let payload = pass `B.append` "\0"
        body = L.fromStrict payload
        pkt = Packet (fromIntegral (B.length payload)) seqN body
    writePacket pkt
    q <- readPacket is
    if isOK q
        then return ()
        else decodeFromPacket q >>= throwIO . ERRException
