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
    , connectDetailExtended
    , connectWithLimits
    , connectDetailWithLimits
    , connectDetailExtendedWithLimits
    , module Data.TLSSetting
    ) where

import           Control.Exception              (bracketOnError, throwIO)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Word                      (Word8)
import qualified Data.Connection                as Conn
import           Data.TLSSetting
import           Data.TLSSetting.ServerIdentification (identifyServer)
import           Database.MySQL.Connection      hiding (connect, connectDetail, connectDetailExtended,
                                                         connectWithLimits, connectDetailWithLimits,
                                                         connectDetailExtendedWithLimits)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import           System.IO.Streams               (InputStream)
import qualified Network.TLS                    as TLS
import qualified System.IO.Streams.TCP          as TCP
import qualified Data.Connection                as TCP
import qualified System.IO.Streams.TLS          as TLS

--------------------------------------------------------------------------------

-- | Provide a 'TLS.ClientParams' and a subject name to establish a TLS connection.
--
-- The server's certificate must be valid for the subject name. Since 1.6.0 the
-- subject name is also sent as the server name indication (SNI) unless it is
-- an IP address or the 'TLS.ClientParams' turn SNI off.
--
connect :: ConnectInfo -> (TLS.ClientParams, String) -> IO MySQLConn
connect = connectWithLimits defaultConnectionLimits

connectDetail :: ConnectInfo -> (TLS.ClientParams, String) -> IO (Greeting, MySQLConn)
connectDetail = connectDetailWithLimits defaultConnectionLimits

-- | 'connectDetail' asking a MariaDB server for extended capabilities, also
-- returning those the server agreed to. Asking for none gives exactly
-- 'connectDetail'. See Note [MariaDB extended capabilities].
--
-- @since 1.4.0
connectDetailExtended :: ExtendedCapabilities -> ConnectInfo -> (TLS.ClientParams, String)
                      -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectDetailExtended = connectDetailExtendedWithLimits defaultConnectionLimits

-- | 'connect' refusing what the server sends beyond these limits, see
-- 'ConnectionLimits'.
--
-- @since 1.6.0
connectWithLimits :: ConnectionLimits -> ConnectInfo -> (TLS.ClientParams, String) -> IO MySQLConn
connectWithLimits limits connectInfo tlsParams = fmap snd (connectDetailWithLimits limits connectInfo tlsParams)

-- | 'connectDetail' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectDetailWithLimits :: ConnectionLimits -> ConnectInfo -> (TLS.ClientParams, String) -> IO (Greeting, MySQLConn)
connectDetailWithLimits limits connectInfo tlsParams =
    fmap withoutNegotiated (connectDetailExtendedWithLimits limits noExtendedCapabilities connectInfo tlsParams)

-- | 'connectDetailExtended' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectDetailExtendedWithLimits :: ConnectionLimits -> ExtendedCapabilities -> ConnectInfo
                                -> (TLS.ClientParams, String) -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectDetailExtendedWithLimits limits requested connectInfo tlsParams =
    bracketOnError
        (TCP.connectSocket (ciHost connectInfo) (ciPort connectInfo) >>= TCP.socketToConnection bUFSIZE)
        TCP.close
        (handshakeTLS limits requested connectInfo tlsParams)

handshakeTLS :: ConnectionLimits -> ExtendedCapabilities -> ConnectInfo -> (TLS.ClientParams, String)
             -> TCP.TCPConnection -> IO (Greeting, ExtendedCapabilities, MySQLConn)
handshakeTLS limits requested (ConnectInfo _host _port db user pass charset) (cparams, subName) c = do
    is' <- decodeInputStreamWithLimits limits (TCP.source c)
    p <- readPacket is'
    (greet, offered) <- getFromPacket getGreetingExtended p
    let negotiated = negotiateExtended requested greet offered
    if supportTLS (greetingCaps greet)
    then do
        let cparams' = identifyServer subName "" cparams
        let (sock, sockAddr) = Conn.connExtraInfo c
        sendPacket c (sslRequestPacket negotiated charset)
        bracketOnError (TLS.contextNew sock cparams')
                       ( \ ctx -> TLS.bye ctx >> TCP.close c ) $ \ ctx -> do
            TLS.handshake ctx
            tc <- TLS.tLsToConnection (ctx, sockAddr)
            tlsIs' <- decodeInputStreamWithLimits limits (TCP.source tc)
            sendPacket tc (handshakeResponsePacket 2 negotiated db user pass charset greet)
            q <- readPacket tlsIs'
            completeAuth tlsIs' (sendPacket tc) pass q tlsFullAuth
            conn <- authenticatedConnection tlsIs' tc
            return (greet, negotiated, conn)
    else error "Database.MySQL.TLS: server doesn't support TLS connection"

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
