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

import           Control.Exception              (bracketOnError, throwIO)
import qualified Data.Binary                    as Binary
import qualified Data.Binary.Put                as Binary
import qualified Data.Connection                as Conn
import           Data.IORef                     (newIORef)
import           Data.TLSSetting
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
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
                    if isOK q
                    then do
                        consumed <- newIORef True
                        let conn = MySQLConn tlsIs' (write c) (TCP.close c) consumed
                        return (greet, conn)
                    else TCP.close c >> decodeFromPacket q >>= throwIO . ERRException
            else error "Database.MySQL.TLS: server doesn't support TLS connection"
  where
    connectWithBufferSize h p bs = TCP.connectSocket h p >>= TCP.socketToConnection bs
    write c a = TCP.send c $ Binary.runPut . Binary.put $ a
