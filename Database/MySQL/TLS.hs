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

module Database.MySQL.TLS where

import           Control.Exception               (bracketOnError,
                                                  throwIO)
import           Control.Monad
import           Data.IORef                      (newIORef)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import qualified Network.Socket                  as N
import qualified Network.TLS                     as TLS
import qualified System.IO.Streams               as Stream
import qualified System.IO.Streams.Binary        as Binary
import qualified System.IO.Streams.TLS           as TLS
import qualified System.IO.Streams.TCP           as TCP
import           Database.MySQL.Connection       hiding (connect, connectDetail)

--------------------------------------------------------------------------------

connect :: ConnectInfo -> TLS.ClientParams -> IO MySQLConn
connect c cp = fmap snd (connectDetail c cp)

connectDetail :: ConnectInfo -> TLS.ClientParams -> IO (Greeting, MySQLConn)
connectDetail ci@(ConnectInfo host port _ _ _) cparams =
    bracketOnError (TCP.connectWithBufferSize host port bUFSIZE)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            if supportTLS (greetingCaps greet)
            then do
                Stream.write (Just (encodeToPacket 1 sslRequest)) os'
                bracketOnError (TLS.contextNew sock cparams) TLS.close $ \ ctx -> do
                    TLS.handshake ctx
                    (tlsIs, tlsOs) <- TLS.tlsToStreams ctx
                    tlsIs' <- decodeInputStream tlsIs
                    tlsOs' <- Binary.encodeOutputStream tlsOs
                    let auth = mkAuth ci greet
                    Stream.write (Just (encodeToPacket 2 auth)) tlsOs'
                    q <- readPacket tlsIs'
                    if isOK q
                    then do
                        consumed <- newIORef True
                        let conn = MySQLConn tlsIs' tlsOs' (TLS.close ctx) consumed
                        return (greet, conn)
                    else Stream.write Nothing tlsOs' >> decodeFromPacket q >>= throwIO . ERRException
            else error "Database.MySQL.TLS: server doesn't support TLS connection"
