{-|
Module      : Database.MySQL.Connection
Description : Connection managment
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provides secure MySQL connection using 'HsOpenSSL' package.

-}

module Database.MySQL.OpenSSL where

import           Control.Exception              (bracketOnError, throwIO)
import           Control.Monad
import           Data.IORef                     (newIORef)
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import qualified Network.Socket                 as N
import qualified OpenSSL                        as SSL
import qualified OpenSSL.Session                as Session
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.Binary       as Binary
import qualified System.IO.Streams.OpenSSL      as SSL
import qualified System.IO.Streams.TCP          as TCP

--------------------------------------------------------------------------------

connect :: ConnectInfo -> Session.SSLContext -> IO MySQLConn
connect c cp = fmap snd (connectDetail c cp)

connectDetail :: ConnectInfo -> Session.SSLContext -> IO (Greeting, MySQLConn)
connectDetail ci@(ConnectInfo host port _ _ _) ctx =
    bracketOnError (TCP.connectWithBufferSize host port bUFSIZE)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            if supportTLS (greetingCaps greet)
            then SSL.withOpenSSL $ do
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
                        let conn = MySQLConn sslIs' sslOs' (SSL.close ssl) consumed
                        return (greet, conn)
                    else Stream.write Nothing sslOs' >> decodeFromPacket q >>= throwIO . ERRException
            else error "Database.MySQL.OpenSSL: server doesn't support TLS connection"
