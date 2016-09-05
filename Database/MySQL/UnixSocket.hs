{-|
Module      : Database.MySQL.UnixSocket
Description : Connection managment
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provides secure MySQL connection using 'tls' package, please make sure your certificate is v3 extension enabled.

-}

module Database.MySQL.UnixSocket (
      ConnectInfo(..)
    , connect
    , connectDetail
    , module Data.TLSSetting
    ) where

import           Control.Exception              (bracketOnError, throwIO)
import           Control.Monad
import           Data.IORef                     (newIORef)
import           Data.ByteString                (ByteString)
import           Data.TLSSetting
import           Database.MySQL.Connection      hiding (connect, connectDetail, ConnectInfo)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import qualified Network.Socket                 as N
import qualified Network.TLS                    as TLS
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.Binary       as Binary
import qualified System.IO.Streams.UnixSocket   as UnixSocket

--------------------------------------------------------------------------------

data ConnectInfo = ConnectInfo
    { ciUnixSocket :: String
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    }

--------------------------------------------------------------------------------

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> IO MySQLConn
connect = fmap snd . connectDetail

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: ConnectInfo -> IO (Greeting, MySQLConn)
connectDetail (ConnectInfo usock db user pass) =
    bracketOnError (UnixSocket.connectWithBufferSize usock bUFSIZE)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            let auth = mkAuth db user pass greet
            Stream.write (Just (encodeToPacket 1 auth)) os'
            q <- readPacket is'
            if isOK q
            then do
                consumed <- newIORef True
                let conn = MySQLConn is' os' (N.close sock) consumed
                return (greet, conn)
            else Stream.write Nothing os' >> decodeFromPacket q >>= throwIO . ERRException
