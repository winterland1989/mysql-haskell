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

module Database.MySQL.OpenSSL
    ( connect
    , connectDetail
    , module Data.OpenSSLSetting
    ) where

import           Control.Exception              (bracketOnError, throwIO)
import           Control.Monad
import           Data.IORef                     (newIORef)
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import qualified Network.Socket                 as N
import qualified OpenSSL                        as SSL
import qualified OpenSSL.X509                   as X509
import qualified OpenSSL.Session                as Session
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.Binary       as Binary
import qualified System.IO.Streams.OpenSSL      as SSL
import qualified System.IO.Streams.TCP          as TCP
import           Data.OpenSSLSetting

--------------------------------------------------------------------------------

-- | Provide a 'Session.SSLContext' and a subject name to establish a TLS connection.
--
connect :: ConnectInfo -> (Session.SSLContext, String) -> IO MySQLConn
connect c cp = fmap snd (connectDetail c cp)

connectDetail :: ConnectInfo -> (Session.SSLContext, String) -> IO (Greeting, MySQLConn)
connectDetail ci@(ConnectInfo host port _ _ _) (ctx, subname) =
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
                    trusted <- Session.getVerifyResult ssl
                    cert <- Session.getPeerCertificate ssl
                    subnames <- maybe (return []) (`X509.getSubjectName` False) cert
                    let cnname = lookup "CN" subnames
                        verified = maybe False (== subname) cnname
                    unless (trusted && verified) (throwIO $ Session.ProtocolError "fail to verify certificate")
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
