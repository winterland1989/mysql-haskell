{-|
Module      : Database.MySQL.Connection
Description : Alternative TLS support for mysql-haskell via @HsOpenSSL@ package.
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
import           Data.Connection                as Conn
import qualified Data.Binary                    as Binary
import qualified Data.Binary.Put                as Binary
import           Database.MySQL.Connection      hiding (connect, connectDetail)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet
import qualified OpenSSL                        as SSL
import qualified OpenSSL.X509                   as X509
import qualified OpenSSL.Session                as Session
import qualified System.IO.Streams.OpenSSL      as SSL
import qualified System.IO.Streams.TCP          as TCP
import           Data.OpenSSLSetting

--------------------------------------------------------------------------------

-- | Provide a 'Session.SSLContext' and a subject name to establish a TLS connection.
--
connect :: ConnectInfo -> (Session.SSLContext, String) -> IO MySQLConn
connect c cp = fmap snd (connectDetail c cp)

connectDetail :: ConnectInfo -> (Session.SSLContext, String) -> IO (Greeting, MySQLConn)
connectDetail (ConnectInfo host port db user pass charset) (ctx, subname) =
    bracketOnError (connectWithBufferSize host port bUFSIZE) Conn.close $ \ conn -> do
            let is = Conn.source conn
            is' <- decodeInputStream is
            p <- readPacket is'
            greet <- decodeFromPacket p
            if supportTLS (greetingCaps greet)
            then SSL.withOpenSSL $ do
                write conn (encodeToPacket 1 $ sslRequest charset)
                let (sock, sockAddr) = Conn.connExtraInfo conn
                bracketOnError (Session.connection ctx sock)
                               (\ ssl -> do
                                    Session.shutdown ssl Session.Unidirectional
                                    Conn.close conn
                               ) $ \ ssl -> do
                    Session.connect ssl
                    trusted <- Session.getVerifyResult ssl
                    cert <- Session.getPeerCertificate ssl
                    subnames <- maybe (return []) (`X509.getSubjectName` False) cert
                    let cnname = lookup "CN" subnames
                        verified = maybe False (== subname) cnname
                    unless (trusted && verified) (throwIO $ Session.ProtocolError "fail to verify certificate")
                    sconn <- SSL.sslToConnection (ssl, sockAddr)
                    let sis = Conn.source sconn
                        auth = mkAuth db user pass charset greet
                    write sconn (encodeToPacket 2 auth)
                    sis' <- decodeInputStream sis
                    q <- readPacket sis'
                    if isOK q
                    then do
                        consumed <- newIORef True
                        let mconn = MySQLConn sis' (write sconn) (Conn.close sconn) consumed
                        return (greet, mconn)
                    else Conn.close sconn >> decodeFromPacket q >>= throwIO . ERRException
            else error "Database.MySQL.OpenSSL: server doesn't support TLS connection"
  where
    connectWithBufferSize h p bs = TCP.connectSocket h p >>= TCP.socketToConnection bs
    write c a = Conn.send c $ Binary.runPut . Binary.put $ a
