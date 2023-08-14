{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | This module provides convenience functions for interfacing raw tcp.
--
-- Please use 'E.bracket' or its friends to ensure exception safety.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import           "Data.Connection"
-- import qualified "System.IO.Streams.TCP" as TCP
-- @
--
module System.IO.Streams.TCP
  ( TCPConnection
    -- * client
  , connect
  , connectSocket
  , socketToConnection
  , defaultChunkSize
    -- * server
  , bindAndListen
  , bindAndListenWith
  , accept
  , acceptWith
  ) where

import qualified Control.Exception         as E
import           Control.Monad
import           Data.Connection
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy.Internal as L
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import qualified Network.Socket.ByteString.Lazy as NL
import qualified System.IO.Streams         as S
import           Foreign.Storable   (sizeOf)

addrAny :: N.HostAddress
#if MIN_VERSION_network(2,7,0)
addrAny = N.tupleToHostAddress (0,0,0,0)
#else
addrAny = N.iNADDR_ANY
#endif

-- | Type alias for tcp connection.
--
-- Normally you shouldn't use 'N.Socket' in 'connExtraInfo' directly, this field is
-- intend for used with 'N.setSocketOption' if you need to.
--
type TCPConnection = Connection (N.Socket, N.SockAddr)

-- | The chunk size used for I\/O, less the memory management overhead.
--
-- Currently set to 32k.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
    k = 1024
    chunkOverhead = 2 * sizeOf (undefined :: Int)

-- | Initiating an raw TCP connection to the given @('HostName', 'PortNumber')@ combination.
--
-- It use 'N.getAddrInfo' to resolve host/service name
-- with 'N.AI_ADDRCONFIG', 'N.AI_NUMERICSERV' hint set, so it should be able to
-- resolve both numeric IPv4/IPv6 hostname and domain name.
--
-- `TCP_NODELAY` are enabled by default. you can use 'N.setSocketOption' to adjust.
--
connectSocket :: N.HostName             -- ^ hostname to connect to
              -> N.PortNumber           -- ^ port number to connect to
              -> IO (N.Socket, N.SockAddr)
connectSocket host port = do
    (family, socketType, protocol, addr) <- resolveAddrInfo host port
    E.bracketOnError (N.socket family socketType protocol)
                     N.close
                     (\sock -> do N.connect sock addr
                                  N.setSocketOption sock N.NoDelay 1
                                  return (sock, addr)
                     )
  where
    resolveAddrInfo host' port' = do
        -- Partial function here OK, network will throw an exception rather than
        -- return the empty list here.
        (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host') (Just $ show port')
        let family     = N.addrFamily addrInfo
        let socketType = N.addrSocketType addrInfo
        let protocol   = N.addrProtocol addrInfo
        let addr    = N.addrAddress addrInfo
        return (family, socketType, protocol, addr)
      where
        hints = N.defaultHints {
                N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
            ,   N.addrSocketType = N.Stream
            }
    {-# INLINABLE resolveAddrInfo #-}

-- | Make a 'Connection' from a 'Socket' with given buffer size.
--
socketToConnection
    :: Int                      -- ^ receive buffer size
    -> (N.Socket, N.SockAddr)   -- ^ socket address pair
    -> IO TCPConnection
socketToConnection bufsiz (sock, addr) = do
    is <- S.makeInputStream $ do
        s <- NB.recv sock bufsiz
        return $! if B.null s then Nothing else Just s
    return (Connection is (send' sock) (N.close sock) (sock, addr))
  where
    send' _    (L.Empty) = return ()
    send' sock' (L.Chunk bs L.Empty) = unless (B.null bs) (NB.sendAll sock' bs)
    send' sock' lbs = NL.sendAll sock' lbs

-- | Connect to server using 'defaultChunkSize'.
--
connect :: N.HostName             -- ^ hostname to connect to
        -> N.PortNumber           -- ^ port number to connect to
        -> IO TCPConnection
connect host port = connectSocket host port >>= socketToConnection defaultChunkSize

-- | Bind and listen on port with a limit on connection count.
--
-- This function will set @SO_REUSEADDR@, @TCP_NODELAY@ before binding.
--
bindAndListen :: Int                 -- ^ connection limit
              -> N.PortNumber        -- ^ port number
              -> IO N.Socket
bindAndListen = bindAndListenWith $ \ sock -> do
    N.setSocketOption sock N.ReuseAddr 1
    N.setSocketOption sock N.NoDelay 1

-- | Bind and listen on port with a limit on connection count.
--
-- Note: The following socket options are inherited by a connected TCP socket from the listening socket:
--
-- @
-- SO_DEBUG
-- SO_DONTROUTE
-- SO_KEEPALIVE
-- SO_LINGER
-- SO_OOBINLINE
-- SO_RCVBUF
-- SO_RCVLOWAT
-- SO_SNDBUF
-- SO_SNDLOWAT
-- TCP_MAXSEG
-- TCP_NODELAY
-- @
--
bindAndListenWith :: (N.Socket -> IO ()) -- ^ set socket options before binding
                  -> Int                 -- ^ connection limit
                  -> N.PortNumber        -- ^ port number
                  -> IO N.Socket
bindAndListenWith f maxc port =
    E.bracketOnError (N.socket N.AF_INET N.Stream 0)
                     N.close
                     (\sock -> do f sock
                                  N.bind sock (N.SockAddrInet port addrAny)
                                  N.listen sock maxc
                                  return sock
                     )

-- | Accept a connection with 'defaultChunkSize'.
--
accept :: N.Socket -> IO TCPConnection
accept = acceptWith (socketToConnection defaultChunkSize)

-- | Accept a connection with user customization.
--
acceptWith :: ((N.Socket, N.SockAddr) -> IO TCPConnection) -- ^ set socket options, adjust receive buffer, etc.
           -> N.Socket
           -> IO TCPConnection
acceptWith f = f <=< N.accept
