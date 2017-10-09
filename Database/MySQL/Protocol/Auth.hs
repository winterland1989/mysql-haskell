{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.Protocol.Auth
Description : MySQL Auth Packets
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Auth related packet.

-}

module Database.MySQL.Protocol.Auth where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Parser
import           Data.Binary.Put
import qualified Data.ByteString                as B
import           Data.ByteString.Char8          as BC
import           Data.Bits
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
-- Authentications

#define CLIENT_LONG_PASSWORD                  0x00000001
#define CLIENT_FOUND_ROWS                     0x00000002
#define CLIENT_LONG_FLAG                      0x00000004
#define CLIENT_CONNECT_WITH_DB                0x00000008
#define CLIENT_NO_SCHEMA                      0x00000010
#define CLIENT_COMPRESS                       0x00000020
#define CLIENT_ODBC                           0x00000040
#define CLIENT_LOCAL_FILES                    0x00000080
#define CLIENT_IGNORE_SPACE                   0x00000100
#define CLIENT_PROTOCOL_41                    0x00000200
#define CLIENT_INTERACTIVE                    0x00000400
#define CLIENT_SSL                            0x00000800
#define CLIENT_IGNORE_SIGPIPE                 0x00001000
#define CLIENT_TRANSACTIONS                   0x00002000
#define CLIENT_RESERVED                       0x00004000
#define CLIENT_SECURE_CONNECTION              0x00008000
#define CLIENT_MULTI_STATEMENTS               0x00010000
#define CLIENT_MULTI_RESULTS                  0x00020000
#define CLIENT_PS_MULTI_RESULTS               0x00040000
#define CLIENT_PLUGIN_AUTH                    0x00080000
#define CLIENT_CONNECT_ATTRS                  0x00100000
#define CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA 0x00200000

data Greeting = Greeting
    { greetingProtocol :: !Word8
    , greetingVersion  :: !B.ByteString
    , greetingConnId   :: !Word32
    , greetingSalt1    :: !B.ByteString
    , greetingCaps     :: !Word32
    , greetingCharset  :: !Word8
    , greetingStatus   :: !Word16
    , greetingSalt2    :: !B.ByteString
    , greetingAuthPlugin :: !B.ByteString
    } deriving (Show, Eq)

putGreeting :: Greeting -> Put
putGreeting (Greeting pv sv cid salt1 cap charset st salt2 authPlugin) = do
    putWord8 pv
    putByteString sv
    putWord8 0x00
    putWord32le cid
    putByteString salt1
    let capL = fromIntegral cap .|. 0xFF
        capH = fromIntegral (cap `shiftR` 16) .|. 0xFF
    putWord16le capL
    putWord8 charset
    putWord16le st
    putWord16le capH
    putWord8 (fromIntegral $ B.length salt2)
    replicateM_ 10 (putWord8 0x00)
    when (cap .&. CLIENT_SECURE_CONNECTION /= 0)
        (putByteString salt2)
    when (cap .&. CLIENT_PLUGIN_AUTH /= 0)
        (putByteString authPlugin)

getGreeting :: Get Greeting
getGreeting = do
    pv <- getWord8
    sv <- getByteStringNul
    cid <- getWord32le
    salt1 <- getByteString 8
    skipN 1  -- 0x00
    capL <- getWord16le
    charset <- getWord8
    status <- getWord16le
    capH <- getWord16le
    let cap = fromIntegral capH `shiftL` 16 .|. fromIntegral capL
    authPluginLen <- getWord8   -- this will issue an unused warning, see the notes below
    skipN 10 -- 10 * 0x00
    salt2 <- if (cap .&. CLIENT_SECURE_CONNECTION) == 0
        then pure B.empty
        else getByteStringNul   -- This is different with the MySQL document here
                                -- The doc said we should expect a MAX(13, length of auth-plugin-data - 8)
                                -- length bytes, but doing so stop us from login
                                -- anyway 'getByteStringNul' works perfectly here.

    authPlugin <- if (cap .&. CLIENT_PLUGIN_AUTH) == 0
        then pure B.empty
        else getByteStringNul

    return (Greeting pv sv cid salt1 cap charset status salt2 authPlugin)

instance Binary Greeting where
    get = getGreeting
    put = putGreeting

data Auth = Auth
    { authCaps      :: !Word32
    , authMaxPacket :: !Word32
    , authCharset   :: !Word8
    , authName      :: !ByteString
    , authPassword  :: !ByteString
    , authSchema    :: !ByteString
    } deriving (Show, Eq)

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skipN 23
    n <- getByteStringNul
    return $ Auth a m c n B.empty B.empty

putAuth :: Auth -> Put
putAuth (Auth cap m c n p s) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 23 (putWord8 0x00)
    putByteString n >> putWord8 0x00
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s
    putWord8 0x00

instance Binary Auth where
    get = getAuth
    put = putAuth

data SSLRequest = SSLRequest
    { sslReqCaps      :: !Word32
    , sslReqMaxPacket :: !Word32
    , sslReqCharset   :: !Word8
    } deriving (Show, Eq)

getSSLRequest :: Get SSLRequest
getSSLRequest = SSLRequest <$> getWord32le <*> getWord32le <*> getWord8 <* skipN 23

putSSLRequest :: SSLRequest -> Put
putSSLRequest (SSLRequest cap m c) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 23 (putWord8 0x00)

instance Binary SSLRequest where
    get = getSSLRequest
    put = putSSLRequest

--------------------------------------------------------------------------------
-- default Capability Flags

clientCap :: Word32
clientCap =  CLIENT_LONG_PASSWORD
                .|. CLIENT_LONG_FLAG
                .|. CLIENT_CONNECT_WITH_DB
                .|. CLIENT_IGNORE_SPACE
                .|. CLIENT_PROTOCOL_41
                .|. CLIENT_TRANSACTIONS
                .|. CLIENT_MULTI_STATEMENTS
                .|. CLIENT_SECURE_CONNECTION

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32


supportTLS :: Word32 -> Bool
supportTLS x = (x .&. CLIENT_SSL) /= 0

sslRequest :: Word8 -> SSLRequest
sslRequest charset = SSLRequest (clientCap .|. CLIENT_SSL) clientMaxPacketSize charset
