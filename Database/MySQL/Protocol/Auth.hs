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

import           Control.Monad
import           Data.Bits
import           Data.Word
import           Database.MySQL.Protocol.Packet
import           GHC.Generics
import           Z.IO.Exception
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V

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
    { greetingProtocol :: {-# UNPACK #-} !Word8
    , greetingVersion  :: {-# UNPACK #-} !T.Text
    , greetingConnId   :: {-# UNPACK #-} !Word32
    , greetingSalt1    :: {-# UNPACK #-} !V.Bytes
    , greetingCaps     :: {-# UNPACK #-} !Word32
    , greetingCharset  :: {-# UNPACK #-} !Word8
    , greetingStatus   :: {-# UNPACK #-} !Word16
    , greetingSalt2    :: {-# UNPACK #-} !V.Bytes
    , greetingAuthPlugin :: {-# UNPACK #-} !T.Text
    } deriving (Show, Eq)

decodeGreeting :: P.Parser Greeting
decodeGreeting = do
    pv <- P.anyWord8
    sv <- T.Text <$> decodeBytesNul
    cid <- P.decodePrimLE @Word32
    salt1 <- P.take 8
    P.skipWord8  -- 0x00
    capL <- P.decodePrimLE @Word16
    charset <- P.anyWord8
    status <- P.decodePrimLE
    capH <- P.decodePrimLE @Word16
    let cap = fromIntegral capH `shiftL` 16 .|. fromIntegral capL
    authPluginLen <- P.anyWord8
    P.skip 10 -- 10 * 0x00A

    salt2 <- if (cap .&. CLIENT_SECURE_CONNECTION) == 0
        then pure V.empty
        else P.take (max 13 (fromIntegral authPluginLen - 8))
                              -- The doc said we should expect a MAX(13, length of auth-plugin-data - 8)
                              -- length bytes, but we have to remove the trailing NULL byte, see below V.init

    authPlugin <- if (cap .&. CLIENT_PLUGIN_AUTH) == 0
        then pure V.empty
        else decodeBytesNul

    return (Greeting pv sv cid salt1 cap charset status (V.init salt2) (T.Text authPlugin))

data HandshakeResponse41 = HandshakeResponse41
    { clientCaps      :: {-# UNPACK #-} !Word32
    , clientMaxPacket :: {-# UNPACK #-} !Word32
    , clientCharset   :: {-# UNPACK #-} !Word8
    , clientName      :: {-# UNPACK #-} !T.Text
    , clientPassword  :: {-# UNPACK #-} !V.Bytes  -- ^ the auth response
    , clientSchema    :: {-# UNPACK #-} !T.Text
    , clientPlugin    :: {-# UNPACK #-} !T.Text
    } deriving (Show, Eq)

encodeHandshakeResponse41 :: HandshakeResponse41 -> B.Builder ()
encodeHandshakeResponse41 (HandshakeResponse41 cap m c n p s plugin) = do
    B.encodePrimLE cap
    B.encodePrimLE m
    B.word8 c
    replicateM_ 23 (B.word8 0x00)
    B.text n >> B.word8 0x00
    B.word8 $ fromIntegral (V.length p)
    B.bytes p
    B.text s
    B.word8 0x00
    B.text plugin
    B.word8 0x00

data AuthResponse = FastAuthSuccess | PerformFullAuthentication deriving (Show, Eq)

isAuthResponse :: Packet -> Bool
isAuthResponse p = pLen p == 2 && V.unsafeIndex (pBody p) 0 == 0x01

decodeAuthResponse :: Packet -> AuthResponse
decodeAuthResponse p = if V.unsafeIndex (pBody p) 1 == 0x03 then FastAuthSuccess else PerformFullAuthentication

data AuthSwitchRequest = AuthSwitchRequest
    { authPluginName :: {-# UNPACK #-} !T.Text
    , authPluginData :: {-# UNPACK #-} !V.Bytes
    } deriving (Show, Eq)

isAuthSwitchRequest :: Packet -> Bool
isAuthSwitchRequest p = V.unsafeIndex (pBody p) 0 == 0xFE

decodeAuthSwitchRequest :: P.Parser AuthSwitchRequest
decodeAuthSwitchRequest = do
    P.skipWord8     -- 0xFE
    n <- decodeBytesNul
    d <- P.takeRemaining
    return (AuthSwitchRequest (T.Text n) (V.take 20 d))

newtype AuthSwitchResponse = AuthSwitchResponse V.Bytes

data SSLRequest = SSLRequest
    { sslReqCaps      :: !Word32
    , sslReqMaxPacket :: !Word32
    , sslReqCharset   :: !Word8
    } deriving (Show, Eq)

decodeSSLRequest :: P.Parser SSLRequest
decodeSSLRequest = SSLRequest <$> P.decodePrimLE <*> P.decodePrimLE <*> P.anyWord8 <* P.skip 23

encodeSSLRequest :: SSLRequest -> B.Builder ()
encodeSSLRequest (SSLRequest cap m c) = do
    B.encodePrimLE cap
    B.encodePrimLE m
    B.word8 c
    replicateM_ 23 (B.word8 0x00)

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
                .|. CLIENT_MULTI_RESULTS
                .|. CLIENT_SECURE_CONNECTION
                .|. CLIENT_PLUGIN_AUTH

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32

supportTLS :: Word32 -> Bool
supportTLS x = (x .&. CLIENT_SSL) /= 0

sslRequest :: Word8 -> SSLRequest
sslRequest charset = SSLRequest (clientCap .|. CLIENT_SSL) clientMaxPacketSize charset
