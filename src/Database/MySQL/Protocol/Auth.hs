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

-- MariaDB reuses the obsolete CLIENT_LONG_PASSWORD bit to mean "MySQL client
-- or server". See Note [MariaDB extended capabilities].
#define CLIENT_MYSQL                          CLIENT_LONG_PASSWORD

{- Note [MariaDB extended capabilities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MariaDB has capabilities beyond the 32 bits of the MySQL handshake
(MARIADB_CLIENT_* in mariadb-connector-c's mariadb_com.h, bits 32 to 36). A
MariaDB server leaves CLIENT_MYSQL out of its greeting and puts the upper 32
bits in the last four of the greeting's ten reserved bytes. A client answers
with its own upper bits in the last four of the 23 reserved bytes of the SSL
request and the handshake response, and must leave CLIENT_MYSQL out as well,
or the server ignores those bytes.

mysql-haskell sets CLIENT_MYSQL by default and writes zeroes there, so a
MariaDB server treats it as a MySQL client. Only a connection that asks for an
extended capability, to a server that offers it, sends anything different:
'negotiateExtended' yields 'noExtendedCapabilities' otherwise, and every
encoder below writes exactly the bytes it wrote before extended capabilities
existed. A MySQL server always sets CLIENT_MYSQL, so it never sees the change.
-}

-- | MariaDB's capabilities above bit 31, shifted down by 32, so bit 0 here is
-- MARIADB_CLIENT_PROGRESS. See Note [MariaDB extended capabilities].
--
-- @since 1.4.0
newtype ExtendedCapabilities = ExtendedCapabilities Word32 deriving (Show, Eq)

-- | What every connection sent before extended capabilities existed.
--
-- @since 1.4.0
noExtendedCapabilities :: ExtendedCapabilities
noExtendedCapabilities = ExtendedCapabilities 0

-- | The extended capabilities a connection uses: those the client asked for
-- that the server offered, or none when the server is not a MariaDB server
-- reading them.
--
-- @since 1.4.0
negotiateExtended :: ExtendedCapabilities  -- ^ requested by the client
                  -> Greeting
                  -> ExtendedCapabilities  -- ^ offered in that greeting
                  -> ExtendedCapabilities
negotiateExtended (ExtendedCapabilities requested) greeting (ExtendedCapabilities offered) =
    if offersExtendedCapabilities greeting
    then ExtendedCapabilities (requested .&. offered)
    else noExtendedCapabilities

-- | Whether the greeting comes from a server speaking MariaDB's protocol
-- extensions: MariaDB 10.2 and later leave CLIENT_MYSQL out of it, MySQL and
-- older MariaDB set it. See Note [MariaDB extended capabilities].
--
-- @since 1.4.0
offersExtendedCapabilities :: Greeting -> Bool
offersExtendedCapabilities greeting = greetingCaps greeting .&. CLIENT_MYSQL == 0

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
getGreeting = fst <$> getGreetingExtended

-- | 'getGreeting' together with the extended capabilities a MariaDB server
-- offers; MySQL servers send zeroes there.
--
-- @since 1.4.0
getGreetingExtended :: Get (Greeting, ExtendedCapabilities)
getGreetingExtended = do
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
    _authPluginLen <- getWord8   -- this will issue an unused warning, see the notes below
    skipN 6
    extended <- ExtendedCapabilities <$> getWord32le
    salt2 <- if (cap .&. CLIENT_SECURE_CONNECTION) == 0
        then pure B.empty
        else getByteStringNul   -- This is different with the MySQL document here
                                -- The doc said we should expect a MAX(13, length of auth-plugin-data - 8)
                                -- length bytes, but doing so stop us from login
                                -- anyway 'getByteStringNul' works perfectly here.

    authPlugin <- if (cap .&. CLIENT_PLUGIN_AUTH) == 0
        then pure B.empty
        else getByteStringNul

    return (Greeting pv sv cid salt1 cap charset status salt2 authPlugin, extended)

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
    , authPlugin    :: !ByteString
    } deriving (Show, Eq)

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skipN 23
    n <- getByteStringNul
    return $ Auth a m c n B.empty B.empty B.empty

putAuth :: Auth -> Put
putAuth = putAuthExtended noExtendedCapabilities

-- | 'putAuth' carrying negotiated extended capabilities; 'authCaps' must then
-- leave CLIENT_MYSQL out, see 'clientCapFor'.
--
-- @since 1.4.0
putAuthExtended :: ExtendedCapabilities -> Auth -> Put
putAuthExtended (ExtendedCapabilities extended) (Auth cap m c n p s plugin) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 19 (putWord8 0x00)
    putWord32le extended
    putByteString n >> putWord8 0x00
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s
    putWord8 0x00
    putByteString plugin
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
putSSLRequest = putSSLRequestExtended noExtendedCapabilities

-- | 'putSSLRequest' carrying negotiated extended capabilities. MariaDB reads
-- them from the SSL request too, before the TLS handshake.
--
-- @since 1.4.0
putSSLRequestExtended :: ExtendedCapabilities -> SSLRequest -> Put
putSSLRequestExtended (ExtendedCapabilities extended) (SSLRequest cap m c) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 19 (putWord8 0x00)
    putWord32le extended

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
                .|. CLIENT_MULTI_RESULTS
                .|. CLIENT_SECURE_CONNECTION
                .|. CLIENT_PLUGIN_AUTH

-- | 'clientCap' for a connection using these extended capabilities.
--
-- @since 1.4.0
clientCapFor :: ExtendedCapabilities -> Word32
clientCapFor negotiated =
    if negotiated == noExtendedCapabilities
    then clientCap
    else clientCap .&. complement CLIENT_MYSQL

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32


supportTLS :: Word32 -> Bool
supportTLS x = (x .&. CLIENT_SSL) /= 0

sslRequest :: Word8 -> SSLRequest
sslRequest = sslRequestFor noExtendedCapabilities

-- | 'sslRequest' for a connection using these extended capabilities; encode
-- it with 'putSSLRequestExtended'.
--
-- @since 1.4.0
sslRequestFor :: ExtendedCapabilities -> Word8 -> SSLRequest
sslRequestFor negotiated charset =
    SSLRequest (clientCapFor negotiated .|. CLIENT_SSL) clientMaxPacketSize charset
