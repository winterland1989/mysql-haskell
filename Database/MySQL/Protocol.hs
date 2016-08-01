{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol
    ( module  Database.MySQL.Protocol.Auth
    , module  Database.MySQL.Protocol.Command
    , module  Database.MySQL.Protocol.ColumnDef
    , module  Database.MySQL.Protocol.Packet
    , module  Database.MySQL.Protocol.MySQLValue
    , clientCap
    , clientMaxPacketSize
    , clientCharset
    ) where

import           Data.Binary
import           Data.Bits
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.MySQLValue

--------------------------------------------------------------------------------
-- default Capability Flags

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

clientCap :: Word32
clientCap =  CLIENT_LONG_PASSWORD
                .|. CLIENT_LONG_FLAG
                .|. CLIENT_CONNECT_WITH_DB
                .|. CLIENT_IGNORE_SPACE
                .|. CLIENT_PROTOCOL_41
                .|. CLIENT_TRANSACTIONS
                .|. CLIENT_SECURE_CONNECTION

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32

-- | Always use @utf8_general_ci@ when connecting mysql server,
-- since this will simplify string decoding.
clientCharset :: Word8
clientCharset = 0x21 :: Word8
