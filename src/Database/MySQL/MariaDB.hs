{-|
Module      : Database.MySQL.MariaDB
Description : MariaDB-only protocol features
License     : BSD
Stability   : experimental
Portability : PORTABLE

Connections that ask a MariaDB server for the capabilities only MariaDB has.
A 'MariaDBConn' wraps an ordinary 'MySQLConn', so everything in
"Database.MySQL.Base" works on 'mariadbConnection' as before; the functions
here add what the negotiated capabilities make available.

Asking a MySQL server, or a MariaDB server that does not offer a capability,
is not an error: the connection is made without it, exactly as
'Database.MySQL.Base.connect' makes it, and 'mariadbCapabilities' says what
was agreed. Functions that need a capability raise 'CapabilityNotNegotiated'
when it is missing.

@since 1.4.1
-}
module Database.MySQL.MariaDB
    ( -- * Capabilities
      MariaDBCapability(..)
    , ServerFlavour(..)
    , serverFlavour
      -- * Connections
    , MariaDBConn
    , mariadbConnection
    , mariadbGreeting
    , mariadbCapabilities
    , hasCapability
    , connectMariaDB
    , connectMariaDBUnixSocket
    , connectMariaDBTLS
      -- * Extended column metadata
    , ColumnMetadata(..)
    , ExtendedMetadata(..)
    , prepareStmtMetadata
    , getColumnMetadata
      -- * Exceptions
    , CapabilityNotNegotiated(..)
    ) where

import           Control.Exception              (Exception (displayException), throwIO)
import           Control.Monad                  (replicateM, unless, void)
import           Data.Binary.Get                (Get, getWord8, isEmpty)
import           Data.Binary.Parser             (parseOnly)
import           Data.Bits                      ((.&.), (.|.))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.Word                      (Word32, Word8)
import           Database.MySQL.Base
import qualified Database.MySQL.Connection      as Connection
import qualified Database.MySQL.TLS             as TLS
import qualified Network.TLS                    as Network.TLS

-- | A capability only MariaDB servers offer. Only capabilities whose replies
-- this library can read are listed, since asking for one changes what the
-- server sends.
data MariaDBCapability
    = MariaDBExtendedMetadata
      -- ^ MARIADB_CLIENT_EXTENDED_METADATA: column definitions name types
      -- such as JSON or UUID, see 'prepareStmtMetadata'.
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Bits 32 to 63 of the capability flags, see
-- Note [MariaDB extended capabilities] in "Database.MySQL.Protocol.Auth".
capabilityBit :: MariaDBCapability -> Word32
capabilityBit MariaDBExtendedMetadata = 0x08  -- 1 << (35 - 32)

capabilityBits :: [MariaDBCapability] -> ExtendedCapabilities
capabilityBits = ExtendedCapabilities . foldr ((.|.) . capabilityBit) 0

negotiatedCapabilities :: ExtendedCapabilities -> [MariaDBCapability]
negotiatedCapabilities (ExtendedCapabilities bits) =
    filter (\capability -> bits .&. capabilityBit capability /= 0) [minBound .. maxBound]

-- | Which server answered.
data ServerFlavour = MySQLServer | MariaDBServer deriving (Show, Eq)

-- | Every MariaDB release names itself in its version string, for example
-- @11.4.9-MariaDB-log@ or @5.5.5-10.6.12-MariaDB@.
serverFlavour :: Greeting -> ServerFlavour
serverFlavour greeting =
    if "MariaDB" `B.isInfixOf` greetingVersion greeting then MariaDBServer else MySQLServer

-- | A connection together with the MariaDB capabilities it negotiated.
data MariaDBConn = MariaDBConn
    { mariadbConnection   :: !MySQLConn
      -- ^ For everything in "Database.MySQL.Base"; closing it closes the
      -- 'MariaDBConn'.
    , mariadbGreeting     :: !Greeting
    , mariadbCapabilities :: ![MariaDBCapability]
      -- ^ Those asked for that the server offered.
    }

hasCapability :: MariaDBCapability -> MariaDBConn -> Bool
hasCapability capability conn = capability `elem` mariadbCapabilities conn

-- | 'Database.MySQL.Base.connect' asking for these capabilities.
connectMariaDB :: [MariaDBCapability] -> ConnectInfo -> IO MariaDBConn
connectMariaDB requested ci =
    mariadbConn <$> Connection.connectDetailExtended (capabilityBits requested) ci

-- | 'Database.MySQL.Base.connectUnixSocket' asking for these capabilities.
connectMariaDBUnixSocket :: [MariaDBCapability] -> FilePath -> ConnectInfo -> IO MariaDBConn
connectMariaDBUnixSocket requested socketPath ci =
    mariadbConn <$> Connection.connectUnixSocketDetailExtended (capabilityBits requested) socketPath ci

-- | 'Database.MySQL.TLS.connect' asking for these capabilities.
connectMariaDBTLS :: [MariaDBCapability] -> ConnectInfo -> (Network.TLS.ClientParams, String) -> IO MariaDBConn
connectMariaDBTLS requested ci tlsParams =
    mariadbConn <$> TLS.connectDetailExtended (capabilityBits requested) ci tlsParams

mariadbConn :: (Greeting, ExtendedCapabilities, MySQLConn) -> MariaDBConn
mariadbConn (greeting, negotiated, conn) =
    MariaDBConn conn greeting (negotiatedCapabilities negotiated)

-- | A column definition with the MariaDB extended metadata the server sent
-- for it.
data ColumnMetadata = ColumnMetadata
    { columnDefinition :: !ColumnDef
    , columnExtended   :: ![ExtendedMetadata]
      -- ^ Empty for columns of ordinary types.
    } deriving (Show, Eq)

-- | One entry of a column's extended metadata.
data ExtendedMetadata
    = ExtendedTypeName !ByteString
      -- ^ The SQL type behind a generic wire type, e.g. @uuid@ or @inet6@.
    | ExtendedFormat !ByteString
      -- ^ How a value is formatted, e.g. @json@ for a JSON column sent as a
      -- string.
    | ExtendedUnrecognised !Word8 !ByteString
      -- ^ An entry kind newer than this library, kept as sent.
    deriving (Show, Eq)

-- | Parse a column definition from a connection with
-- 'MariaDBExtendedMetadata', or without it, giving no extended metadata.
getColumnMetadata :: Get ColumnMetadata
getColumnMetadata = do
    (column, block) <- getFieldExtended
    case parseOnly getExtendedEntries block of
        Left err      -> fail ("MariaDB extended column metadata: " ++ err)
        Right entries -> pure (ColumnMetadata column entries)

getExtendedEntries :: Get [ExtendedMetadata]
getExtendedEntries = do
    done <- isEmpty
    if done then pure [] else (:) <$> getExtendedEntry <*> getExtendedEntries

getExtendedEntry :: Get ExtendedMetadata
getExtendedEntry = extendedEntry <$> getWord8 <*> getLenEncBytes

extendedEntry :: Word8 -> ByteString -> ExtendedMetadata
extendedEntry kind = if
    | kind == 0 -> ExtendedTypeName
    | kind == 1 -> ExtendedFormat
    | otherwise -> ExtendedUnrecognised kind

-- | 'Database.MySQL.Base.prepareStmtDetail' with the extended metadata of
-- every parameter and result column. Use the returned statement with the
-- usual functions of "Database.MySQL.Base".
--
-- Raises 'CapabilityNotNegotiated' without 'MariaDBExtendedMetadata', as the
-- server then sends no extended metadata to return.
--
-- Decision: offered for prepared statements only. A text query variant needs
-- the result-set reader that "Database.MySQL.Base" keeps internal, and
-- preparing a query is how a caller learns its column types up front anyway.
prepareStmtMetadata :: MariaDBConn -> Query -> IO (StmtPrepareOK, [ColumnMetadata], [ColumnMetadata])
prepareStmtMetadata conn (Query stmt) = do
    requireCapability MariaDBExtendedMetadata conn
    let mysqlConn = mariadbConnection conn
        is = Connection.mysqlRead mysqlConn
    Connection.guardUnconsumed mysqlConn
    Connection.writeCommand (COM_STMT_PREPARE stmt) (Connection.mysqlWrite mysqlConn)
    p <- Connection.readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        prepared@(StmtPrepareOK _ columnCount paramCount _) <- getFromPacket getStmtPrepareOK p
        params <- replicateM paramCount (Connection.readPacket is >>= getFromPacket getColumnMetadata)
        unless (paramCount == 0) (void (Connection.readPacket is))  -- EOF
        columns <- replicateM columnCount (Connection.readPacket is >>= getFromPacket getColumnMetadata)
        unless (columnCount == 0) (void (Connection.readPacket is))  -- EOF
        pure (prepared, params, columns)

requireCapability :: MariaDBCapability -> MariaDBConn -> IO ()
requireCapability capability conn =
    unless (hasCapability capability conn) (throwIO (CapabilityNotNegotiated capability))

-- | A function needed a capability the connection did not negotiate, because
-- it was not asked for in 'connectMariaDB' or the server does not offer it
-- (MySQL servers offer none).
newtype CapabilityNotNegotiated = CapabilityNotNegotiated MariaDBCapability
    deriving Show

instance Exception CapabilityNotNegotiated where
    displayException (CapabilityNotNegotiated capability) =
        "mysql-haskell: this function needs the MariaDB capability " ++ show capability
        ++ ", which the connection did not negotiate. Ask for it in connectMariaDB; "
        ++ "MySQL servers offer no MariaDB capabilities."
