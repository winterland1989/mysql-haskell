{-|
Module      : Database.MariaDB
Description : MariaDB-only protocol features
License     : BSD
Stability   : experimental
Portability : PORTABLE

Features only MariaDB servers have, such as extended column metadata and bulk
execution. They rely on MariaDB's own protocol extensions, so this module
works with MariaDB servers only: its connect functions refuse a server that
does not speak them, MySQL or MariaDB before 10.2, with 'NotMariaDB'.

The functions here take a 'MariaDBConn', so they cannot be given a connection
to MySQL. A 'MariaDBConn' wraps an ordinary 'MySQLConn': use
'mysqlConnection' for everything in "Database.MySQL.Base".

A MariaDB server that does not offer a requested capability is not an error.
The connection is made without it, 'mariadbCapabilities' says what was agreed,
and a function that needs the missing capability raises
'CapabilityNotNegotiated'.

@since 1.4.0
-}
module Database.MariaDB
    ( -- * Capabilities
      MariaDBCapability(..)
      -- * Connections
    , MariaDBConn
    , mysqlConnection
    , mariadbGreeting
    , mariadbCapabilities
    , connectMariaDB
    , connectMariaDBUnixSocket
    , connectMariaDBTLS
    , connectMariaDBWithLimits
    , connectMariaDBUnixSocketWithLimits
    , connectMariaDBTLSWithLimits
      -- * Extended column metadata
    , ColumnMetadata(..)
    , ExtendedMetadata(..)
    , prepareStmtMetadata
      -- * Bulk execution
    , BulkParam(..)
    , executeBulk
    , BulkExecuteError(..)
      -- * Exceptions
    , NotMariaDB(..)
    , CapabilityNotNegotiated(..)
    ) where

import           Control.Exception              (Exception (displayException), throwIO)
import           Control.Monad                  (forM_, replicateM, unless, void)
import           Data.Binary.Get                (Get, getWord8, isEmpty)
import           Data.Binary.Put                (Put, putLazyByteString, putWord16le, putWord32le,
                                                 putWord8, runPut)
import           Data.Binary.Parser             (parseOnly)
import           Data.Bits                      ((.&.), (.|.))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as L
import           Data.List                      (nub, transpose)
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import qualified Data.List.NonEmpty             as NonEmpty
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
    | MariaDBBulkOperations
      -- ^ MARIADB_CLIENT_STMT_BULK_OPERATIONS: 'executeBulk'.
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Bits 32 to 63 of the capability flags, see
-- Note [MariaDB extended capabilities] in "Database.MySQL.Protocol.Auth".
capabilityBit :: MariaDBCapability -> Word32
capabilityBit MariaDBExtendedMetadata = 0x08  -- 1 << (35 - 32)
capabilityBit MariaDBBulkOperations   = 0x04  -- 1 << (34 - 32)

capabilityBits :: [MariaDBCapability] -> ExtendedCapabilities
capabilityBits = ExtendedCapabilities . foldr ((.|.) . capabilityBit) 0

negotiatedCapabilities :: ExtendedCapabilities -> [MariaDBCapability]
negotiatedCapabilities (ExtendedCapabilities bits) =
    filter (\capability -> bits .&. capabilityBit capability /= 0) [minBound .. maxBound]

-- | A connection together with the MariaDB capabilities it negotiated.
data MariaDBConn = MariaDBConn
    { mysqlConnection     :: !MySQLConn
      -- ^ The underlying connection, for everything in "Database.MySQL.Base";
      -- closing it closes the 'MariaDBConn'.
    , mariadbGreeting     :: !Greeting
    , mariadbCapabilities :: ![MariaDBCapability]
      -- ^ Those asked for that the server offered.
    }

hasCapability :: MariaDBCapability -> MariaDBConn -> Bool
hasCapability capability conn = capability `elem` mariadbCapabilities conn

-- | 'Database.MySQL.Base.connect' asking for these capabilities. Raises
-- 'NotMariaDB', after closing the connection, when the server does not speak
-- MariaDB's protocol extensions (MySQL, or MariaDB before 10.2).
connectMariaDB :: [MariaDBCapability] -> ConnectInfo -> IO MariaDBConn
connectMariaDB = connectMariaDBWithLimits Connection.defaultConnectionLimits

-- | 'Database.MySQL.Base.connectUnixSocket' asking for these capabilities, and
-- refusing a MySQL server as 'connectMariaDB' does.
connectMariaDBUnixSocket :: [MariaDBCapability] -> FilePath -> ConnectInfo -> IO MariaDBConn
connectMariaDBUnixSocket = connectMariaDBUnixSocketWithLimits Connection.defaultConnectionLimits

-- | 'Database.MySQL.TLS.connect' asking for these capabilities, and refusing a
-- MySQL server as 'connectMariaDB' does.
connectMariaDBTLS :: [MariaDBCapability] -> ConnectInfo -> (Network.TLS.ClientParams, String) -> IO MariaDBConn
connectMariaDBTLS = connectMariaDBTLSWithLimits Connection.defaultConnectionLimits

-- | 'connectMariaDB' refusing what the server sends beyond these limits, see
-- t'Database.MySQL.Connection.ConnectionLimits'.
--
-- @since 1.6.0
connectMariaDBWithLimits :: Connection.ConnectionLimits -> [MariaDBCapability] -> ConnectInfo -> IO MariaDBConn
connectMariaDBWithLimits limits requested connectInfo =
    Connection.connectDetailExtendedWithLimits limits (capabilityBits requested) connectInfo >>= mariadbConn

-- | 'connectMariaDBUnixSocket' refusing what the server sends beyond these
-- limits.
--
-- @since 1.6.0
connectMariaDBUnixSocketWithLimits :: Connection.ConnectionLimits -> [MariaDBCapability] -> FilePath -> ConnectInfo
                                   -> IO MariaDBConn
connectMariaDBUnixSocketWithLimits limits requested socketPath connectInfo =
    Connection.connectUnixSocketDetailExtendedWithLimits limits (capabilityBits requested) socketPath connectInfo
    >>= mariadbConn

-- | 'connectMariaDBTLS' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectMariaDBTLSWithLimits :: Connection.ConnectionLimits -> [MariaDBCapability] -> ConnectInfo
                            -> (Network.TLS.ClientParams, String) -> IO MariaDBConn
connectMariaDBTLSWithLimits limits requested connectInfo tlsParams =
    TLS.connectDetailExtendedWithLimits limits (capabilityBits requested) connectInfo tlsParams >>= mariadbConn

-- | Refuses a server whose greeting does not announce MariaDB's protocol
-- extensions, after closing the connection. The greeting's capabilities decide
-- this, not its version string, which @mariadbd --version@ can set to anything.
mariadbConn :: (Greeting, ExtendedCapabilities, MySQLConn) -> IO MariaDBConn
mariadbConn (greeting, negotiated, conn) =
    if offersExtendedCapabilities greeting
    then pure (MariaDBConn conn greeting (negotiatedCapabilities negotiated))
    else do
        close conn
        throwIO (NotMariaDB (greetingVersion greeting))

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
    let mysqlConn = mysqlConnection conn
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

-- | One parameter of one row given to 'executeBulk'.
data BulkParam
    = BulkValue !MySQLValue
      -- ^ 'MySQLNull' sends SQL NULL.
    | BulkDefault
      -- ^ The column's default, for INSERT and UPDATE.
    | BulkIgnore
      -- ^ The column's default for INSERT; UPDATE leaves the column as it is.
    deriving (Show, Eq)

-- | Rows 'executeBulk' refuses before sending anything, so the connection
-- stays usable.
data BulkExecuteError
    = BulkNoParameters
      -- ^ The rows have no parameters; a bulk command sends parameter rows.
    | BulkRowWidth !Int !Int !Int
      -- ^ Row number (counting from 0), the first row's width, this row's width.
    | BulkMixedTypes !Int ![MySQLValue]
      -- ^ Parameter number (counting from 0) and one value of each type found
      -- there. The command declares one type per parameter for every row.
    deriving (Show, Eq)

instance Exception BulkExecuteError where
    displayException err = "mysql-haskell: executeBulk: " ++ case err of
        BulkNoParameters -> "the rows have no parameters"
        BulkRowWidth row expected found ->
            "row " ++ show row ++ " has " ++ show found ++ " parameters, the first row has " ++ show expected
        BulkMixedTypes param examples ->
            "parameter " ++ show param ++ " mixes value types " ++ show examples
            ++ "; give every row the same MySQLValue constructor there"

-- | Execute a prepared statement once per row in a single round trip
-- (COM_STMT_BULK_EXECUTE). The 'OK' sums the affected rows of every
-- execution. A statement that returns rows is refused by the server with an
-- 'ERRException'.
--
-- Every row must have the same number of parameters, and each parameter the
-- same 'MySQLValue' constructor in every row where it is a 'BulkValue' other
-- than 'MySQLNull', or 'BulkExecuteError' is raised. Raises
-- 'CapabilityNotNegotiated' without 'MariaDBBulkOperations'; MySQL has no
-- bulk command.
--
-- The rows travel in one command, which the server's @max_allowed_packet@
-- limits; split larger inputs over several calls.
--
-- > import Data.List.NonEmpty (NonEmpty ((:|)))
-- >
-- > conn <- connectMariaDB [MariaDBBulkOperations] defaultConnectInfo
-- > let mysqlConn = mysqlConnection conn
-- > _ <- execute_ mysqlConn "CREATE TABLE people (name VARCHAR(20), age INT DEFAULT 18)"
-- > insert <- prepareStmt mysqlConn "INSERT INTO people (name, age) VALUES (?, ?)"
-- > ok <- executeBulk conn insert
-- >     (  [BulkValue (MySQLText "Ada"),   BulkValue (MySQLInt32 36)]
-- >     :| [ [BulkValue (MySQLText "Alan"),  BulkValue MySQLNull]  -- age NULL
-- >        , [BulkValue (MySQLText "Grace"), BulkDefault]          -- age 18
-- >        ])
-- > okAffectedRows ok  -- 3
executeBulk :: MariaDBConn -> StmtID -> NonEmpty [BulkParam] -> IO OK
executeBulk conn stid rows = do
    requireCapability MariaDBBulkOperations conn
    types <- either throwIO pure (bulkParamTypes rows)
    let mysqlConn = mysqlConnection conn
        is = Connection.mysqlRead mysqlConn
    Connection.guardUnconsumed mysqlConn
    Connection.writePayload (runPut (putBulkExecute stid types rows)) (Connection.mysqlWrite mysqlConn)
    p <- Connection.readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> decodeFromPacket p
        | otherwise -> throwIO (UnexpectedPacket p)

-- | The type each parameter is declared with: that of its values, or NULL's
-- when every row sends NULL, DEFAULT or IGNORE there.
bulkParamTypes :: NonEmpty [BulkParam] -> Either BulkExecuteError [L.ByteString]
bulkParamTypes rows@(firstRow :| _) =
    if null firstRow
    then Left BulkNoParameters
    else do
        mapM_ (checkRowWidth (length firstRow)) (zip [0 ..] (NonEmpty.toList rows))
        mapM paramType (zip [0 ..] (transpose (NonEmpty.toList rows)))

checkRowWidth :: Int -> (Int, [BulkParam]) -> Either BulkExecuteError ()
checkRowWidth expected (row, params) =
    if length params == expected then Right () else Left (BulkRowWidth row expected (length params))

paramType :: (Int, [BulkParam]) -> Either BulkExecuteError L.ByteString
paramType (param, column) =
    let values = concatMap typedValue column
        typed = nubOn (runPut . putParamMySQLType) values
    in case typed of
        []      -> Right (runPut (putParamMySQLType MySQLNull))
        [value] -> Right (runPut (putParamMySQLType value))
        (_ : _ : _) -> Left (BulkMixedTypes param typed)

typedValue :: BulkParam -> [MySQLValue]
typedValue (BulkValue MySQLNull) = []
typedValue (BulkValue value)     = [value]
typedValue BulkDefault           = []
typedValue BulkIgnore            = []

-- | One value per distinct key, in order of first appearance.
nubOn :: Eq key => (value -> key) -> [value] -> [value]
nubOn key values =
    let keys = nub (map key values)
    in concatMap (\k -> take 1 (filter ((== k) . key) values)) keys

-- Decision: always send the parameter types (SEND_TYPES_TO_SERVER, 128), as
-- mariadb-connector-c does. The server keeps no types from an earlier
-- COM_STMT_EXECUTE that a bulk command could rely on. Per-row results
-- (SEND_UNIT_RESULTS) need MARIADB_CLIENT_BULK_UNIT_RESULTS, which MariaDB
-- 11.4, the version CI runs, does not offer.
putBulkExecute :: StmtID -> [L.ByteString] -> NonEmpty [BulkParam] -> Put
putBulkExecute stid types rows = do
    putWord8 0xfa
    putWord32le stid
    putWord16le 128
    mapM_ putLazyByteString types
    forM_ rows (mapM_ putBulkParam)

putBulkParam :: BulkParam -> Put
putBulkParam (BulkValue MySQLNull) = putWord8 1
putBulkParam (BulkValue value)     = putWord8 0 >> putBinaryField value
putBulkParam BulkDefault           = putWord8 2
putBulkParam BulkIgnore            = putWord8 3

requireCapability :: MariaDBCapability -> MariaDBConn -> IO ()
requireCapability capability conn =
    unless (hasCapability capability conn) (throwIO (CapabilityNotNegotiated capability))

-- | A function needed a capability the connection did not negotiate, because
-- it was not asked for in 'connectMariaDB' or the server's MariaDB version
-- does not offer it.
newtype CapabilityNotNegotiated = CapabilityNotNegotiated MariaDBCapability
    deriving Show

instance Exception CapabilityNotNegotiated where
    displayException (CapabilityNotNegotiated capability) =
        "mysql-haskell: this function needs the MariaDB capability " ++ show capability
        ++ ", which the connection did not negotiate. Ask for it in connectMariaDB,"
        ++ " on a MariaDB version that offers it."

-- | A connect function of this module reached a server that does not speak
-- MariaDB's protocol extensions: MySQL, or MariaDB before 10.2. Carries the
-- server's version string.
newtype NotMariaDB = NotMariaDB ByteString
    deriving Show

instance Exception NotMariaDB where
    displayException (NotMariaDB version) =
        "mysql-haskell: Database.MariaDB needs a server speaking MariaDB's protocol"
        ++ " extensions, but server " ++ show version ++ " set CLIENT_MYSQL in its"
        ++ " greeting, as MySQL servers and MariaDB before 10.2 do. Connect with"
        ++ " Database.MySQL.Base for those servers, or upgrade to MariaDB 10.2 or later."
