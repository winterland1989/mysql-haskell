module MariaDBExtendedMetadata (tests) where

import qualified Data.ByteString        as B
import           Database.MySQL.Base
import           Database.MariaDB
import qualified Database.MySQL.TLS     as TLS
import qualified System.IO.Streams      as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}

createTable :: MySQLConn -> IO ()
createTable conn = do
    _ <- execute_ conn "DROP TABLE IF EXISTS mariadb_extended"
    _ <- execute_ conn "CREATE TABLE mariadb_extended (n INT, j JSON, u UUID, a INET6, p POINT, s VARCHAR(8))"
    _ <- execute_ conn "INSERT INTO mariadb_extended VALUES (1, '{\"k\":1}', '123e4567-e89b-12d3-a456-426614174000', '::1', POINT(1,2), 'plain')"
    pure ()

columnNamed :: B.ByteString -> [ColumnMetadata] -> IO ColumnMetadata
columnNamed name columns = case filter ((== name) . columnName . columnDefinition) columns of
    [column] -> pure column
    found    -> assertFailure ("expected one column " ++ show name ++ ", got " ++ show found)

selectAll :: Query
selectAll = "SELECT n, j, u, a, s FROM mariadb_extended WHERE n = ?"

expectedRow :: [MySQLValue]
expectedRow =
    [ MySQLInt32 1, MySQLText "{\"k\":1}", MySQLText "123e4567-e89b-12d3-a456-426614174000"
    , MySQLText "::1", MySQLText "plain" ]

tests :: Maybe FilePath -> Maybe FilePath -> TestTree
tests socketPath caPath = testGroup "MariaDB extended column metadata" $
    [ testCase "prepared statement columns name their SQL types" $ do
        conn <- connectMariaDB [MariaDBExtendedMetadata] testConnectInfo
        createTable (mysqlConnection conn)
        (_, params, columns) <- prepareStmtMetadata conn "SELECT n, j, u, a, p, s FROM mariadb_extended WHERE n = ?"
        length params @?= 1
        map (columnName . columnDefinition) columns @?= ["n", "j", "u", "a", "p", "s"]
        n <- columnNamed "n" columns
        columnExtended n @?= []
        j <- columnNamed "j" columns
        columnExtended j @?= [ExtendedFormat "json"]
        u <- columnNamed "u" columns
        columnExtended u @?= [ExtendedTypeName "uuid"]
        a <- columnNamed "a" columns
        columnExtended a @?= [ExtendedTypeName "inet6"]
        p <- columnNamed "p" columns
        columnExtended p @?= [ExtendedTypeName "point"]
        s <- columnNamed "s" columns
        columnExtended s @?= []
        close (mysqlConnection conn)
    , testCase "text and binary queries read rows despite the metadata" $ do
        conn <- connectMariaDB [MariaDBExtendedMetadata] testConnectInfo
        let mysqlConn = mysqlConnection conn
        createTable mysqlConn
        (textColumns, textRows) <- query mysqlConn selectAll [MySQLInt32 1]
        map columnName textColumns @?= ["n", "j", "u", "a", "s"]
        textValues <- Stream.toList textRows
        textValues @?= [expectedRow]
        statement <- prepareStmt mysqlConn selectAll
        (binaryColumns, binaryRows) <- queryStmt mysqlConn statement [MySQLInt32 1]
        map columnType binaryColumns @?= map columnType textColumns
        binaryValues <- Stream.toList binaryRows
        binaryValues @?= [expectedRow]
        close mysqlConn
    ]
    ++ maybe [] (pure . unixSocketTest) socketPath
    ++ maybe [] (pure . tlsTest) caPath

-- | A temporary table keeps tests on separate connections apart.
jsonColumnMetadata :: MariaDBConn -> Assertion
jsonColumnMetadata conn = do
    _ <- execute_ (mysqlConnection conn) "CREATE TEMPORARY TABLE json_only (j JSON)"
    (_, _, columns) <- prepareStmtMetadata conn "SELECT j FROM json_only"
    map columnExtended columns @?= [[ExtendedFormat "json"]]

unixSocketTest :: FilePath -> TestTree
unixSocketTest path = testCase "negotiated over a Unix socket" $ do
    conn <- connectMariaDBUnixSocket [MariaDBExtendedMetadata] path testConnectInfo
    mariadbCapabilities conn @?= [MariaDBExtendedMetadata]
    jsonColumnMetadata conn
    close (mysqlConnection conn)

tlsTest :: FilePath -> TestTree
tlsTest caPath = testCase "negotiated over TLS, where the SSL request carries it" $ do
    cparams <- TLS.makeClientParams (TLS.CustomCAStore caPath)
    conn <- connectMariaDBTLS [MariaDBExtendedMetadata] testConnectInfo (cparams, "Winter")
    mariadbCapabilities conn @?= [MariaDBExtendedMetadata]
    (_, cipher) <- query_ (mysqlConnection conn) "SHOW STATUS LIKE 'Ssl_cipher'"
    cipherRows <- Stream.toList cipher
    assertBool "the connection is encrypted" (cipherRows /= [[MySQLText "Ssl_cipher", MySQLText ""]])
    jsonColumnMetadata conn
    close (mysqlConnection conn)
