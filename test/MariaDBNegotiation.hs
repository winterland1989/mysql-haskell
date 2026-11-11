module MariaDBNegotiation (tests) where

import           Control.Concurrent     (threadDelay)
import           Control.Exception      (try)
import qualified Data.ByteString        as B
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Database.MariaDB
import           Database.MySQL.Base
import qualified System.IO.Streams      as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}

-- | Runs on both servers, given the greeting of a plain connection: a MariaDB
-- server negotiates what it offers, and a MySQL server is refused.
tests :: Greeting -> TestTree
tests greeting = testGroup "MariaDB capability negotiation" $
    if offersExtendedCapabilities greeting then mariadbTests else mysqlTests

mariadbTests :: [TestTree]
mariadbTests =
    [ testCase "extended metadata is negotiated, and the MySQL functions work on it" $ do
        conn <- connectMariaDB [MariaDBExtendedMetadata] testConnectInfo
        mariadbCapabilities conn @?= [MariaDBExtendedMetadata]
        (_, rows) <- query_ (mysqlConnection conn) "SELECT 'probe'"
        values <- Stream.toList rows
        values @?= [[MySQLText "probe"]]
        close (mysqlConnection conn)
    , testCase "nothing asked, nothing negotiated" $ do
        conn <- connectMariaDB [] testConnectInfo
        mariadbCapabilities conn @?= []
        result <- try (prepareStmtMetadata conn "SELECT 1")
        case result of
            Left (CapabilityNotNegotiated MariaDBExtendedMetadata) -> pure ()
            Left (CapabilityNotNegotiated MariaDBBulkOperations) ->
                assertFailure "asked for the bulk capability instead of extended metadata"
            Right _ -> assertFailure "prepareStmtMetadata ran without extended metadata"
        close (mysqlConnection conn)
    , testCase "bulk execution with the capability" $ do
        conn <- connectMariaDB [MariaDBBulkOperations] testConnectInfo
        let mysqlConn = mysqlConnection conn
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_negotiated (n INT)"
        statement <- prepareStmt mysqlConn "INSERT INTO bulk_negotiated VALUES (?)"
        ok <- executeBulk conn statement ([BulkValue (MySQLInt32 1)] :| [[BulkValue (MySQLInt32 2)]])
        okAffectedRows ok @?= 2
        close mysqlConn
    ]

mysqlTests :: [TestTree]
mysqlTests =
    [ testCase "connectMariaDB refuses a MySQL server and closes the connection" $ do
        admin <- connect testConnectInfo
        _ <- execute_ admin "CREATE USER IF NOT EXISTS 'mariadbRefusal'@'localhost' IDENTIFIED WITH mysql_native_password BY 'refusal'"
        observer <- connect refusalConnectInfo
        before <- refusalConnections observer
        result <- try (connectMariaDB [MariaDBExtendedMetadata] refusalConnectInfo)
        case result of
            Left (NotMariaDB version) ->
                assertBool ("refused with the server's version, got " ++ show version)
                    ("8." `B.isPrefixOf` version)
            Right conn -> do
                close (mysqlConnection conn)
                assertFailure "connected to a MySQL server"
        after <- settledRefusalConnections observer before (20 :: Int)
        after @?= before
        close observer
        _ <- execute_ admin "DROP USER 'mariadbRefusal'@'localhost'"
        close admin
    ]

-- | A user only this test connects as, so concurrent tests' connections do
-- not show up in its count.
refusalConnectInfo :: ConnectInfo
refusalConnectInfo = defaultConnectInfo {ciUser = "mariadbRefusal", ciPassword = "refusal", ciDatabase = ""}

-- | How many connections the server has open for 'refusalConnectInfo''s user.
-- A user sees its own threads in PROCESSLIST without the PROCESS privilege.
refusalConnections :: MySQLConn -> IO [[MySQLValue]]
refusalConnections conn = do
    (_, rows) <- query_ conn "SELECT COUNT(*) FROM information_schema.PROCESSLIST WHERE USER = 'mariadbRefusal'"
    Stream.toList rows

-- | The server ends a connection's thread shortly after COM_QUIT, so the count
-- may take a moment to drop back; poll for up to a second.
settledRefusalConnections :: MySQLConn -> [[MySQLValue]] -> Int -> IO [[MySQLValue]]
settledRefusalConnections conn expected attempts = do
    current <- refusalConnections conn
    if current == expected || attempts <= 0
        then pure current
        else threadDelay 50000 >> settledRefusalConnections conn expected (attempts - 1)
