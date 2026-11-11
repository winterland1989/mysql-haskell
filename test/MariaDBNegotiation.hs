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

-- | Runs on both servers: a MariaDB server negotiates what it offers, and a
-- MySQL server is refused.
tests :: ServerFlavour -> TestTree
tests flavour = testGroup "MariaDB capability negotiation" $ case flavour of
    MariaDBServer -> mariadbTests
    MySQLServer   -> mysqlTests

mariadbTests :: [TestTree]
mariadbTests =
    [ testCase "extended metadata is negotiated, and the MySQL functions work on it" $ do
        conn <- connectMariaDB [MariaDBExtendedMetadata] testConnectInfo
        serverFlavour (mariadbGreeting conn) @?= MariaDBServer
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
        observer <- connect testConnectInfo
        before <- threadsConnected observer
        result <- try (connectMariaDB [MariaDBExtendedMetadata] testConnectInfo)
        case result of
            Left (NotMariaDB version) ->
                assertBool ("refused with the server's version, got " ++ show version)
                    ("8." `B.isPrefixOf` version)
            Right conn -> do
                close (mysqlConnection conn)
                assertFailure "connected to a MySQL server"
        after <- settledThreadsConnected observer before (20 :: Int)
        after @?= before
        close observer
    ]

-- | The server's count of open client connections, this one included.
threadsConnected :: MySQLConn -> IO [[MySQLValue]]
threadsConnected conn = do
    (_, rows) <- query_ conn "SHOW GLOBAL STATUS LIKE 'Threads_connected'"
    Stream.toList rows

-- | The server ends a connection's thread shortly after COM_QUIT, so the count
-- may take a moment to drop back; poll for up to a second.
settledThreadsConnected :: MySQLConn -> [[MySQLValue]] -> Int -> IO [[MySQLValue]]
settledThreadsConnected conn expected attempts = do
    current <- threadsConnected conn
    if current == expected || attempts <= 0
        then pure current
        else threadDelay 50000 >> settledThreadsConnected conn expected (attempts - 1)
