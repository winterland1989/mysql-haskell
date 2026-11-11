module MariaDBNegotiation (tests) where

import           Control.Exception      (try)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import           Database.MySQL.Base
import           Database.MySQL.MariaDB
import qualified System.IO.Streams      as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}

-- | Runs on both servers: asking a MySQL server for a MariaDB capability is
-- not an error, the connection is made without it.
tests :: ServerFlavour -> TestTree
tests flavour = testGroup "MariaDB capability negotiation"
    [ testCase "extended metadata is negotiated exactly on MariaDB" $ do
        conn <- connectMariaDB [MariaDBExtendedMetadata] testConnectInfo
        serverFlavour (mariadbGreeting conn) @?= flavour
        mariadbCapabilities conn @?= case flavour of
            MariaDBServer -> [MariaDBExtendedMetadata]
            MySQLServer   -> []
        (_, rows) <- query_ (mariadbConnection conn) "SELECT 'probe'"
        values <- Stream.toList rows
        values @?= [[MySQLText "probe"]]
        close (mariadbConnection conn)
    , testCase "nothing asked, nothing negotiated" $ do
        conn <- connectMariaDB [] testConnectInfo
        mariadbCapabilities conn @?= []
        result <- try (prepareStmtMetadata conn "SELECT 1")
        case result of
            Left (CapabilityNotNegotiated MariaDBExtendedMetadata) -> pure ()
            Right _ -> assertFailure "prepareStmtMetadata ran without extended metadata"
        close (mariadbConnection conn)
    , testCase "bulk execution needs the capability, which MySQL lacks" $ do
        conn <- connectMariaDB [MariaDBBulkOperations] testConnectInfo
        let mysqlConn = mariadbConnection conn
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_negotiated (n INT)"
        statement <- prepareStmt mysqlConn "INSERT INTO bulk_negotiated VALUES (?)"
        result <- try (executeBulk conn statement ([BulkValue (MySQLInt32 1)] :| [[BulkValue (MySQLInt32 2)]]))
        case (flavour, result) of
            (MariaDBServer, Right ok) -> okAffectedRows ok @?= 2
            (MySQLServer, Left (CapabilityNotNegotiated MariaDBBulkOperations)) -> pure ()
            (_, Right ok) -> assertFailure ("bulk ran on MySQL: " ++ show ok)
            (_, Left err) -> assertFailure ("unexpected refusal: " ++ show err)
        close mysqlConn
    ]
