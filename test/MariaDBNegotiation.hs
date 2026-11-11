module MariaDBNegotiation (tests) where

import           Control.Exception      (try)
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
    ]
