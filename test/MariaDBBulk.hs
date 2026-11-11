module MariaDBBulk (tests) where

import           Control.Exception      (try)
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Text              as T
import           Database.MySQL.Base
import           Database.MariaDB
import qualified System.IO.Streams      as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}

withBulkConnection :: (MariaDBConn -> MySQLConn -> IO a) -> IO a
withBulkConnection action = do
    conn <- connectMariaDB [MariaDBBulkOperations] testConnectInfo
    mariadbCapabilities conn @?= [MariaDBBulkOperations]
    result <- action conn (mysqlConnection conn)
    close (mysqlConnection conn)
    pure result

selectRows :: MySQLConn -> Query -> IO [[MySQLValue]]
selectRows conn sql = query_ conn sql >>= Stream.toList . snd

thousandRows :: NonEmpty [BulkParam]
thousandRows = NonEmpty.fromList
    (map (\n -> [BulkValue (MySQLInt32 n), BulkValue (MySQLText (T.pack ("row " ++ show n)))]) [1 .. 1000])

tests :: TestTree
tests = testGroup "MariaDB bulk execution"
    [ testCase "inserts every row in one command" $ withBulkConnection $ \conn mysqlConn -> do
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_rows (n INT, s VARCHAR(16))"
        statement <- prepareStmt mysqlConn "INSERT INTO bulk_rows VALUES (?, ?)"
        ok <- executeBulk conn statement thousandRows
        okAffectedRows ok @?= 1000
        counts <- selectRows mysqlConn "SELECT COUNT(*), SUM(n), COUNT(DISTINCT s) FROM bulk_rows"
        counts @?= [[MySQLInt64 1000, MySQLDecimal 500500, MySQLInt64 1000]]
        sample <- selectRows mysqlConn "SELECT n, s FROM bulk_rows WHERE n IN (1, 1000) ORDER BY n"
        sample @?= [[MySQLInt32 1, MySQLText "row 1"], [MySQLInt32 1000, MySQLText "row 1000"]]
    , testCase "NULL, DEFAULT and IGNORE indicators" $ withBulkConnection $ \conn mysqlConn -> do
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_indicators \
            \(id INT PRIMARY KEY, a INT DEFAULT 7, b VARCHAR(8) DEFAULT 'dflt', c INT NULL)"
        insert <- prepareStmt mysqlConn "INSERT INTO bulk_indicators VALUES (?, ?, ?, ?)"
        _ <- executeBulk conn insert $
               [BulkValue (MySQLInt32 1), BulkValue (MySQLInt32 1), BulkValue (MySQLText "x"), BulkValue MySQLNull]
            :| [[BulkValue (MySQLInt32 2), BulkDefault, BulkIgnore, BulkValue (MySQLInt32 3)]]
        inserted <- selectRows mysqlConn "SELECT id, a, b, c FROM bulk_indicators ORDER BY id"
        inserted @?=
            [ [MySQLInt32 1, MySQLInt32 1, MySQLText "x", MySQLNull]
            , [MySQLInt32 2, MySQLInt32 7, MySQLText "dflt", MySQLInt32 3] ]
        update <- prepareStmt mysqlConn "UPDATE bulk_indicators SET a = ?, b = ? WHERE id = ?"
        ok <- executeBulk conn update $
               [BulkIgnore, BulkValue (MySQLText "y"), BulkValue (MySQLInt32 1)]
            :| [[BulkValue (MySQLInt32 9), BulkIgnore, BulkValue (MySQLInt32 2)]]
        okAffectedRows ok @?= 2
        updated <- selectRows mysqlConn "SELECT id, a, b FROM bulk_indicators ORDER BY id"
        updated @?=
            [ [MySQLInt32 1, MySQLInt32 1, MySQLText "y"]
            , [MySQLInt32 2, MySQLInt32 9, MySQLText "dflt"] ]
    , testCase "rows the command cannot carry are refused before sending" $ withBulkConnection $ \conn mysqlConn -> do
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_refused (n INT)"
        statement <- prepareStmt mysqlConn "INSERT INTO bulk_refused VALUES (?)"
        mixed <- try (executeBulk conn statement ([BulkValue (MySQLInt32 1)] :| [[BulkValue (MySQLText "2")]]))
        mixed @?= Left (BulkMixedTypes 0 [MySQLInt32 1, MySQLText "2"])
        ragged <- try (executeBulk conn statement ([BulkValue (MySQLInt32 1)] :| [[]]))
        ragged @?= Left (BulkRowWidth 1 1 0)
        empty <- try (executeBulk conn statement ([] :| []))
        empty @?= Left BulkNoParameters
        stored <- selectRows mysqlConn "SELECT COUNT(*) FROM bulk_refused"
        stored @?= [[MySQLInt64 0]]
    , testCase "a server error leaves the connection usable" $ withBulkConnection $ \conn mysqlConn -> do
        _ <- execute_ mysqlConn "CREATE TEMPORARY TABLE bulk_unique (n INT PRIMARY KEY)"
        statement <- prepareStmt mysqlConn "INSERT INTO bulk_unique VALUES (?)"
        duplicate <- try (executeBulk conn statement ([BulkValue (MySQLInt32 1)] :| [[BulkValue (MySQLInt32 1)]]))
        case duplicate of
            Left (ERRException err) -> errCode err @?= 1062
            Right ok -> assertFailure ("duplicate key accepted: " ++ show ok)
        rows <- selectRows mysqlConn "SELECT 'still usable'"
        rows @?= [[MySQLText "still usable"]]
    ]
