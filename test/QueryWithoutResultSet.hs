-- | Statements without a result set (INSERT, UPDATE, DELETE) sent through the
-- functions that expect one. The server answers with a lone OK packet, which
-- used to leave these functions blocked forever waiting for column
-- definitions that never come (issue #47).
module QueryWithoutResultSet (tests) where

import           Database.MySQL.Base
import qualified Data.Vector       as V
import qualified System.IO.Streams as Stream
import           System.Timeout    (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "query functions given an INSERT"
    [ testCase "query_" $ assertEmptyResultSet $ \c -> do
        (columns, rows) <- query_ c "INSERT INTO without_result_set VALUES (1)"
        rowList <- Stream.toList rows
        pure (length columns, length rowList)
    , testCase "queryVector_" $ assertEmptyResultSet $ \c -> do
        (columns, rows) <- queryVector_ c "INSERT INTO without_result_set VALUES (1)"
        rowList <- Stream.toList rows
        pure (V.length columns, length rowList)
    , testCase "queryStmt" $ assertEmptyResultSet $ \c -> do
        stmt <- prepareStmt c "INSERT INTO without_result_set VALUES (?)"
        (columns, rows) <- queryStmt c stmt [MySQLInt32 1]
        rowList <- Stream.toList rows
        pure (length columns, length rowList)
    , testCase "queryStmtVector" $ assertEmptyResultSet $ \c -> do
        stmt <- prepareStmt c "INSERT INTO without_result_set VALUES (?)"
        (columns, rows) <- queryStmtVector c stmt [MySQLInt32 1]
        rowList <- Stream.toList rows
        pure (V.length columns, length rowList)
    ]

-- | Generous for one INSERT into an empty temporary table, and short enough
-- that a hang fails the suite instead of stalling CI.
replyTimeLimitMicroseconds :: Int
replyTimeLimitMicroseconds = 10000000

-- | Runs the insert on a fresh connection; it reports the column and row counts
-- of the result set it got back. Both must be zero, the row must be inserted
-- exactly once, and the connection must stay usable afterwards.
assertEmptyResultSet :: (MySQLConn -> IO (Int, Int)) -> Assertion
assertEmptyResultSet runInsert = do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }
    _ <- execute_ c "CREATE TEMPORARY TABLE without_result_set (__id INT)"
    outcome <- timeout replyTimeLimitMicroseconds (runInsert c)
    case outcome of
        Nothing -> assertFailure
            "blocked for 10 s waiting for a result set the server never sends"
        Just columnsAndRows -> do
            assertEqual "columns and rows returned" (0, 0) columnsAndRows
            (_, rows) <- query_ c "SELECT COUNT(*) FROM without_result_set"
            counts <- Stream.toList rows
            assertEqual "the row was inserted once" [[MySQLInt64 1]] counts
    close c
