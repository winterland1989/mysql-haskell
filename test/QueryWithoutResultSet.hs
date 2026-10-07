-- | Statements without a result set (INSERT, UPDATE, DELETE) sent through the
-- functions that expect one. The server answers with a lone OK packet, which
-- used to leave these functions blocked forever waiting for column
-- definitions that never come (issue #47).
module QueryWithoutResultSet (tests) where

import           Control.Exception (try)
import           Database.MySQL.Base
import qualified System.IO.Streams as Stream
import           System.Timeout    (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "query functions given an INSERT"
    [ testCase "query_" $ assertNoResultSet $ \c ->
        () <$ query_ c "INSERT INTO without_result_set VALUES (1)"
    , testCase "queryVector_" $ assertNoResultSet $ \c ->
        () <$ queryVector_ c "INSERT INTO without_result_set VALUES (1)"
    , testCase "queryStmt" $ assertNoResultSet $ \c -> do
        stmt <- prepareStmt c "INSERT INTO without_result_set VALUES (?)"
        () <$ queryStmt c stmt [MySQLInt32 1]
    , testCase "queryStmtVector" $ assertNoResultSet $ \c -> do
        stmt <- prepareStmt c "INSERT INTO without_result_set VALUES (?)"
        () <$ queryStmtVector c stmt [MySQLInt32 1]
    ]

-- | Generous for one INSERT into an empty temporary table, and short enough
-- that a hang fails the suite instead of stalling CI.
replyTimeLimitMicroseconds :: Int
replyTimeLimitMicroseconds = 10000000

-- | Runs the insert on a fresh connection. It must throw 'NoResultSet' carrying
-- the server's OK, and the connection must stay usable afterwards.
assertNoResultSet :: (MySQLConn -> IO ()) -> Assertion
assertNoResultSet runInsert = do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }
    _ <- execute_ c "CREATE TEMPORARY TABLE without_result_set (__id INT)"
    outcome <- timeout replyTimeLimitMicroseconds (try (runInsert c))
    case outcome of
        Nothing -> assertFailure
            "blocked for 10 s waiting for a result set the server never sends"
        Just (Right ()) -> assertFailure
            "returned a result set for an INSERT instead of throwing NoResultSet"
        Just (Left (NoResultSet ok)) -> do
            assertEqual "affected rows reported by the OK" 1 (okAffectedRows ok)
            (_, rows) <- query_ c "SELECT COUNT(*) FROM without_result_set"
            counts <- Stream.toList rows
            assertEqual "the row was inserted once" [[MySQLInt64 1]] counts
    close c
