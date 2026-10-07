-- | Statements whose reply holds several results: multi-statement strings and
-- CALLs. Whatever a function cannot hand back must still be read off the
-- connection, or the next query receives the previous one's leftovers.
module MultipleResults (tests) where

import           Control.Exception (try)
import           Data.Int          (Int64)
import           Database.MySQL.Base
import qualified System.IO.Streams as Stream
import           System.Timeout    (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "statements with several results"
    [ testCase "query_ with two INSERTs" $ withTable $ \c -> do
        (_, rows) <- query_ c
            "INSERT INTO several_results VALUES (1); INSERT INTO several_results VALUES (2)"
        Stream.skipToEof rows
        assertRowCount c 2
    , testCase "execute_ with two INSERTs" $ withTable $ \c -> do
        _ <- execute_ c
            "INSERT INTO several_results VALUES (1); INSERT INTO several_results VALUES (2)"
        assertRowCount c 2
    , testCase "execute_ with a SELECT" $ withTable $ \c ->
        assertExtraResultSets c (execute_ c "SELECT COUNT(*) FROM several_results")
    , testCase "executeStmt with a SELECT" $ withTable $ \c -> do
        stmt <- prepareStmt c "SELECT COUNT(*) FROM several_results"
        assertExtraResultSets c (executeStmt c stmt [])
    , testCase "execute_ with a CALL returning one result set" $ withTable $ \c -> do
        _ <- execute_ c "DROP PROCEDURE IF EXISTS one_result_set"
        _ <- execute_ c "CREATE PROCEDURE one_result_set() SELECT COUNT(*) FROM several_results"
        assertExtraResultSets c (execute_ c "CALL one_result_set()")
    , testCase "query_ with a CALL returning one result set" $ withTable $ \c -> do
        _ <- execute_ c "DROP PROCEDURE IF EXISTS one_result_set"
        _ <- execute_ c "CREATE PROCEDURE one_result_set() SELECT COUNT(*) FROM several_results"
        (_, rows) <- query_ c "CALL one_result_set()"
        counts <- Stream.toList rows
        assertEqual "rows of the procedure's SELECT" [[MySQLInt64 0]] counts
        assertRowCount c 0
    , testCase "query_ with an INSERT before a SELECT" $ withTable $ \c -> do
        (_, rows) <- query_ c
            "INSERT INTO several_results VALUES (1); SELECT COUNT(*) FROM several_results"
        counts <- Stream.toList rows
        assertEqual "rows of the SELECT" [[MySQLInt64 1]] counts
        assertRowCount c 1
    , testCase "query_ with two SELECTs" $ withTable $ \c -> do
        (_, rows) <- query_ c
            "SELECT COUNT(*) FROM several_results; SELECT COUNT(*) FROM several_results"
        outcome <- try (Stream.toList rows)
        case outcome of
            Left ExtraResultSets -> pure ()
            Right _ -> assertFailure "the second SELECT's result set went unreported"
        again <- Stream.read rows
        assertEqual "reading the finished rows again" Nothing again
        assertRowCount c 0
    ]

-- | Generous for a few statements on an empty temporary table, and short enough
-- that a hang fails the suite instead of stalling CI.
replyTimeLimitMicroseconds :: Int
replyTimeLimitMicroseconds = 10000000

-- | Runs a case on a fresh connection with an empty temporary table.
withTable :: (MySQLConn -> Assertion) -> Assertion
withTable body = do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }
    _ <- execute_ c "CREATE TEMPORARY TABLE several_results (__id INT)"
    finished <- timeout replyTimeLimitMicroseconds (body c)
    case finished of
        Nothing -> assertFailure "blocked for 10 s waiting for a reply"
        Just () -> close c

-- | The statement must raise 'ExtraResultSets' and leave the connection in step.
assertExtraResultSets :: MySQLConn -> IO OK -> Assertion
assertExtraResultSets c runStatement = do
    outcome <- try runStatement
    case outcome of
        Left ExtraResultSets -> pure ()
        Right _ -> assertFailure "the result set went unreported"
    assertRowCount c 0

-- | The connection must be back in step: a fresh query gets its own rows.
assertRowCount :: MySQLConn -> Int64 -> Assertion
assertRowCount c expected = do
    (_, rows) <- query_ c "SELECT COUNT(*) FROM several_results"
    counts <- Stream.toList rows
    assertEqual "rows of the next query" [[MySQLInt64 expected]] counts
