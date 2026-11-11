-- | 'resetConnection' gives a connection the session of a new one, so a pool
-- can hand it to its next user.
module ResetConnection (tests) where

import           Control.Exception   (try)
import           Control.Monad       (void)
import           Data.Word           (Word16)
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: TestTree
tests = testGroup "resetConnection"
    [ testCase "a user variable is NULL afterwards" $
        withTestConnection $ \c -> do
            void (execute_ c "SET @reset_probe = 42")
            resetConnection c
            selectAll c "SELECT @reset_probe" >>= assertEqual "the variable" [[MySQLNull]]
    , testCase "a temporary table is gone afterwards" $
        withTestConnection $ \c -> do
            void (execute_ c "CREATE TEMPORARY TABLE reset_connection_temporary (x INT)")
            resetConnection c
            -- ER_NO_SUCH_TABLE
            assertRefused 1146 (selectAll c "SELECT x FROM reset_connection_temporary")
    , testCase "an uncommitted insert is rolled back" $
        withTestConnection $ \c -> do
            void (execute_ c "DROP TABLE IF EXISTS reset_connection_rollback")
            void (execute_ c "CREATE TABLE reset_connection_rollback (x INT) ENGINE=InnoDB")
            void (execute_ c "START TRANSACTION")
            void (execute_ c "INSERT INTO reset_connection_rollback VALUES (1)")
            resetConnection c
            selectAll c "SELECT COUNT(*) FROM reset_connection_rollback"
                >>= assertEqual "rows in the table" [[MySQLInt64 0]]
            void (execute_ c "DROP TABLE reset_connection_rollback")
    , testCase "a statement prepared before it is refused by the server" $
        withTestConnection $ \c -> do
            stmt <- prepareStmt c "SELECT 'prepared'"
            resetConnection c
            -- ER_UNKNOWN_STMT_HANDLER
            assertRefused 1243 (queryStmt c stmt [] >>= Stream.toList . snd)
            selectAll c "SELECT 'next'" >>= assertEqual "the next query's rows" [[MySQLText "next"]]
    , testCase "with rows unread it refuses instead of reading a row as its reply" $
        withTestConnection $ \c -> do
            void (execute_ c "SET @reset_probe = 42")
            (_, unread) <- query_ c "SELECT 'row' UNION ALL SELECT 'row'"
            refused <- try (resetConnection c)
            case refused of
                Left UnconsumedResultSet -> pure ()
                Right () -> assertFailure "resetConnection read a row packet as its reply"
            Stream.skipToEof unread
            selectAll c "SELECT @reset_probe" >>= assertEqual "the session kept its state" [[MySQLInt64 42]]
    ]

selectAll :: MySQLConn -> Query -> IO [[MySQLValue]]
selectAll c q = query_ c q >>= Stream.toList . snd

assertRefused :: Show a => Word16 -> IO a -> Assertion
assertRefused code action = do
    outcome <- try action
    case outcome of
        Left (ERRException err) -> assertEqual ("error " ++ show (errMsg err)) code (errCode err)
        Right answer -> assertFailure ("the server answered " ++ show answer)
