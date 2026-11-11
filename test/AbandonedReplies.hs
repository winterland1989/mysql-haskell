{-# LANGUAGE ScopedTypeVariables #-}
-- | Replies left unread on the connection. The server answers in order, so
-- whatever part of a reply stays unread used to be taken for the reply to the
-- next command (issue #10).
module AbandonedReplies (tests) where

import           Control.Exception   (SomeException, fromException, throwIO, try)
import           Control.Monad       (void)
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           System.Timeout      (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: TestTree
tests = testGroup "replies left unread"
    [ testCase "a query interrupted by a timeout makes the connection refuse commands" $
        withTestConnection $ \c -> do
            interrupted <- timeout 20000 (query_ c "SELECT SLEEP(0.2)" >>= Stream.toList . snd)
            assertEqual "the timeout fired" Nothing interrupted
            next <- try (query_ c "SELECT 'next'" >>= Stream.toList . snd)
            case next of
                Left AbandonedReply -> pure ()
                Right rows -> assertFailure ("the next query returned " ++ show rows)
    , testCase "resetStmt with rows unread refuses instead of reading a row as its reply" $
        withTestConnection $ \c -> do
            stmt <- prepareStmt c "SELECT 'row' UNION ALL SELECT 'row'"
            (_, rows) <- queryStmt c stmt []
            reset <- try (resetStmt c stmt)
            case reset of
                Left UnconsumedResultSet -> pure ()
                Right () -> assertFailure "resetStmt read a row packet as its reply"
            Stream.skipToEof rows
            (_, after) <- query_ c "SELECT 'next'"
            Stream.toList after >>= assertEqual "the next query's rows" [[MySQLText "next"]]
    , testCase "withTransaction rethrows the procedure's exception" $
        withTestConnection $ \c -> do
            outcome <- try (withTransaction c (query_ c "SELECT 'unread'" >> throwIO (userError "procedure failed")))
            case outcome of
                Left (failure :: SomeException) -> assertEqual "exception"
                    (Just "user error (procedure failed)") (fmap show (fromException failure :: Maybe IOError))
                Right () -> assertFailure "no exception"
            next <- try (void (execute_ c "SELECT 1"))
            case next of
                Left AbandonedReply -> pure ()
                Right () -> assertFailure "the connection took a command inside the failed transaction"
    ]
