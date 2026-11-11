{-# LANGUAGE ScopedTypeVariables #-}
-- | Row events the binlog decoder used to drop: those after the first one of
-- a statement, which splits its rows over events of about 8 KiB, and those
-- of a statement that maps several tables before its first rows event.
module BinLogRowEvents (tests, compressedTransactionTests) where

import           Control.Concurrent  (forkIO)
import           Control.Exception   (SomeException, fromException, try)
import           Control.Monad       (forM_, void)
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.IORef          (IORef, modifyIORef', newIORef, readIORef)
import           Data.List           (intercalate)
import           Database.MySQL.Base
import           Database.MySQL.BinLog
import qualified System.IO.Streams   as Stream
import           System.Timeout      (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (testConnectInfo)

tests :: TestTree
tests = testGroup "binlog row events"
    [ testCase "every rows event of a statement that spans several" $ do
            c <- dumpConnection
            void (execute_ c "DROP TABLE IF EXISTS binlog_split_rows")
            void (execute_ c "CREATE TABLE binlog_split_rows (id INT, filler VARCHAR(1000))")
            rowEvents <- rowEventsFrom c 1010 $ \producer ->
                execute_ producer (Query (LC.pack ("INSERT INTO binlog_split_rows VALUES "
                    ++ intercalate ", " (map splitRow [1 .. splitRowCount]))))
            written <- countUntil splitRowCount rowEvents writtenRows
            assertEqual "rows decoded" (Just splitRowCount) written
    , testCase "every table of a statement that maps two before its rows" $ do
            c <- dumpConnection
            forM_ ["binlog_left", "binlog_right" :: LC.ByteString] $ \table -> do
                void (execute_ c (Query ("DROP TABLE IF EXISTS " <> table)))
                void (execute_ c (Query ("CREATE TABLE " <> table <> " (id INT, v INT)")))
                void (execute_ c (Query ("INSERT INTO " <> table <> " VALUES (1, 0)")))
            rowEvents <- rowEventsFrom c 1011 $ \producer ->
                execute_ producer "UPDATE binlog_left, binlog_right \
                                  \SET binlog_left.v = 1, binlog_right.v = 1 \
                                  \WHERE binlog_left.id = binlog_right.id"
            updated <- countUntil 2 rowEvents updatedRows
            assertEqual "update rows decoded across both tables" (Just 2) updated
    ]

-- | MySQL 8 only: a compressed transaction is one TRANSACTION_PAYLOAD_EVENT
-- holding its rows, which the decoder cannot read.
compressedTransactionTests :: TestTree
compressedTransactionTests = testCase "a compressed transaction stops the row decoder" $ do
        c <- dumpConnection
        void (execute_ c "DROP TABLE IF EXISTS binlog_compressed")
        void (execute_ c "CREATE TABLE binlog_compressed (id INT)")
        rowEvents <- rowEventsFrom c 1012 $ \producer -> do
            void (execute_ producer "SET SESSION binlog_transaction_compression = ON")
            execute_ producer "INSERT INTO binlog_compressed VALUES (1)"
        decoded <- timeout tenSeconds (try (Stream.read rowEvents))
        case decoded of
            Just (Left (failure :: SomeException)) -> case fromException failure of
                Just (UnsupportedEventType code _) -> assertEqual "event type" 40 code
                _ -> assertFailure ("raised " ++ show failure)
            Just (Right event) -> assertFailure ("decoded " ++ show event)
            Nothing -> assertFailure "no event within 10 s"

-- | A connection for 'dumpBinLog', left open: once dumping, the server stops
-- reading commands from it, so 'close' would wait for its next event (issue #35).
dumpConnection :: IO MySQLConn
dumpConnection = connect testConnectInfo

-- | The row events from the current end of the binlog on, while @produce@
-- runs on a second connection.
rowEventsFrom :: MySQLConn -> SlaveID -> (MySQLConn -> IO OK) -> IO (Stream.InputStream RowBinLogEvent)
rowEventsFrom c slaveId produce = do
    Just tracker <- getLastBinLogTracker c
    dump <- dumpBinLog c slaveId tracker False
    rowEvents <- decodeRowBinLogEvent dump
    void (forkIO (connect testConnectInfo >>= \producer -> produce producer >> close producer))
    pure rowEvents

-- | Reads events until the counted rows reach @target@; Nothing when they
-- do not within 10 s, as when events are dropped.
countUntil :: Int -> Stream.InputStream RowBinLogEvent -> (RowBinLogEvent -> Int) -> IO (Maybe Int)
countUntil target rowEvents rowsOf = do
    counted <- newIORef 0
    reached <- timeout tenSeconds (readUntil target rowEvents rowsOf counted)
    case reached of
        Just () -> Just <$> readIORef counted
        Nothing -> pure Nothing

readUntil :: Int -> Stream.InputStream RowBinLogEvent -> (RowBinLogEvent -> Int) -> IORef Int -> IO ()
readUntil target rowEvents rowsOf counted = do
    event <- Stream.read rowEvents
    forM_ event (\e -> modifyIORef' counted (+ rowsOf e))
    total <- readIORef counted
    if total >= target then pure () else readUntil target rowEvents rowsOf counted

writtenRows :: RowBinLogEvent -> Int
writtenRows event = case event of
    RowWriteEvent _ _ table rows -> if tmTableName table == "binlog_split_rows" then length (writeRowData rows) else 0
    RowUpdateEvent {} -> 0
    RowDeleteEvent {} -> 0
    RowQueryEvent {} -> 0

updatedRows :: RowBinLogEvent -> Int
updatedRows event = case event of
    RowUpdateEvent _ _ _ rows -> length (updateRowData rows)
    RowWriteEvent {} -> 0
    RowDeleteEvent {} -> 0
    RowQueryEvent {} -> 0

-- | Enough 1000-byte rows for several rows events of binlog_row_event_max_size
-- (8 KiB by default).
splitRowCount :: Int
splitRowCount = 200

splitRow :: Int -> String
splitRow n = "(" ++ show n ++ ", REPEAT('x', 1000))"

tenSeconds :: Int
tenSeconds = 10000000
