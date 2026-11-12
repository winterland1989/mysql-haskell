-- | Binlog files written under different binlog_checksum settings, read by
-- one dump. The decoder used to strip and check four checksum bytes by the
-- server's setting when the dump started, so a file written under the other
-- setting failed its checks, where a replica reads it fine.
module BinLogChecksumSwitch (tests) where

import           Control.Exception    (finally)
import           Control.Monad        (void)
import           Data.Int             (Int32)
import           Data.IORef           (IORef, modifyIORef', newIORef, readIORef)
import           Data.Text            (Text)
import           Database.MySQL.Base
import           Database.MySQL.BinLog
import qualified System.IO.Streams    as Stream
import           System.Timeout       (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser             (testConnectInfo)

tests :: TestTree
tests = testGroup "binlog files of both binlog_checksum settings"
    [ testCase "a dump from a file without checksums into one with them" $
        withChecksumRestored $ \c -> do
            createTable c
            setChecksum c "NONE"
            Just unchecked <- getLastBinLogTracker c
            insertIds c [1, 2, 3]
            setChecksum c "CRC32"
            insertIds c [4, 5]
            ids <- dumpedIds 1301 (BinLogTracker (btFileName unchecked) 4) 5
            assertEqual "ids of both files, in order" (Just [1, 2, 3, 4, 5]) ids
    , testCase "a dump from a file with checksums into one without" $
        withChecksumRestored $ \c -> do
            setChecksum c "CRC32"
            createTable c
            Just checked <- getLastBinLogTracker c
            insertIds c [1, 2]
            setChecksum c "NONE"
            insertIds c [3, 4]
            ids <- dumpedIds 1302 checked 4
            assertEqual "ids of both files, in order" (Just [1, 2, 3, 4]) ids
    ]

-- | Runs the test on a connection that may set binlog_checksum, and puts the
-- server's setting back afterwards, as the other binlog tests expect it.
withChecksumRestored :: (MySQLConn -> IO ()) -> IO ()
withChecksumRestored test = do
    c <- connect testConnectInfo
    original <- currentChecksum c
    test c `finally` (setChecksum c original >> close c)

currentChecksum :: MySQLConn -> IO Text
currentChecksum c = do
    (_, rows) <- query_ c "SELECT @@GLOBAL.binlog_checksum"
    values <- Stream.toList rows
    case values of
        [[MySQLText setting]] -> pure setting
        _ -> assertFailure ("binlog_checksum reads as " ++ show values)

-- | Both servers start a new binlog file on the change, under the new setting.
setChecksum :: MySQLConn -> Text -> IO ()
setChecksum c setting =
    void (execute c "SET GLOBAL binlog_checksum = ?" [MySQLText setting])

createTable :: MySQLConn -> IO ()
createTable c = do
    void (execute_ c "DROP TABLE IF EXISTS binlog_checksum_switch")
    void (execute_ c "CREATE TABLE binlog_checksum_switch (id INT)")

insertIds :: MySQLConn -> [Int32] -> IO ()
insertIds c = mapM_ (insertId c)

insertId :: MySQLConn -> Int32 -> IO ()
insertId c i = void (execute c "INSERT INTO binlog_checksum_switch VALUES (?)" [MySQLInt32 i])

-- | The ids of the first @count@ rows written to the table from @tracker@ on,
-- or Nothing when they do not arrive within 10 s. The dump's connection is
-- left open, as 'close' would wait for its next event (issue #35).
dumpedIds :: SlaveID -> BinLogTracker -> Int -> IO (Maybe [Int32])
dumpedIds slaveId tracker count = do
    c <- connect testConnectInfo
    rowEvents <- dumpBinLog c slaveId tracker False >>= decodeRowBinLogEvent
    ids <- newIORef []
    arrived <- timeout tenSeconds (readIds count rowEvents ids)
    case arrived of
        Just () -> Just . reverse <$> readIORef ids
        Nothing -> pure Nothing

-- | Collects the ids newest first.
readIds :: Int -> Stream.InputStream RowBinLogEvent -> IORef [Int32] -> IO ()
readIds count rowEvents ids = do
    event <- Stream.read rowEvents
    newIds <- mapM rowId (maybe [] writtenRows event)
    modifyIORef' ids (reverse newIds ++)
    seen <- readIORef ids
    if length seen >= count then pure () else readIds count rowEvents ids

writtenRows :: RowBinLogEvent -> [[BinLogValue]]
writtenRows event = case event of
    RowWriteEvent _ _ table rows ->
        if tmTableName table == "binlog_checksum_switch" then writeRowData rows else []
    RowUpdateEvent {} -> []
    RowDeleteEvent {} -> []
    RowQueryEvent {} -> []

rowId :: [BinLogValue] -> IO Int32
rowId row = case row of
    [BinLogLong i] -> pure i
    _ -> assertFailure ("a row of binlog_checksum_switch decodes as " ++ show row)

tenSeconds :: Int
tenSeconds = 10000000
