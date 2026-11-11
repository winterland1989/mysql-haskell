-- | Dates a 'Day' cannot hold, read and written back through both protocols.
-- They used to come back clamped to a real date (0000-00-00 as 0000-01-01),
-- and that date was what got written back.
module ZeroDateRoundtrip (tests) where

import           Control.Monad       (forM_, void)
import qualified Data.ByteString.Lazy as L
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: TestTree
tests = testCase "zero, partial and invalid dates survive a round trip" $
    withTestConnection $ \c -> do
        void (execute_ c "SET SESSION sql_mode = 'ALLOW_INVALID_DATES'")
        void (execute_ c "CREATE TEMPORARY TABLE zero_dates \
                         \(id INT, d DATE, dt DATETIME, ts TIMESTAMP NULL DEFAULT NULL)")
        void (execute_ c "INSERT INTO zero_dates VALUES \
                         \(1, '0000-00-00', '0000-00-00 00:00:00', '0000-00-00 00:00:00'), \
                         \(2, '2024-00-00', '2024-05-00 13:14:15', NULL), \
                         \(3, '2024-02-30', '2024-02-30 01:02:03', NULL)")

        (_, textRows) <- query_ c "SELECT d, dt, ts FROM zero_dates ORDER BY id"
        textValues <- Stream.toList textRows
        forM_ (zip [11 ..] textValues) $ \(newId, values) ->
            execute c "INSERT INTO zero_dates VALUES (?, ?, ?, ?)" (MySQLInt32 newId : values)

        select <- prepareStmt c "SELECT d, dt, ts FROM zero_dates WHERE id < 10 ORDER BY id"
        (_, binaryRows) <- queryStmt c select []
        binaryValues <- Stream.toList binaryRows
        insert <- prepareStmt c "INSERT INTO zero_dates VALUES (?, ?, ?, ?)"
        forM_ (zip [21 ..] binaryValues) $ \(newId, values) ->
            executeStmt c insert (MySQLInt32 newId : values)

        original <- storedAsText c "id < 10"
        assertEqual "values stored" 3 (length original)
        textCopies <- storedAsText c "id BETWEEN 11 AND 19"
        assertEqual "written back through the text protocol" original textCopies
        binaryCopies <- storedAsText c "id BETWEEN 21 AND 29"
        assertEqual "written back through the binary protocol" original binaryCopies

-- | The dates as the server prints them, which no decoding can clamp.
storedAsText :: MySQLConn -> L.ByteString -> IO [[MySQLValue]]
storedAsText c condition = do
    (_, rows) <- query_ c (Query ("SELECT CAST(d AS CHAR), CAST(dt AS CHAR), CAST(ts AS CHAR) \
                                  \FROM zero_dates WHERE " <> condition <> " ORDER BY id"))
    Stream.toList rows
