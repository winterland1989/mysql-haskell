-- | 'queryRows_', 'queryStmtRows' and 'queryRawRows_' against a server. The
-- typed decoders must give what 'query_' and 'queryStmt' give through
-- "Database.MySQL.Field", and a result set a decoder does not fit, or a field
-- it cannot decode, must leave the connection usable.
module RowDecoderQueries (tests) where

import           Control.Exception      (try)
import           Data.Int               (Int32)
import           Data.Scientific        (Scientific)
import           Data.Text              (Text)
import           Data.Time.Calendar     (Day, fromGregorian)
import           Data.Time.LocalTime    (LocalTime (..), TimeOfDay (..))
import           Database.MySQL.Base
import qualified Database.MySQL.Decoder as Decode
import           Database.MySQL.Field
import qualified System.IO.Streams      as Stream
import           System.Timeout         (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

type Row = (Int32, Text, Day, LocalTime, Double, Scientific, Maybe Int32, Bool, TimeOfDay)

rowDecoder :: Decode.RowDecoder Row
rowDecoder = (,,,,,,,,)
    <$> Decode.field Decode.int32
    <*> Decode.field Decode.text
    <*> Decode.field Decode.day
    <*> Decode.field Decode.localTime
    <*> Decode.field Decode.double
    <*> Decode.field Decode.scientific
    <*> Decode.field (Decode.nullable Decode.int32)
    <*> Decode.field Decode.bool
    <*> Decode.field Decode.timeOfDay

expectedRows :: [Row]
expectedRows =
    [ ( 1, "Jappie", fromGregorian 1990 1 2
      , LocalTime (fromGregorian 2026 10 8) (TimeOfDay 12 34 56.789012)
      , 0.25, 12.5, Nothing, True, TimeOfDay 12 0 1 )
    , ( 2, "café", fromGregorian 2000 2 29
      , LocalTime (fromGregorian 2000 2 29) (TimeOfDay 23 59 59.000001)
      , -1.5, -3.25, Just 7, False, TimeOfDay 838 59 59 )
    ]

-- | The same row through 'MySQLValue' and "Database.MySQL.Field".
viaField :: [MySQLValue] -> Either String Row
viaField values = case values of
    [number, name, born, created, ratio, amount, count, flag, duration] ->
        either (Left . show) Right $ (,,,,,,,,)
            <$> decodeInt32 number
            <*> decodeText name
            <*> decodeDay born
            <*> decodeLocalTime created
            <*> decodeDouble ratio
            <*> decodeScientific amount
            <*> decodeMaybe count
            <*> decodeBool flag
            <*> decodeTimeOfDay duration
    _ -> Left ("expected 9 values, got " ++ show values)

tests :: TestTree
tests = testGroup "decoding rows without MySQLValue"
    [ testCase "queryRows_ gives what query_ and Field give" $ withRows $ \c -> do
        (_, rows) <- queryRows_ rowDecoder c "SELECT * FROM decoded_rows ORDER BY id"
        decoded <- Stream.toList rows
        (_, values) <- query_ c "SELECT * FROM decoded_rows ORDER BY id"
        throughField <- Stream.toList values
        decoded @?= expectedRows
        traverse viaField throughField @?= Right expectedRows
    , testCase "queryStmtRows gives what queryStmt and Field give" $ withRows $ \c -> do
        stmt <- prepareStmt c "SELECT * FROM decoded_rows WHERE id >= ? ORDER BY id"
        (_, rows) <- queryStmtRows rowDecoder c stmt [MySQLInt32 0]
        decoded <- Stream.toList rows
        (_, values) <- queryStmt c stmt [MySQLInt32 0]
        throughField <- Stream.toList values
        decoded @?= expectedRows
        traverse viaField throughField @?= Right expectedRows
    , testCase "queryRawRows_ rows decode by column number" $ withRows $ \c -> do
        (columns, rows) <- queryRawRows_ c "SELECT id, name FROM decoded_rows ORDER BY id"
        parser <- either (assertFailure . show) pure
            (Decode.prepareFieldParser Decode.text 1 (columns !! 1))
        names <- Stream.toList =<< Stream.mapM
            (\row -> Decode.runFieldParser parser row 1 (assertFailure . show) pure) rows
        names @?= ["Jappie", "café"]
    , testCase "a column the decoder does not take raises before any row" $ withRows $ \c -> do
        outcome <- try (queryRows_ (Decode.field Decode.text) c "SELECT id FROM decoded_rows")
        case outcome of
            Left mismatch -> mismatch @?= Decode.ColumnTypeMismatch 0 "id" mySQLTypeLong "Text"
            Right _       -> assertFailure "the INT column was accepted as Text"
        assertRowCount c
    , testCase "a decoder for another number of columns raises before any row" $ withRows $ \c -> do
        stmt <- prepareStmt c "SELECT id, name FROM decoded_rows"
        outcome <- try (queryStmtRows (Decode.field Decode.int32) c stmt [])
        case outcome of
            Left mismatch -> mismatch @?= Decode.ColumnCountMismatch 1 2
            Right _       -> assertFailure "a 2-column result set fit a 1-column decoder"
        assertRowCount c
    , testCase "a field that does not decode raises and skips the other rows" $ withRows $ \c -> do
        (_, rows) <- queryRows_ (Decode.field Decode.int32) c
            "SELECT maybe_count FROM decoded_rows ORDER BY id"
        outcome <- try (Stream.toList rows)
        case outcome of
            Left fieldError -> fieldError @?= Decode.FieldError 0 Decode.FieldUnexpectedNull
            Right counts    -> assertFailure ("the NULL decoded: " ++ show counts)
        assertRowCount c
    , testCase "an INSERT gives no columns and no rows" $ withRows $ \c -> do
        (columns, rows) <- queryRows_ rowDecoder c
            "INSERT INTO decoded_rows (id, name, born, created, ratio, amount, flag, duration) \
            \VALUES (3, 'x', '2001-01-01', '2001-01-01', 0, 0, 0, '00:00:00')"
        decoded <- Stream.toList rows
        (length columns, length decoded) @?= (0, 0)
    ]

-- | Generous for a few statements on a two-row temporary table, and short
-- enough that a hang fails the suite instead of stalling CI.
replyTimeLimitMicroseconds :: Int
replyTimeLimitMicroseconds = 10000000

withRows :: (MySQLConn -> Assertion) -> Assertion
withRows body = do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }
    _ <- execute_ c
        "CREATE TEMPORARY TABLE decoded_rows (\
        \id INT, name VARCHAR(20), born DATE, created DATETIME(6), ratio DOUBLE, \
        \amount DECIMAL(10, 2), maybe_count INT NULL, flag TINYINT(1), duration TIME\
        \) CHARACTER SET utf8mb4"
    _ <- execute_ c
        "INSERT INTO decoded_rows VALUES \
        \(1, 'Jappie', '1990-01-02', '2026-10-08 12:34:56.789012', 0.25, 12.50, NULL, 1, '12:00:01'), \
        \(2, 'café', '2000-02-29', '2000-02-29 23:59:59.000001', -1.5, -3.25, 7, 0, '838:59:59')"
    finished <- timeout replyTimeLimitMicroseconds (body c)
    case finished of
        Nothing -> assertFailure "blocked for 10 s waiting for a reply"
        Just () -> close c

-- | The connection must be back in step: a fresh query gets its own rows.
assertRowCount :: MySQLConn -> Assertion
assertRowCount c = do
    (_, rows) <- query_ c "SELECT COUNT(*) FROM decoded_rows"
    counts <- Stream.toList rows
    assertEqual "rows of the next query" [[MySQLInt64 2]] counts
