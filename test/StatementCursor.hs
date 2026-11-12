{-# LANGUAGE ScopedTypeVariables #-}
-- | 'queryStmtCursor' and 'queryStmtRowsCursor' against a server: the rows
-- come in fetches of the asked size as they are read, the connection takes
-- other commands between fetches, and a cursor left early or refused by the
-- server leaves the connection usable.
module StatementCursor (tests) where

import           Control.Exception      (IOException, try)
import           Control.Monad          (replicateM, void)
import           Data.Int               (Int32)
import           Data.Maybe             (catMaybes)
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.MySQL.Base
import qualified Database.MySQL.Decoder as Decode
import           System.IO.Streams      (InputStream)
import qualified System.IO.Streams      as Stream
import           System.Timeout         (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser               (withTestConnection)
import           Text.Read              (readMaybe)

tests :: TestTree
tests = testGroup "server-side cursors"
    [ testCase "10,000 rows read 100 at a time are queryStmt's rows" $
        withNumbers $ \c stmt -> do
            plain <- queryStmt c stmt [MySQLInt32 0] >>= Stream.toList . snd
            (columns, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            batched <- Stream.toList rows
            assertEqual "the columns" ["n", "label"] (map columnName columns)
            assertEqual "the rows against queryStmt's" plain batched
            assertEqual "the rows" (numbersFrom 0) batched
    , testCase "rows are fetched 100 at a time as they are read" $
        withNumbers $ \c stmt -> do
            before <- fetchCount c
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            firstRows <- replicateM 150 (Stream.read rows)
            afterFirstRows <- fetchCount c
            assertEqual "fetches for the first 150 rows" 2 (afterFirstRows - before)
            later <- Stream.toList rows
            afterAll <- fetchCount c
            -- The 100th fetch fills its batch exactly; the 101st learns it was the last.
            assertEqual "fetches for all rows" 101 (afterAll - before)
            assertEqual "the rows" (numbersFrom 0) (catMaybes firstRows ++ later)
    , testCase "two statements' cursors are read in turns" $
        withNumbers $ \c ascending -> do
            descending <- prepareStmt c
                "SELECT n, label FROM statement_cursor_numbers WHERE n >= ? ORDER BY n DESC"
            (_, up) <- queryStmtCursor c ascending [MySQLInt32 9000] 7
            (_, down) <- queryStmtCursor c descending [MySQLInt32 9000] 7
            (upRows, downRows) <- readInTurns up down
            assertEqual "the ascending rows" (numbersFrom 9000) upRows
            assertEqual "the descending rows" (reverse (numbersFrom 9000)) downRows
    , testCase "stopping part-way leaves the connection usable" $
        withNumbers $ \c stmt -> do
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            void (replicateM 150 (Stream.read rows))
            selectAll c "SELECT 'next'" >>= assertEqual "a query after stopping" [[MySQLText "next"]]
            resetStmt c stmt
            (_, again) <- queryStmtCursor c stmt [MySQLInt32 9990] 100
            Stream.toList again >>= assertEqual "the statement's next run" (numbersFrom 9990)
    , testCase "a fetch the server refuses raises ERRException and ends the stream" $
        withNumbers $ \c stmt -> do
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            firstBatch <- replicateM 100 (Stream.read rows)
            assertEqual "the first batch" (take 100 (numbersFrom 0)) (catMaybes firstBatch)
            resetStmt c stmt
            refused <- try (Stream.read rows)
            case refused of
                -- ER_STMT_HAS_NO_OPEN_CURSOR
                Left (ERRException err) -> assertEqual ("error " ++ show (errMsg err)) 1421 (errCode err)
                Right row -> assertFailure ("the fetch after resetStmt gave " ++ show row)
            Stream.read rows >>= assertEqual "a read after the refusal" Nothing
            selectAll c "SELECT 'next'" >>= assertEqual "a query after the refusal" [[MySQLText "next"]]
    , testCase "an empty result gives the columns and no rows" $
        withNumbers $ \c stmt -> do
            (columns, rows) <- queryStmtCursor c stmt [MySQLInt32 10000] 100
            assertEqual "the columns" ["n", "label"] (map columnName columns)
            Stream.toList rows >>= assertEqual "the rows" []
            selectAll c "SELECT 'next'" >>= assertEqual "a query afterwards" [[MySQLText "next"]]
    , testCase "a statement without a result set gives no columns and runs" $
        withNumbers $ \c _ -> do
            insert <- prepareStmt c "INSERT INTO statement_cursor_numbers VALUES (?, 'inserted')"
            (columns, rows) <- queryStmtCursor c insert [MySQLInt32 10000] 100
            assertEqual "the columns" 0 (length columns)
            Stream.toList rows >>= assertEqual "the rows" []
            selectAll c "SELECT label FROM statement_cursor_numbers WHERE n = 10000"
                >>= assertEqual "the inserted row" [[MySQLText "inserted"]]
    , testCase "a CALL, which gets no cursor, gives its rows" $
        withNumbers $ \c _ -> do
            void (execute_ c "DROP PROCEDURE IF EXISTS statement_cursor_call")
            void (execute_ c "CREATE PROCEDURE statement_cursor_call() \
                             \SELECT n FROM statement_cursor_numbers WHERE n < 3 ORDER BY n")
            call <- prepareStmt c "CALL statement_cursor_call()"
            (_, rows) <- queryStmtCursor c call [] 1
            Stream.toList rows >>= assertEqual "the rows" [[MySQLInt32 0], [MySQLInt32 1], [MySQLInt32 2]]
            selectAll c "SELECT 'next'" >>= assertEqual "a query afterwards" [[MySQLText "next"]]
    , testCase "queryStmt and executeStmt run a statement first run through a cursor" $
        withNumbers $ \c _ -> do
            stmt <- prepareStmt c "SELECT n FROM statement_cursor_numbers WHERE n >= ? ORDER BY n"
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 9997] 2
            Stream.toList rows >>= assertEqual "the cursor's rows" (map single [9997 .. 9999])
            plain <- timeout 10000000 (queryStmt c stmt [MySQLInt32 9998] >>= Stream.toList . snd)
            assertEqual "queryStmt's rows" (Just (map single [9998, 9999])) plain
            executeRefusingRows c stmt 9998
            selectAll c "SELECT 'next'" >>= assertEqual "a query afterwards" [[MySQLText "next"]]
    -- MySQL keeps the rows of an execution nobody fetched in the statement's
    -- cursor, ahead of the next execution's rows.
    , testCase "queryStmt after a cursor and an executeStmt gives only its own rows" $
        withNumbers $ \c stmt -> do
            readThroughCursor c stmt
            executeRefusingRows c stmt 9998
            plain <- timeout 10000000 (queryStmt c stmt [MySQLInt32 9998] >>= Stream.toList . snd)
            assertEqual "queryStmt's rows" (Just (numbersFrom 9998)) plain
    , testCase "queryStmtCursor after a cursor and an executeStmt gives only its own rows" $
        withNumbers $ \c stmt -> do
            readThroughCursor c stmt
            executeRefusingRows c stmt 9998
            queryStmtCursor c stmt [MySQLInt32 9998] 100 >>= Stream.toList . snd
                >>= assertEqual "the cursor's rows" (numbersFrom 9998)
    , testCase "queryStmt after a cursor and two executeStmts gives only its own rows" $
        withNumbers $ \c stmt -> do
            readThroughCursor c stmt
            executeRefusingRows c stmt 9998
            executeRefusingRows c stmt 9997
            plain <- timeout 10000000 (queryStmt c stmt [MySQLInt32 9999] >>= Stream.toList . snd)
            assertEqual "queryStmt's rows" (Just (numbersFrom 9999)) plain
    , testCase "queryStmt and queryStmtCursor after a cursor left part-way give only their own rows" $
        withNumbers $ \c stmt -> do
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            void (replicateM 150 (Stream.read rows))
            queryStmt c stmt [MySQLInt32 9998] >>= Stream.toList . snd
                >>= assertEqual "queryStmt's rows" (numbersFrom 9998)
            queryStmtCursor c stmt [MySQLInt32 9997] 100 >>= Stream.toList . snd
                >>= assertEqual "the next cursor's rows" (numbersFrom 9997)
    , testCase "closing the statement ends its cursor's stream" $
        withNumbers $ \c stmt -> do
            (_, rows) <- queryStmtCursor c stmt [MySQLInt32 0] 100
            void (replicateM 200 (Stream.read rows))
            closeStmt c stmt
            refused <- try (Stream.read rows)
            case refused of
                -- ER_UNKNOWN_STMT_HANDLER
                Left (ERRException err) -> assertEqual ("error " ++ show (errMsg err)) 1243 (errCode err)
                Right row -> assertFailure ("the fetch after closeStmt gave " ++ show row)
            Stream.read rows >>= assertEqual "a read after the refusal" Nothing
            again <- prepareStmt c "SELECT n, label FROM statement_cursor_numbers WHERE n >= ? ORDER BY n"
            queryStmt c again [MySQLInt32 9998] >>= Stream.toList . snd
                >>= assertEqual "the statement prepared again" (numbersFrom 9998)
    , testCase "0 rows per fetch is refused before the statement runs" $
        withNumbers $ \c stmt -> do
            refused <- try (queryStmtCursor c stmt [MySQLInt32 0] 0)
            case refused of
                Left (_ :: IOException) -> pure ()
                Right (columns, _) -> assertFailure ("the statement ran, giving columns " ++ show columns)
            queryStmt c stmt [MySQLInt32 9999] >>= Stream.toList . snd
                >>= assertEqual "the statement's next run" (numbersFrom 9999)
    , testCase "queryStmtRowsCursor decodes the rows queryStmtRows does" $
        withNumbers $ \c stmt -> do
            plain <- queryStmtRows numberDecoder c stmt [MySQLInt32 0] >>= Stream.toList . snd
            batched <- queryStmtRowsCursor numberDecoder c stmt [MySQLInt32 0] 100 >>= Stream.toList . snd
            assertEqual "the rows against queryStmtRows's" plain batched
            assertEqual "the rows" (map numberPair [0 .. 9999]) batched
    , testCase "queryStmtRowsCursor refuses a decoder that does not fit" $
        withNumbers $ \c stmt -> do
            refused <- try (queryStmtRowsCursor (Decode.field Decode.int32) c stmt [MySQLInt32 0] 100)
            case refused of
                Left (_ :: Decode.ColumnMismatch) -> pure ()
                Right (columns, _) -> assertFailure ("the decoder took columns " ++ show columns)
            selectAll c "SELECT 'next'" >>= assertEqual "a query afterwards" [[MySQLText "next"]]
    ]

-- | A connection with the table @statement_cursor_numbers@ holding the rows
-- 'numbersFrom' gives, and a statement selecting those from a number up.
withNumbers :: (MySQLConn -> StmtID -> IO a) -> IO a
withNumbers run = withTestConnection $ \c -> do
    void (execute_ c "DROP TABLE IF EXISTS statement_cursor_numbers")
    void (execute_ c "CREATE TABLE statement_cursor_numbers (n INT PRIMARY KEY, label VARCHAR(16) NOT NULL)")
    void (execute_ c fillNumbers)
    stmt <- prepareStmt c "SELECT n, label FROM statement_cursor_numbers WHERE n >= ? ORDER BY n"
    run c stmt

-- | The numbers 0 to 9999, from four digits.
fillNumbers :: Query
fillNumbers = fromString $
    "INSERT INTO statement_cursor_numbers SELECT n, CONCAT('row ', n) FROM \
    \(SELECT ones.d + 10 * tens.d + 100 * hundreds.d + 1000 * thousands.d AS n FROM "
    ++ digits "ones" ++ " CROSS JOIN " ++ digits "tens" ++ " CROSS JOIN "
    ++ digits "hundreds" ++ " CROSS JOIN " ++ digits "thousands" ++ ") AS numbers"

digits :: String -> String
digits name =
    "(SELECT 0 AS d" ++ concatMap ((" UNION ALL SELECT " ++) . show) [1 .. 9 :: Int] ++ ") AS " ++ name

numbersFrom :: Int32 -> [[MySQLValue]]
numbersFrom from = map numberRow [from .. 9999]

numberRow :: Int32 -> [MySQLValue]
numberRow n = [MySQLInt32 n, MySQLText (snd (numberPair n))]

numberPair :: Int32 -> (Int32, Text)
numberPair n = (n, T.pack ("row " ++ show n))

numberDecoder :: Decode.RowDecoder (Int32, Text)
numberDecoder = (,) <$> Decode.field Decode.int32 <*> Decode.field Decode.text

single :: Int32 -> [MySQLValue]
single n = [MySQLInt32 n]

-- | The statement's first execution, through a cursor read to the end.
readThroughCursor :: MySQLConn -> StmtID -> IO ()
readThroughCursor c stmt =
    queryStmtCursor c stmt [MySQLInt32 9999] 100 >>= Stream.toList . snd
        >>= assertEqual "the cursor's rows" (numbersFrom 9999)

-- | 'executeStmt' on a statement with rows, which it reads and refuses.
executeRefusingRows :: MySQLConn -> StmtID -> Int32 -> Assertion
executeRefusingRows c stmt from = do
    executed <- timeout 10000000 (try (executeStmt c stmt [MySQLInt32 from]))
    case executed of
        Just (Left ExtraResultSets) -> pure ()
        Just (Right ok) -> assertFailure ("executeStmt gave " ++ show ok)
        Nothing -> assertFailure "executeStmt waited for rows that never came"

-- | The session's COM_STMT_FETCH count, which both servers keep.
fetchCount :: MySQLConn -> IO Int
fetchCount c = do
    status <- selectAll c "SHOW SESSION STATUS LIKE 'Com_stmt_fetch'"
    case status of
        [[_, MySQLText count]] -> maybe (assertFailure ("not a count: " ++ show count)) pure
            (readMaybe (T.unpack count))
        _ -> assertFailure ("unexpected status rows " ++ show status)

-- | Both streams to their ends, one row from each in turn.
readInTurns :: InputStream a -> InputStream a -> IO ([a], [a])
readInTurns first second = do
    fromFirst <- Stream.read first
    fromSecond <- Stream.read second
    case (fromFirst, fromSecond) of
        (Nothing, Nothing) -> pure ([], [])
        _ -> do
            (firstRest, secondRest) <- readInTurns first second
            pure (maybe firstRest (: firstRest) fromFirst, maybe secondRest (: secondRest) fromSecond)

selectAll :: MySQLConn -> Query -> IO [[MySQLValue]]
selectAll c q = query_ c q >>= Stream.toList . snd
