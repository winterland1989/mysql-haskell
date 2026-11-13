{-# LANGUAGE ScopedTypeVariables #-}
-- | Prepared statement parameters produced in chunks and sent ahead of the
-- execute command (COM_STMT_SEND_LONG_DATA), so a long value is never built
-- in memory as one 'MySQLValue'.
module StreamedParameters (tests) where

import           Control.Exception   (try)
import           Control.Monad       (void)
import qualified Data.ByteString     as B
import           Data.IORef          (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Word           (Word16)
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: TestTree
tests = testGroup "streamed parameters"
    [ testCase "50 MiB in 1 MiB chunks is stored as executeStmt stores it" $
        withStreamTable $ \c -> do
            let value = patterned (50 * mebibyte)
            assertStoredAlike c (chunksOf mebibyte value)
    , testCase "a chunk longer than a packet is stored whole" $
        withStreamTable $ \c ->
            assertStoredAlike c [patterned (17 * mebibyte + 3)]
    , testCase "two streamed parameters keep their own values" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content, other) VALUES (?, ?, ?)"
            first <- bytesFrom ["first ", "value"]
            second <- bytesFrom ["second value"]
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedBytes first, StreamedBytes second])
            selectAll c "SELECT content, other FROM streamed_param"
                >>= assertEqual "stored values" [[MySQLBytes "first value", MySQLBytes "second value"]]
    , testCase "values around a streamed parameter keep their places" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content, label, other) VALUES (?, ?, ?, ?)"
            content <- bytesFrom ["streamed"]
            void (executeStmtStreamed c stmt
                [InlineParam (MySQLInt32 7), StreamedBytes content, InlineParam (MySQLText "after"), InlineParam MySQLNull])
            selectAll c "SELECT id, content, label, other FROM streamed_param"
                >>= assertEqual "stored values" [[MySQLInt32 7, MySQLBytes "streamed", MySQLText "after", MySQLNull]]
    , testCase "an empty first chunk gives an empty value, not NULL" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content) VALUES (?, ?)"
            content <- bytesFrom []
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedBytes content])
            selectAll c "SELECT content FROM streamed_param" >>= assertEqual "stored value" [[MySQLBytes ""]]
    , testCase "streamed text is stored as the text it was" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, note) VALUES (?, ?)"
            note <- producerOf T.empty (map T.pack ["caf", "\233 ", "\9749"])
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedText note])
            selectAll c "SELECT note FROM streamed_param" >>= assertEqual "stored text" [[MySQLText (T.pack "caf\233 \9749")]]
    , testCase "a producer is not called after the empty chunk that ends its value" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content) VALUES (?, ?)"
            chunks <- newIORef ["value", B.empty]
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedBytes (chunkOrFail chunks)])
            selectAll c "SELECT content FROM streamed_param" >>= assertEqual "stored value" [[MySQLBytes "value"]]
    , testCase "queryStmtStreamed returns the statement's rows" $
        withTestConnection $ \c -> do
            stmt <- prepareStmt c "SELECT HEX(?), ?"
            content <- bytesFrom ["ab", "c"]
            (_, rows) <- queryStmtStreamed c stmt [StreamedBytes content, InlineParam (MySQLText "inline")]
            Stream.toList rows >>= assertEqual "rows" [[MySQLText "616263", MySQLText "inline"]]
    , testCase "after a streamed execute the statement takes values inline and after a reset streams again" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content) VALUES (?, ?)"
            streamed <- bytesFrom ["streamed"]
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedBytes streamed])
            void (executeStmt c stmt [MySQLInt32 2, MySQLBytes "inline"])
            resetStmt c stmt
            again <- bytesFrom ["after reset"]
            void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 3), StreamedBytes again])
            selectAll c "SELECT id, content FROM streamed_param ORDER BY id"
                >>= assertEqual "stored rows"
                    [ [MySQLInt32 1, MySQLBytes "streamed"]
                    , [MySQLInt32 2, MySQLBytes "inline"]
                    , [MySQLInt32 3, MySQLBytes "after reset"] ]
    , testCase "a refused execute leaves the statement executable" $
        withStreamTable $ \c -> do
            stmt <- prepareStmt c "INSERT INTO streamed_param (id, content) VALUES (?, ?)"
            extra <- bytesFrom ["for a third parameter the statement lacks"]
            -- ER_WRONG_ARGUMENTS, which the server repeats for every later
            -- execute of the statement until it is reset
            assertRefused 1210 (executeStmtStreamed c stmt
                [InlineParam (MySQLInt32 1), InlineParam (MySQLBytes "x"), StreamedBytes extra])
            void (executeStmt c stmt [MySQLInt32 2, MySQLBytes "next"])
            selectAll c "SELECT id, content FROM streamed_param" >>= assertEqual "stored rows" [[MySQLInt32 2, MySQLBytes "next"]]
    , testCase "an unknown statement is refused and the connection stays usable" $
        withTestConnection $ \c -> do
            content <- bytesFrom ["for no statement"]
            -- ER_UNKNOWN_STMT_HANDLER, also the answer to the reset that follows it
            assertRefused 1243 (executeStmtStreamed c 4000000000 [StreamedBytes content])
            selectAll c "SELECT 'next'" >>= assertEqual "the next query's rows" [[MySQLText "next"]]
    , testCase "more parameters than a statement can have are refused before sending" $
        withTestConnection $ \c -> do
            stmt <- prepareStmt c "SELECT 1"
            outcome <- try (executeStmtStreamed c stmt (replicate 65536 (InlineParam MySQLNull)))
            case outcome of
                Left WrongParamsCount -> pure ()
                Right ok -> assertFailure ("the server answered " ++ show ok)
            selectAll c "SELECT 'next'" >>= assertEqual "the next query's rows" [[MySQLText "next"]]
    , testCase "a streamed value longer than max_allowed_packet is refused, and the statement runs again" $
        withTestConnection $ \c -> do
            limit <- maxAllowedPacket c
            stmt <- prepareStmt c "SELECT HEX(?)"
            -- The same chunk over and over, so the client holds 1 MiB of it.
            tooLong <- bytesFrom (replicate (limit `div` mebibyte + 1) (B.replicate mebibyte 0x61))
            -- ER_UNKNOWN_ERROR, "longer than 'max_allowed_packet' bytes"
            assertRefused 1105 (queryStmtStreamed c stmt [StreamedBytes tooLong] >>= Stream.toList . snd)
            (_, rows) <- queryStmt c stmt [MySQLBytes "ok"]
            Stream.toList rows >>= assertEqual "rows" [[MySQLText "6F6B"]]
    , testCase "a producer that throws part-way makes the connection refuse commands" $
        withTestConnection $ \c -> do
            stmt <- prepareStmt c "SELECT HEX(?)"
            chunks <- newIORef ["sent", "also sent"]
            outcome <- try (queryStmtStreamed c stmt [StreamedBytes (chunkOrFail chunks)] >>= Stream.toList . snd)
            case outcome of
                Left (failure :: IOError) -> assertEqual "the producer's exception" "user error (producer failed)" (show failure)
                Right rows -> assertFailure ("the statement ran on part of its value: " ++ show rows)
            next <- try (selectAll c "SELECT 'next'")
            case next of
                Left AbandonedReply -> pure ()
                Right rows -> assertFailure ("the next query returned " ++ show rows)
    ]

-- | Insert the chunks streamed and their concatenation through 'executeStmt',
-- then compare what the server stored for both.
assertStoredAlike :: MySQLConn -> [B.ByteString] -> Assertion
assertStoredAlike c chunks = do
    stmt <- prepareStmt c "INSERT INTO streamed_param (id, content) VALUES (?, ?)"
    content <- bytesFrom chunks
    void (executeStmtStreamed c stmt [InlineParam (MySQLInt32 1), StreamedBytes content])
    void (executeStmt c stmt [MySQLInt32 2, MySQLBytes (B.concat chunks)])
    stored <- selectAll c "SELECT CAST(LENGTH(content) AS CHAR), SHA2(content, 256) FROM streamed_param ORDER BY id"
    case stored of
        [streamed@[streamedLength, _], inline] -> do
            assertEqual "streamed length" (MySQLText (T.pack (show (sum (map B.length chunks))))) streamedLength
            assertEqual "streamed and inline value" inline streamed
        other -> assertFailure ("rows: " ++ show other)

withStreamTable :: (MySQLConn -> IO a) -> IO a
withStreamTable action = withTestConnection $ \c -> do
    void (execute_ c "CREATE TEMPORARY TABLE streamed_param (id INT PRIMARY KEY, content LONGBLOB, \
                     \other LONGBLOB, label TEXT, note LONGTEXT CHARACTER SET utf8mb4)")
    action c

bytesFrom :: [B.ByteString] -> IO (IO B.ByteString)
bytesFrom = producerOf B.empty

-- | Each call gives the next of these chunks, then @end@ for good.
producerOf :: a -> [a] -> IO (IO a)
producerOf end chunks = do
    remaining <- newIORef chunks
    pure (fromMaybe end <$> atomicModifyIORef' remaining popChunk)

-- | The next of these chunks, throwing once they are used up.
chunkOrFail :: IORef [B.ByteString] -> IO B.ByteString
chunkOrFail chunks =
    atomicModifyIORef' chunks popChunk >>= maybe (ioError (userError "producer failed")) pure

popChunk :: [a] -> ([a], Maybe a)
popChunk chunks = case chunks of
    [] -> ([], Nothing)
    chunk : later -> (later, Just chunk)

maxAllowedPacket :: MySQLConn -> IO Int
maxAllowedPacket c = do
    rows <- selectAll c "SELECT CAST(@@max_allowed_packet AS CHAR)"
    case rows of
        [[MySQLText limit]] -> pure (read (T.unpack limit))
        other -> assertFailure ("max_allowed_packet: " ++ show other)

-- | Bytes of a period that does not divide a chunk, so a chunk stored twice,
-- dropped or out of order changes the value.
patterned :: Int -> B.ByteString
patterned size = B.take size (B.concat (replicate (size `div` 251 + 1) (B.pack [0 .. 250])))

chunksOf :: Int -> B.ByteString -> [B.ByteString]
chunksOf size bytes =
    if B.null bytes then [] else B.take size bytes : chunksOf size (B.drop size bytes)

mebibyte :: Int
mebibyte = 1048576

selectAll :: MySQLConn -> Query -> IO [[MySQLValue]]
selectAll c q = query_ c q >>= Stream.toList . snd

assertRefused :: Show a => Word16 -> IO a -> Assertion
assertRefused code action = do
    outcome <- try action
    case outcome of
        Left (ERRException err) -> assertEqual ("error " ++ show (errMsg err)) code (errCode err)
        Right answer -> assertFailure ("the server answered " ++ show answer)
