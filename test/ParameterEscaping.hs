-- | Values that 'query', 'execute' and 'executeMany' render into the query
-- text, read back under the default sql_mode and under NO_BACKSLASH_ESCAPES.
-- In that mode a backslash is an ordinary character, so a quote escaped as
-- backslash-quote used to end the string literal and let the rest of the
-- value run as SQL.
module ParameterEscaping (tests) where

import qualified Data.ByteString     as B
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Word           (Word8)
import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: QueryApi -> TestTree
tests api = testGroup "parameters rendered into the query text"
    [ testGroup "NO_BACKSLASH_ESCAPES"
        [ testCase "a quote in a query parameter does not end the literal" $
            withUsers $ \c -> do
                (_, rows) <- apiQuery api (ColumnCount 2) c
                    "SELECT name, secret FROM escaping_users WHERE name = ?"
                    [One (MySQLText endsLiteral)]
                found <- Stream.toList rows
                assertEqual "rows matching the name" [] found
        , testCase "a quote in an executeMany parameter does not end the literal" $
            withUsers $ \c -> do
                oks <- executeMany c
                    "UPDATE escaping_users SET secret = 'changed' WHERE name = ?"
                    [[MySQLText endsLiteral]]
                assertEqual "rows changed" 0 (sum (map okAffectedRows oks))
        , testCase "text with quotes and control characters round-trips" $
            withTestConnection $ \c -> do
                setNoBackslashEscapes c
                assertTextRoundTrip api c (T.filter (/= '\\') trickyText)
        , testCase "every byte but the backslash round-trips" $
            withTestConnection $ \c -> do
                setNoBackslashEscapes c
                assertBytesRoundTrip api c (B.filter (/= backslash) everyByte)
        ]
    , testGroup "default sql_mode"
        [ testCase "text with quotes, backslashes and control characters round-trips" $
            withTestConnection $ \c -> assertTextRoundTrip api c trickyText
        , testCase "every byte round-trips" $
            withTestConnection $ \c -> assertBytesRoundTrip api c everyByte
        ]
    ]

-- | A name that matches every row once its quote ends the string literal.
endsLiteral :: Text
endsLiteral = "nobody' OR 1=1 #"

-- | Ends in a backslash, which in the default mode escapes the closing quote
-- unless it is escaped itself.
trickyText :: Text
trickyText = "it's \"quoted\" '' \\' \\\\ \0 \n \r \t \b \SUB % _ \252nic\246de \128512 \\"

everyByte :: B.ByteString
everyByte = B.pack [minBound .. maxBound]

backslash :: Word8
backslash = 0x5C

-- | A connection under NO_BACKSLASH_ESCAPES with one row in @escaping_users@
-- whose name is not 'endsLiteral'.
withUsers :: (MySQLConn -> IO a) -> IO a
withUsers run = withTestConnection $ \c -> do
    _ <- execute_ c "CREATE TEMPORARY TABLE escaping_users (name VARCHAR(64), secret VARCHAR(64))"
    _ <- execute_ c "INSERT INTO escaping_users VALUES ('alice', 'hunter2')"
    setNoBackslashEscapes c
    run c

setNoBackslashEscapes :: MySQLConn -> IO ()
setNoBackslashEscapes c = do
    _ <- execute_ c "SET SESSION sql_mode = 'NO_BACKSLASH_ESCAPES'"
    pure ()

-- | Inserts with 'execute' and selects with the value in the WHERE clause, so
-- both the stored value and the literal compared against it must be exact.
assertTextRoundTrip :: QueryApi -> MySQLConn -> Text -> Assertion
assertTextRoundTrip api c text = do
    _ <- execute_ c "CREATE TEMPORARY TABLE escaping_text (value VARCHAR(255) CHARACTER SET utf8mb4)"
    _ <- execute c "INSERT INTO escaping_text VALUES (?)" [MySQLText text]
    (_, rows) <- apiQuery api (ColumnCount 1) c "SELECT value FROM escaping_text WHERE value = ?"
        [One (MySQLText text)]
    found <- Stream.toList rows
    assertEqual "rows read back" [[MySQLText text]] found

assertBytesRoundTrip :: QueryApi -> MySQLConn -> B.ByteString -> Assertion
assertBytesRoundTrip api c bytes = do
    _ <- execute_ c "CREATE TEMPORARY TABLE escaping_bytes (value VARBINARY(512))"
    _ <- execute c "INSERT INTO escaping_bytes VALUES (?)" [MySQLBytes bytes]
    (_, rows) <- apiQuery api (ColumnCount 1) c "SELECT value FROM escaping_bytes WHERE value = ?"
        [One (MySQLBytes bytes)]
    found <- Stream.toList rows
    assertEqual "rows read back" [[MySQLBytes bytes]] found
