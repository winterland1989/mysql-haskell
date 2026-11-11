-- | Text and bytes rendered into the query text as single-quoted string
-- literals, read back the way the server's lexer reads them, with and
-- without NO_BACKSLASH_ESCAPES.
module StringLiterals (tests) where

import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as L
import           Data.Maybe                         (isJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Word                          (Word8)
import           Database.MySQL.Base
import           Test.QuickCheck                    (Gen, arbitrary, elements,
                                                     forAll, frequency, listOf)
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (testProperty, (===))

tests :: TestTree
tests = testGroup "string literals"
    [ testCase "a quote and a backslash are doubled, other characters kept" $
        rendered (MySQLText "it's\n\"C:\\\"") @?= "'it''s\n\"C:\\\\\"'"
    , testGroup "default sql_mode"
        [ testProperty "bytes read back exactly" $ forAll genBytes $ \bytes ->
            readLiteral BackslashEscapes (rendered (MySQLBytes bytes)) === Just bytes
        , testProperty "text reads back exactly" $ forAll genText $ \text ->
            readLiteral BackslashEscapes (rendered (MySQLText text)) === Just (T.encodeUtf8 text)
        ]
    , testGroup "NO_BACKSLASH_ESCAPES"
        [ testProperty "bytes end the literal at its closing quote" $ forAll genBytes $ \bytes ->
            isJust (readLiteral NoBackslashEscapes (rendered (MySQLBytes bytes)))
        , testProperty "text ends the literal at its closing quote" $ forAll genText $ \text ->
            isJust (readLiteral NoBackslashEscapes (rendered (MySQLText text)))
        , testProperty "bytes without a backslash read back exactly" $
            forAll (B.filter (/= backslash) <$> genBytes) $ \bytes ->
                readLiteral NoBackslashEscapes (rendered (MySQLBytes bytes)) === Just bytes
        , testProperty "text without a backslash reads back exactly" $
            forAll (T.filter (/= '\\') <$> genText) $ \text ->
                readLiteral NoBackslashEscapes (rendered (MySQLText text)) === Just (T.encodeUtf8 text)
        ]
    ]

rendered :: MySQLValue -> ByteString
rendered value = L.toStrict (fromQuery (renderParams "?" [value]))

data BackslashMode = BackslashEscapes | NoBackslashEscapes

-- | The value the server reads from a whole single-quoted literal, following
-- <https://dev.mysql.com/doc/refman/8.0/en/string-literals.html>; Nothing
-- when the literal ends before the input does, or not at all. Reading bytes
-- is exact for UTF-8, where a quote or backslash byte never belongs to a
-- longer character.
readLiteral :: BackslashMode -> ByteString -> Maybe ByteString
readLiteral mode literal = case B.unpack literal of
    [] -> Nothing
    open : body -> if open == quote then B.pack <$> readBody mode body else Nothing

readBody :: BackslashMode -> [Word8] -> Maybe [Word8]
readBody mode body = case body of
    [] -> Nothing
    byte : rest -> if
        | byte == quote -> case rest of
            [] -> Just []
            next : afterNext ->
                if next == quote then (quote :) <$> readBody mode afterNext else Nothing
        | byte == backslash -> case mode of
            NoBackslashEscapes -> (backslash :) <$> readBody mode rest
            BackslashEscapes -> case rest of
                [] -> Nothing
                escaped : afterEscaped -> (escapedBytes escaped ++) <$> readBody mode afterEscaped
        | otherwise -> (byte :) <$> readBody mode rest

-- | What a backslash followed by this byte stands for in the default mode.
escapedBytes :: Word8 -> [Word8]
escapedBytes escaped = case lookup escaped escapeSequences of
    Just meaning -> [meaning]
    Nothing -> if escaped == 0x25 || escaped == 0x5F then [backslash, escaped] else [escaped]

-- | @\\0 \\b \\n \\r \\t \\Z@; @\\%@ and @\\_@ keep their backslash, any other
-- escaped byte stands for itself.
escapeSequences :: [(Word8, Word8)]
escapeSequences = [(0x30, 0), (0x62, 8), (0x6E, 10), (0x72, 13), (0x74, 9), (0x5A, 26)]

quote, backslash :: Word8
quote = 0x27
backslash = 0x5C

-- | Half the bytes are ones the escaping or the lexer treats specially.
genBytes :: Gen ByteString
genBytes = B.pack <$> listOf (frequency
    [(1, elements (B.unpack (T.encodeUtf8 (T.pack specialCharacters)))), (1, arbitrary)])

genText :: Gen Text
genText = T.pack <$> listOf (frequency [(1, elements specialCharacters), (1, arbitrary)])

specialCharacters :: String
specialCharacters = "\0\b\t\n\r\SUB\"%'\\_"
