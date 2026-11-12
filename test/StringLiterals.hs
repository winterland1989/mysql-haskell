-- | Text and bytes rendered into the query text as single-quoted string
-- literals: the escaped form of every special character, and the literal read
-- back the way the server's lexer reads it, with and without
-- NO_BACKSLASH_ESCAPES.
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
import           Database.MySQL.Protocol.Escape     (escapeBytes, escapeText)
import           Test.QuickCheck                    (Gen, arbitrary, elements,
                                                     forAll, frequency, listOf)
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (testProperty, (===))

tests :: TestTree
tests = testGroup "string literals"
    [ testGroup "1.4.1's escapes with the quote doubled"
        [ testGroup "escapeText" (map escapeTextGolden escapeGoldens)
        , testGroup "escapeBytes" $
            testCase "every byte" (escapeBytes everyByte @?= everyByteEscaped)
                : map escapeBytesGolden escapeGoldens
        ]
    , testCase "a parameter is rendered escaped between single quotes" $
        rendered (MySQLText "it's\n\"C:\\\"") @?= "'it''s\\n\\\"C:\\\\\\\"'"
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
        , testProperty "bytes without backslash-escaped characters read back exactly" $
            forAll (B.filter (`notElem` backslashEscapedBytes) <$> genBytes) $ \bytes ->
                readLiteral NoBackslashEscapes (rendered (MySQLBytes bytes)) === Just bytes
        , testProperty "text without backslash-escaped characters reads back exactly" $
            forAll (T.filter (`notElem` backslashEscaped) <$> genText) $ \text ->
                readLiteral NoBackslashEscapes (rendered (MySQLText text)) === Just (T.encodeUtf8 text)
        ]
    ]

escapeTextGolden :: (Text, Text) -> TestTree
escapeTextGolden (input, escaped) = testCase (show input) (escapeText input @?= escaped)

escapeBytesGolden :: (Text, Text) -> TestTree
escapeBytesGolden (input, escaped) =
    testCase (show input) (escapeBytes (T.encodeUtf8 input) @?= T.encodeUtf8 escaped)

-- | Inputs holding every character the escaping treats specially, with their
-- escaped forms. The forms are not written by hand: a scratch program built
-- against a worktree of 23f7366 (1.4.1) printed what escapeText gave, with
-- each @\\'@ rewritten to @''@. It also checked that 1.4.1's escapeBytes gave
-- the UTF-8 of the same forms.
escapeGoldens :: [(Text, Text)]
escapeGoldens =
    [ ("", "")
    , ("plain text, 100% ok_", "plain text, 100% ok_")
    , ("it's", "it''s")
    , ("say \"hi\"", "say \\\"hi\\\"")
    , ("C:\\dir\\", "C:\\\\dir\\\\")
    , ("nul\NULbyte", "nul\\\NULbyte")
    , ("back\bspace", "back\\bspace")
    , ("tab\there", "tab\\there")
    , ("new\nline", "new\\nline")
    , ("carriage\rreturn", "carriage\\rreturn")
    , ("ctrl\SUBz", "ctrl\\Zz")
    , ("\\'", "\\\\''")
    , ("''", "''''")
    , ("\252nic\246de \128512 \10003", "\252nic\246de \128512 \10003")
    , ("\NUL\b\t\n\r\SUB\"'\\%_", "\\\NUL\\b\\t\\n\\r\\Z\\\"''\\\\%_")
    ]

everyByte :: ByteString
everyByte = B.pack [minBound .. maxBound]

-- | 'everyByte' escaped, printed by the same program from 1.4.1's
-- escapeBytes, with its @\\'@ rewritten to @''@.
everyByteEscaped :: ByteString
everyByteEscaped =
    "\\\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\\b\\t\\n\v\f\\r\SO\SI\DLE\DC1\DC2\DC3\
    \\DC4\NAK\SYN\ETB\CAN\EM\\Z\ESC\FS\GS\RS\US !\\\"#$%&''()*+,-./0123456789\
    \:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\
    \\DEL\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\
    \\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\
    \\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\
    \\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\
    \\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\
    \\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\
    \\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\
    \\253\254\255"

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

-- | The characters the escaping writes after a backslash, which
-- NO_BACKSLASH_ESCAPES stores along with it.
backslashEscaped :: String
backslashEscaped = "\0\b\t\n\r\SUB\"\\"

backslashEscapedBytes :: [Word8]
backslashEscapedBytes = B.unpack (T.encodeUtf8 (T.pack backslashEscaped))
