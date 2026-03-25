module BoundsCheck (tests) where

import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import           Data.Binary.Put             (runPut, putWord8)
import           Data.Binary.Parser          (parseOnly)
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit            hiding (Test)

import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue (getTextField, MySQLValue)
import           Database.MySQL.Protocol.Packet (putLenEncInt)
import           Database.MySQL.BinLogProtocol.BinLogEvent
                    (FormatDescription(..), eventHeaderLen, BinLogEventType(..))
import           Database.MySQL.BinLogProtocol.BinLogValue (getBinLogField)
import           Database.MySQL.BinLogProtocol.BinLogMeta (BinLogMeta(..))

tests :: TestTree
tests = testGroup "bounds-check"
    [ testGroup "text-protocol" textProtocolTests
    , testGroup "binlog-value" binlogValueTests
    , testGroup "binlog-event" binlogEventTests
    ]

--------------------------------------------------------------------------------
-- Helpers

-- | Build a ColumnDef with the given field type and default flags.
mkColumnDef :: FieldType -> ColumnDef
mkColumnDef ft = ColumnDef
    { columnDB        = ""
    , columnTable     = ""
    , columnOrigTable = ""
    , columnName      = ""
    , columnOrigName  = ""
    , columnCharSet   = 63 -- binary
    , columnLength    = 0
    , columnType      = ft
    , columnFlags     = 0
    , columnDecimals  = 0
    }

-- | Encode a length-prefixed ByteString as the MySQL wire protocol does,
-- then run the given Get parser on it.
parseLenEncField :: ColumnDef -> B.ByteString -> Either String MySQLValue
parseLenEncField cd payload =
    let encoded = L.toStrict . runPut $ do
            putLenEncInt (B.length payload)
            mapM_ putWord8 (B.unpack payload)
    in parseOnly (getTextField cd) encoded

-- | Check that parsing yields a Left (failure), not a crash or garbage.
assertParseFailure :: String -> Either String a -> Assertion
assertParseFailure label result = case result of
    Left _  -> pure ()
    Right _ -> assertFailure (label ++ ": expected parse failure but got a result")

--------------------------------------------------------------------------------
-- Text protocol tests

textProtocolTests :: [TestTree]
textProtocolTests =
    [ testCase "timestamp: short input (3 bytes)" $ do
        -- TIMESTAMP expects "YYYY-MM-DD HH:MM:SS" (19+ chars)
        -- 3 bytes is far too short; unsafeDrop 11 on 3 bytes is UB
        let cd = mkColumnDef mySQLTypeTimestamp
            result = parseLenEncField cd "abc"
        assertParseFailure "timestamp short" result

    , testCase "timestamp: empty input" $ do
        let cd = mkColumnDef mySQLTypeTimestamp
            result = parseLenEncField cd ""
        assertParseFailure "timestamp empty" result

    , testCase "datetime: short input (3 bytes)" $ do
        let cd = mkColumnDef mySQLTypeDateTime
            result = parseLenEncField cd "abc"
        assertParseFailure "datetime short" result

    , testCase "time: empty input" $ do
        -- TIME parser does unsafeIndex 0 on the payload; empty = UB
        let cd = mkColumnDef mySQLTypeTime
            result = parseLenEncField cd ""
        assertParseFailure "time empty" result

    , testCase "date: missing separator" $ do
        -- "2024" has no '-' separator; readDecimal consumes everything,
        -- then unsafeTail is called on an empty remainder
        let cd = mkColumnDef mySQLTypeDate
            result = parseLenEncField cd "2024"
        assertParseFailure "date missing sep" result

    , testCase "time: missing separator" $ do
        -- "12" has no ':' separator; readDecimal consumes it,
        -- then unsafeTail is called on empty remainder
        let cd = mkColumnDef mySQLTypeTime
            result = parseLenEncField cd "12"
        assertParseFailure "time missing sep" result
    ]

--------------------------------------------------------------------------------
-- BinLog value tests

binlogValueTests :: [TestTree]
binlogValueTests =
    [ testCase "NEWDECIMAL: precision < scale causes underflow" $ do
        -- precision=3, scale=10: (precision - scale) underflows Word8 to 249,
        -- producing a huge index into the 10-element sizeTable
        let meta = BINLOG_TYPE_NEWDECIMAL 3 10
            -- Provide some bytes so Get doesn't fail on "not enough input" first
            input = B.replicate 64 0x80
            result = parseOnly (getBinLogField meta) input
        assertParseFailure "NEWDECIMAL underflow" result
    ]

--------------------------------------------------------------------------------
-- BinLog event tests

binlogEventTests :: [TestTree]
binlogEventTests =
    [ testCase "eventHeaderLen: BINLOG_UNKNOWN_EVENT (negative index)" $ do
        -- fromEnum BINLOG_UNKNOWN_EVENT - 1 = -1, negative index into ByteString
        let fd = FormatDescription
                { fdVersion = 4
                , fdMySQLVersion = B.replicate 50 0
                , fdCreateTime = 0
                , fdEventHeaderLenVector = B.pack [19, 19, 19]  -- short vector
                }
            -- This should not crash; should return safe fallback
            result = eventHeaderLen fd BINLOG_UNKNOWN_EVENT
        -- After fix: returns 0 (safe fallback) instead of crashing
        assertEqual "unknown event should return 0" 0 result
    ]
