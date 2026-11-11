-- | Dates a 'Day' cannot hold, such as 0000-00-00, which used to come back
-- clamped to a real date (0000-01-01) and were written back that way.
module ZeroDates (tests) where

import           Data.Binary.Parser             (parseOnly)
import           Data.Binary.Put                (runPut)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Time.Calendar             (Day, fromGregorianValid)
import           Data.Word                      (Word16, Word8)
import           Data.Time.LocalTime            (TimeOfDay (..))
import           Database.MySQL.Field
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet  (putLenEncBytes)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "zero dates"
    [ testCase "text DATE 0000-00-00" $
        assertEqual "value" (Right (MySQLDateRaw (RawDate 0 0 0)))
            (parseOnly (getTextField (column mySQLTypeDate)) (lenEnc "0000-00-00"))
    , testCase "text DATETIME with a zero day" $
        assertEqual "value" (Right (MySQLDateTimeRaw (RawDate 2024 5 0) (TimeOfDay 13 14 15)))
            (parseOnly (getTextField (column mySQLTypeDateTime)) (lenEnc "2024-05-00 13:14:15"))
    , testCase "binary TIMESTAMP of length 0" $
        assertEqual "value" (Right (MySQLTimeStampRaw (RawDate 0 0 0) (TimeOfDay 0 0 0)))
            (parseOnly (getBinaryField (column mySQLTypeTimestamp)) "\x00")
    , testCase "binary DATE past the end of its month" $
        assertEqual "value" (Right (MySQLDateRaw (RawDate 2024 2 30)))
            (parseOnly (getBinaryField (column mySQLTypeDate)) "\x04\xE8\x07\x02\x1E")
    , testCase "rendered back as the server stored it" $
        assertEqual "SQL" "'2024-05-00 13:14:15'"
            (runPut (putTextField (MySQLDateTimeRaw (RawDate 2024 5 0) (TimeOfDay 13 14 15))))
    , testCase "sent back in binary as the server stored it" $
        assertEqual "bytes" "\x04\x00\x00\x00\x00" (runPut (putBinaryField (MySQLDateRaw (RawDate 0 0 0))))
    , testCase "decoding one as a Day fails" $
        assertEqual "decoded"
            (Left (DecodeUnrepresentableDate (ExpectedTypeName "Day") (MySQLDateRaw (RawDate 0 0 0))))
            (fromField (MySQLDateRaw (RawDate 0 0 0)) :: Either DecodeError Day)
    , testCase "calendarDate agrees with fromGregorianValid on every month and day" $
        take 5 (filter disagrees calendarSample) @?= []
    ]

-- | Two full 400-year Gregorian cycles from year 0, the turns of centuries
-- around them, and the ends of 'Word16', each with every month 0 to 13 and
-- day 0 to 32, so that the fast path and its fallback both run.
calendarSample :: [RawDate]
calendarSample = do
    year <- [0 .. 800] ++ [1582, 1858, 1900, 1970, 2000, 2024, 2100, 9999, 65534, 65535 :: Word16]
    month <- [0 .. 13 :: Word8]
    day <- [0 .. 32 :: Word8]
    pure (RawDate year month day)

disagrees :: RawDate -> Bool
disagrees raw@(RawDate year month day) =
    calendarDate raw
        /= maybe (Left raw) Right (fromGregorianValid (toInteger year) (fromEnum month) (fromEnum day))

column :: FieldType -> ColumnDef
column fieldType = ColumnDef "" "" "" "" "" 63 0 fieldType 0 0

lenEnc :: B.ByteString -> B.ByteString
lenEnc value = L.toStrict (runPut (putLenEncBytes value))
