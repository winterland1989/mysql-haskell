-- | 'decodeTextRow', which the query functions decode text-protocol rows with,
-- must give the values 'getTextRow' gives, and say where a row is malformed.
module TextRowDecoding (tests) where

import           Data.Binary.Parser                 (parseOnly)
import           Data.Binary.Put                    (Put, putByteString,
                                                     putWord8, runPut)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lazy               as L
import qualified Data.Text.Encoding                 as T
import           Data.Time.Calendar                 (Day, fromGregorian)
import           Data.Time.LocalTime                (LocalTime, TimeOfDay)
import           Data.Word                          (Word16)
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet     (putLenEncInt)
import           Test.QuickCheck
import           Test.QuickCheck.Instances          ()
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (testProperty)

tests :: TestTree
tests = testGroup "text rows"
    [ testProperty "decodeTextRow gives the values getTextRow gives" $ checkCoverage $
        forAll genRow $ \(columns, fields) ->
            forAll (choose (0, B.length (encodeRow fields))) $ \kept ->
                cover 25 (decodes columns (encodeRow fields)) "the whole row decodes" $
                cover 25 (not (decodes columns (B.take kept (encodeRow fields))))
                    "the cut row fails" $
                conjoin
                    [ sameValues columns (encodeRow fields)
                    , sameValues columns (B.take kept (encodeRow fields))
                    ]
    , testCase "an employees row" $
        decodeTextRow (map textColumn employeeColumns) employeeRow
            @?= Right
                [ MySQLInt32 10001
                , MySQLDate (fromGregorian 1953 9 2)
                , MySQLText "Georgi"
                , MySQLText "Facello"
                , MySQLText "M"
                , MySQLDate (fromGregorian 1986 6 26)
                ]
    , testCase "NULL fields" $
        decodeTextRow (map textColumn (take 2 employeeColumns)) (B.pack [0xFB, 0xFB])
            @?= Right [MySQLNull, MySQLNull]
    , testCase "a row cut inside a field names that column and where it starts" $
        decodeTextRow (map textColumn employeeColumns) (B.take 20 employeeRow)
            @?= Left (TextRowError 2 17 TextRowEndsEarly)
    , testCase "a field that does not lex names its column" $
        decodeTextRow (map textColumn employeeColumns) (encodeRow [Just "1", Just "abc"])
            @?= Left (TextRowError 1 2
                        (TextRowField (TextFieldUnparsable mySQLTypeDate "abc")))
    ]

-- | Both decoders succeed with the same values, or both fail.
sameValues :: [ColumnDef] -> ByteString -> Property
sameValues columns row =
    rowValues (decodeTextRow (map textColumn columns) row)
        === rowValues (parseOnly (getTextRow columns) row)

decodes :: [ColumnDef] -> ByteString -> Bool
decodes columns row = either (const False) (const True) (decodeTextRow (map textColumn columns) row)

rowValues :: Either rowError [MySQLValue] -> Maybe [MySQLValue]
rowValues = either (const Nothing) Just

-- | @emp_no INT, birth_date DATE, first_name VARCHAR, last_name VARCHAR,
-- gender ENUM, hire_date DATE@, as in the select benchmark.
employeeColumns :: [ColumnDef]
employeeColumns =
    [ columnOf mySQLTypeLong 0 utf8
    , columnOf mySQLTypeDate 0 binary
    , columnOf mySQLTypeVarString 0 utf8
    , columnOf mySQLTypeVarString 0 utf8
    , columnOf mySQLTypeString 0 utf8
    , columnOf mySQLTypeDate 0 binary
    ]

employeeRow :: ByteString
employeeRow = encodeRow
    [Just "10001", Just "1953-09-02", Just "Georgi", Just "Facello", Just "M", Just "1986-06-26"]

utf8, binary :: Word16
utf8 = 33
binary = 63

unsigned :: Word16
unsigned = 32

columnOf :: FieldType -> Word16 -> Word16 -> ColumnDef
columnOf fieldType flags charSet = ColumnDef
    { columnDB        = ""
    , columnTable     = ""
    , columnOrigTable = ""
    , columnName      = ""
    , columnOrigName  = ""
    , columnCharSet   = charSet
    , columnLength    = 0
    , columnType      = fieldType
    , columnFlags     = flags
    , columnDecimals  = 0
    }

-- | A row's fields as the server sends them; 'Nothing' is the NULL marker.
encodeRow :: [Maybe ByteString] -> ByteString
encodeRow fields = L.toStrict (runPut (mapM_ putRowField fields))

putRowField :: Maybe ByteString -> Put
putRowField field = case field of
    Nothing    -> putWord8 0xFB
    Just bytes -> putLenEncInt (B.length bytes) >> putByteString bytes

genRow :: Gen ([ColumnDef], [Maybe ByteString])
genRow = do
    columnCount <- choose (0, 8)
    columns <- vectorOf columnCount genColumn
    fields <- mapM (genField . textColumn) columns
    pure (columns, fields)

-- | Every column type the text decoder knows, and JSON (0xf5), which it does not.
genColumn :: Gen ColumnDef
genColumn = columnOf
    <$> elements
        [ mySQLTypeDecimal, mySQLTypeNewDecimal, mySQLTypeTiny, mySQLTypeShort
        , mySQLTypeLong, mySQLTypeInt24, mySQLTypeLongLong, mySQLTypeFloat
        , mySQLTypeDouble, mySQLTypeNull, mySQLTypeYear, mySQLTypeTimestamp
        , mySQLTypeTimestamp2, mySQLTypeDateTime, mySQLTypeDateTime2
        , mySQLTypeDate, mySQLTypeNewDate, mySQLTypeTime, mySQLTypeTime2
        , mySQLTypeGeometry, mySQLTypeVarChar, mySQLTypeEnum, mySQLTypeSet
        , mySQLTypeTinyBlob, mySQLTypeMediumBlob, mySQLTypeLongBlob
        , mySQLTypeBlob, mySQLTypeVarString, mySQLTypeString, mySQLTypeBit
        , FieldType 0xf5
        ]
    <*> elements [0, unsigned]
    <*> elements [utf8, binary]

-- | A column of type NULL always holds the NULL marker.
genField :: TextColumn -> Gen (Maybe ByteString)
genField column = case column of
    TextColumnNull          -> pure Nothing
    TextColumnValue _ value -> frequency [(1, pure Nothing), (6, Just <$> genValueBytes value)]

-- | Mostly what the server sends for the column, sometimes bytes it would not.
-- Text in a non-binary character set stays valid UTF-8: 'getTextRow' decodes it
-- lazily, so invalid bytes would throw while the values are compared.
genValueBytes :: TextValue -> Gen ByteString
genValueBytes value = case value of
    TextDecimal     -> renderedOrArbitrary (arbitrary :: Gen Double)
    TextInt8U       -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt8        -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt16U      -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt16       -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt32U      -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt32       -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt64U      -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextInt64       -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextFloat       -> renderedOrArbitrary (arbitrary :: Gen Double)
    TextDouble      -> renderedOrArbitrary (arbitrary :: Gen Double)
    TextYear        -> renderedOrArbitrary (arbitrary :: Gen Integer)
    TextTimeStamp   -> renderedOrArbitrary (arbitrary :: Gen LocalTime)
    TextDateTime    -> renderedOrArbitrary (arbitrary :: Gen LocalTime)
    TextDate        -> renderedOrArbitrary (arbitrary :: Gen Day)
    TextTime        -> oneof [genTime, renderedOrArbitrary (arbitrary :: Gen TimeOfDay)]
    TextGeometry    -> arbitrary
    TextUtf8        -> T.encodeUtf8 <$> arbitrary
    TextBytes       -> frequency [(9, arbitrary), (1, genLongBytes)]
    TextBit         -> B.pack <$> (choose (1, 10) >>= vector)
    TextUnsupported -> arbitrary

renderedOrArbitrary :: Show a => Gen a -> Gen ByteString
renderedOrArbitrary gen = frequency [(4, BC.pack . show <$> gen), (1, arbitrary)]

-- | A TIME up to MySQL's 838 hours, sometimes negative.
genTime :: Gen ByteString
genTime = do
    sign <- elements ["", "-"]
    hours <- choose (0, 838 :: Int)
    minutes <- choose (0, 59 :: Int)
    seconds <- choose (0, 59 :: Int)
    pure (BC.pack (sign ++ show hours ++ ":" ++ show minutes ++ ":" ++ show seconds))

-- | Long enough for the 2- and 3-byte length prefixes.
genLongBytes :: Gen ByteString
genLongBytes = B.replicate <$> choose (251, 70000) <*> arbitrary
