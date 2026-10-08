-- | The decoders of "Database.MySQL.Decoder" against the paths they bypass:
-- 'mysqlValue' must give the values 'decodeTextRow' and 'getBinaryRow' give,
-- and each typed decoder the value "Database.MySQL.Field" gets from the same
-- field through 'MySQLValue', in both protocols.
module DirectDecoding (tests) where

import           Data.Binary.Parser                 (parseOnly)
import           Data.Binary.Put                    (putByteString, putDoublele,
                                                     putFloatle, runPut)
import           Data.Bits                          (setBit)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lazy               as L
import           Data.Int                           (Int32)
import           Data.Maybe                         (fromMaybe, isJust, isNothing)
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import           Data.Time.Calendar                 (Day, fromGregorian)
import qualified Data.Vector                        as V
import           Data.Word                          (Word8)
import qualified Database.MySQL.Decoder             as Decode
import           Database.MySQL.Field
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet     (putLenEncInt)
import           Test.QuickCheck
import           Test.QuickCheck.Instances          ()
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (testProperty)
import           TextRowDecoding                    (binary, columnOf,
                                                     employeeColumns,
                                                     employeeRow, encodeRow,
                                                     genColumn, genField, genRow,
                                                     utf8)

tests :: TestTree
tests = testGroup "direct decoding"
    [ testProperty "text rows: mysqlValue gives what decodeTextRow gives" $
        forAll genRow $ \(columns, fields) ->
            forAll (choose (0, B.length (encodeRow fields))) $ \kept ->
                conjoin
                    [ textMySQLValues columns (encodeRow fields)
                        === shownRow (decodeTextRow (map columnKind columns) (encodeRow fields))
                    , textMySQLValues columns (B.take kept (encodeRow fields))
                        === shownRow (decodeTextRow (map columnKind columns) (B.take kept (encodeRow fields)))
                    ]
    , testProperty "binary rows: mysqlValue gives what getBinaryRow gives" $ checkCoverage $
        forAll genBinaryRow $ \(columns, row) ->
            forAll (choose (0, B.length row)) $ \kept ->
                cover 25 (isJust (getBinaryRowValues columns row)) "the whole row decodes" $
                conjoin
                    [ binaryMySQLValues columns row === getBinaryRowValues columns row
                    , binaryMySQLValues columns (B.take kept row)
                        === getBinaryRowValues columns (B.take kept row)
                    ]
    , testProperty "text rows: by column number gives what the row decoder gives" $ checkCoverage $
        forAll genRow $ \(columns, fields) ->
            forAll (choose (0, B.length (encodeRow fields))) $ \kept ->
                cover 25 (isJust (textMySQLValues columns (encodeRow fields))) "the whole row decodes" $
                conjoin
                    [ rawRowValues columns (Decode.textRawRow (length columns) (encodeRow fields))
                        === textMySQLValues columns (encodeRow fields)
                    , rawRowValues columns (Decode.textRawRow (length columns) (B.take kept (encodeRow fields)))
                        === textMySQLValues columns (B.take kept (encodeRow fields))
                    ]
    , testProperty "binary rows: by column number gives what the row decoder gives" $ checkCoverage $
        forAll genBinaryRow $ \(columns, row) ->
            forAll (choose (0, B.length row)) $ \kept ->
                cover 25 (isJust (binaryMySQLValues columns row)) "the whole row decodes" $
                conjoin
                    [ rawRowValues columns (binaryRawRowOf columns row) === binaryMySQLValues columns row
                    , rawRowValues columns (binaryRawRowOf columns (B.take kept row))
                        === binaryMySQLValues columns (B.take kept row)
                    ]
    , testProperty "text fields: a typed decoder gives what Field gives" $ checkCoverage $
        forAll genColumn $ \column -> forAll (genField (columnKind column)) $ \fieldBytes ->
            case comparisonFor column of
                Nothing -> property True
                Just (Comparison decoder viaField) ->
                    cover 40 (isJust (viaFieldText viaField column (encodeRow [fieldBytes])))
                        "the field decodes" $
                    typedText decoder column (encodeRow [fieldBytes])
                        === viaFieldText viaField column (encodeRow [fieldBytes])
    , testProperty "binary fields: a typed decoder gives what Field gives" $ checkCoverage $
        forAll genColumn $ \column -> forAll (genBinaryField column) $ \fieldBytes ->
            case comparisonFor column of
                Nothing -> property True
                Just (Comparison decoder viaField) ->
                    cover 40 (isJust (viaFieldBinary viaField column (encodeBinaryRow [fieldBytes])))
                        "the field decodes" $
                    typedBinary decoder column (encodeBinaryRow [fieldBytes])
                        === viaFieldBinary viaField column (encodeBinaryRow [fieldBytes])
    , testCase "a raw row knows where each field is" $
        fmap (\row -> map (Decode.rawField row) [0, 1, 6]) (Decode.textRawRow 6 employeeRow)
            @?= Right [Decode.RawBytes "10001", Decode.RawBytes "1953-09-02", Decode.RawAbsent]
    , testCase "a record decoder reads the employees row" $
        decodeTextWith employeeDecoder employeeColumns employeeRow
            @?= Decoded (10001, fromGregorian 1953 9 2, "Georgi", "Facello", "M", fromGregorian 1986 6 26)
    , testCase "a decoder for fewer columns than the result set has" $
        decodeTextWith (Decode.field Decode.int32) employeeColumns employeeRow
            @?= Mismatch (Decode.ColumnCountMismatch 1 6)
    , testCase "a decoder that does not take the column's type" $
        decodeTextWith (Decode.field Decode.text) [columnOf mySQLTypeLong 0 binary] (encodeRow [Just "1"])
            @?= Mismatch (Decode.ColumnTypeMismatch 0 "" mySQLTypeLong "Text")
    , testCase "NULL is Nothing for a nullable decoder" $
        decodeTextWith (Decode.field (Decode.nullable Decode.int32)) [columnOf mySQLTypeLong 0 binary]
            (encodeRow [Nothing])
            @?= Decoded Nothing
    , testCase "NULL is an error for any other decoder" $
        decodeTextWith (Decode.field Decode.int32) [columnOf mySQLTypeLong 0 binary] (encodeRow [Nothing])
            @?= FieldFailed (Decode.FieldError 0 Decode.FieldUnexpectedNull)
    , testCase "a negative TIME is not a TimeOfDay" $
        decodeTextWith (Decode.field Decode.timeOfDay) [columnOf mySQLTypeTime 0 binary]
            (encodeRow [Just "-01:00:00"])
            @?= FieldFailed (Decode.FieldError 0 (Decode.FieldNegativeTime "-01:00:00"))
    , testCase "invalid UTF-8 is an error, not an exception" $
        decodeTextWith (Decode.field Decode.text) [columnOf mySQLTypeVarString 0 utf8]
            (encodeRow [Just (B.pack [0xff])])
            @?= FieldFailed (Decode.FieldError 0 (Decode.FieldInvalidUtf8 (B.pack [0xff])))
    , testCase "the binary protocol's signed integers are two's complement" $
        decodeBinaryWith (Decode.field Decode.int32) [columnOf mySQLTypeLong 0 binary]
            (encodeBinaryRow [Just (B.pack [0xfe, 0xff, 0xff, 0xff])])
            @?= Decoded (-2)
    ]

employeeDecoder :: Decode.RowDecoder (Int32, Day, Text, Text, Text, Day)
employeeDecoder = (,,,,,)
    <$> Decode.field Decode.int32
    <*> Decode.field Decode.day
    <*> Decode.field Decode.text
    <*> Decode.field Decode.text
    <*> Decode.field Decode.text
    <*> Decode.field Decode.day

-- | What decoding one row came to.
data Outcome a
    = Mismatch Decode.ColumnMismatch
    | FieldFailed Decode.FieldError
    | Decoded a
    deriving (Show, Eq)

decodeTextWith :: Decode.RowDecoder a -> [ColumnDef] -> ByteString -> Outcome a
decodeTextWith decoder columns row = case Decode.prepareRowDecoder @Decode.TextProtocol decoder (V.fromList columns) of
    Left mismatch  -> Mismatch mismatch
    Right prepared -> either FieldFailed Decoded (Decode.runPreparedRow prepared row)

decodeBinaryWith :: Decode.RowDecoder a -> [ColumnDef] -> ByteString -> Outcome a
decodeBinaryWith decoder columns row = case Decode.prepareRowDecoder @Decode.BinaryProtocol decoder (V.fromList columns) of
    Left mismatch  -> Mismatch mismatch
    Right prepared -> either FieldFailed Decoded (Decode.runPreparedRow prepared row)

-- | Every column through 'Decode.mysqlValue'.
allValues :: [ColumnDef] -> Decode.RowDecoder [MySQLValue]
allValues = traverse (const (Decode.field Decode.mysqlValue))

-- | Every column through 'Decode.mysqlValue' on a 'Decode.RawRow', by column
-- number, as a library decoding by column number would.
rawRowValues :: Decode.RowProtocol protocol
             => [ColumnDef] -> Either RowError (Decode.RawRow protocol) -> Maybe String
rawRowValues columns rawRow = do
    raw <- either (const Nothing) Just rawRow
    parsers <- either (const Nothing) Just
        (traverse (uncurry (Decode.prepareFieldParser Decode.mysqlValue)) (zip [0 ..] columns))
    shownRow (traverse (rawFieldValue raw) (zip [0 ..] parsers))

rawFieldValue :: Decode.RawRow protocol -> (Int, Decode.FieldParser protocol MySQLValue)
              -> Either Decode.FieldError MySQLValue
rawFieldValue raw (column, parser) = Decode.runFieldParser parser raw column Left Right

-- | Values are compared shown, so that a NaN read from random bytes equals itself.
shownRow :: Either rowError [MySQLValue] -> Maybe String
shownRow = either (const Nothing) (Just . show)

binaryRawRowOf :: [ColumnDef] -> ByteString -> Either RowError (Decode.RawRow Decode.BinaryProtocol)
binaryRawRowOf columns = Decode.binaryRawRow (V.fromList (map Decode.binaryWidth columns))

textMySQLValues :: [ColumnDef] -> ByteString -> Maybe String
textMySQLValues columns row = shownOutcome (decodeTextWith (allValues columns) columns row)

binaryMySQLValues :: [ColumnDef] -> ByteString -> Maybe String
binaryMySQLValues columns row = shownOutcome (decodeBinaryWith (allValues columns) columns row)

getBinaryRowValues :: [ColumnDef] -> ByteString -> Maybe String
getBinaryRowValues columns row = shownRow (parseOnly (getBinaryRow columns (length columns)) row)

-- | A typed decoder and the "Database.MySQL.Field" conversion for the same type.
data Comparison = forall a. Show a => Comparison (Decode.FieldDecoder a) (MySQLValue -> Either DecodeError a)

-- | The comparison for a column's kind; YEAR, GEOMETRY and BIT have no typed
-- decoder, and a column of type NULL has no kind.
comparisonFor :: ColumnDef -> Maybe Comparison
comparisonFor column = case columnKind column of
    NullColumn -> Nothing
    ValueColumn _ kind -> case kind of
        KindDecimal     -> Just (Comparison Decode.scientific decodeScientific)
        KindInt8U       -> Just (Comparison Decode.word8 decodeWord8)
        KindInt8        -> Just (Comparison Decode.int8 decodeInt8)
        KindInt16U      -> Just (Comparison Decode.word16 decodeWord16)
        KindInt16       -> Just (Comparison Decode.int16 decodeInt16)
        KindInt32U      -> Just (Comparison Decode.word32 decodeWord32)
        KindInt32       -> Just (Comparison Decode.int32 decodeInt32)
        KindInt64U      -> Just (Comparison Decode.word64 decodeWord64)
        KindInt64       -> Just (Comparison Decode.int64 decodeInt64)
        KindFloat       -> Just (Comparison Decode.float decodeFloat')
        KindDouble      -> Just (Comparison Decode.double decodeDouble)
        KindYear        -> Nothing
        KindTimeStamp   -> Just (Comparison Decode.localTime decodeLocalTime)
        KindDateTime    -> Just (Comparison Decode.localTime decodeLocalTime)
        KindDate        -> Just (Comparison Decode.day decodeDay)
        KindTime        -> Just (Comparison Decode.timeOfDay decodeTimeOfDay)
        KindGeometry    -> Nothing
        KindText        -> Just (Comparison Decode.text decodeText)
        KindBytes       -> Just (Comparison Decode.bytes decodeByteString)
        KindBit         -> Nothing
        KindUnsupported -> Nothing

typedText :: Show a => Decode.FieldDecoder a -> ColumnDef -> ByteString -> Maybe String
typedText decoder column row = shownOutcome (decodeTextWith (Decode.field decoder) [column] row)

viaFieldText :: Show a => (MySQLValue -> Either DecodeError a) -> ColumnDef -> ByteString -> Maybe String
viaFieldText viaField column row = case decodeTextRow [columnKind column] row of
    Right [value] -> either (const Nothing) (Just . show) (viaField value)
    Right values  -> Just ("unexpected values: " ++ show values)
    Left _        -> Nothing

typedBinary :: Show a => Decode.FieldDecoder a -> ColumnDef -> ByteString -> Maybe String
typedBinary decoder column row = shownOutcome (decodeBinaryWith (Decode.field decoder) [column] row)

-- | The value shown, 'Nothing' for a row or field that did not decode; a column
-- mismatch is a failure of the test, so it shows up in the comparison.
shownOutcome :: Show a => Outcome a -> Maybe String
shownOutcome decoded = case decoded of
    Decoded value     -> Just (show value)
    FieldFailed _     -> Nothing
    Mismatch mismatch -> Just ("mismatch: " ++ show mismatch)

viaFieldBinary :: Show a => (MySQLValue -> Either DecodeError a) -> ColumnDef -> ByteString -> Maybe String
viaFieldBinary viaField column row = case parseOnly (getBinaryRow [column] 1) row of
    Right [value] -> either (const Nothing) (Just . show) (viaField value)
    Right values  -> Just ("unexpected values: " ++ show values)
    Left _        -> Nothing

genBinaryRow :: Gen ([ColumnDef], ByteString)
genBinaryRow = do
    columnCount <- choose (0, 8)
    columns <- vectorOf columnCount genColumn
    fields <- mapM genBinaryField columns
    pure (columns, encodeBinaryRow fields)

-- | A field as a prepared statement's row carries it: 'Nothing' for NULL, which
-- a column of type NULL always is, otherwise the bytes after the NULL map.
genBinaryField :: ColumnDef -> Gen (Maybe ByteString)
genBinaryField column = case columnKind column of
    NullColumn         -> pure Nothing
    ValueColumn _ kind -> frequency [(1, pure Nothing), (6, Just <$> genBinaryValue kind)]

-- | Mostly what the server sends, sometimes lengths it would not. Text stays
-- valid UTF-8, as 'getBinaryRow' decodes it lazily and invalid bytes would
-- throw while the values are compared.
genBinaryValue :: ValueKind -> Gen ByteString
genBinaryValue kind = case kind of
    KindDecimal     -> lengthEncoded . BC.pack . show <$> (arbitrary :: Gen Double)
    KindInt8U       -> genBytes 1
    KindInt8        -> genBytes 1
    KindInt16U      -> genBytes 2
    KindInt16       -> genBytes 2
    KindInt32U      -> genBytes 4
    KindInt32       -> genBytes 4
    KindInt64U      -> genBytes 8
    KindInt64       -> genBytes 8
    KindFloat       -> L.toStrict . runPut . putFloatle <$> arbitrary
    KindDouble      -> L.toStrict . runPut . putDoublele <$> arbitrary
    KindYear        -> genBytes 2
    KindTimeStamp   -> lengthEncoded <$> (elements [0, 4, 7, 11, 5] >>= genBytes)
    KindDateTime    -> lengthEncoded <$> (elements [0, 4, 7, 11, 5] >>= genBytes)
    KindDate        -> lengthEncoded <$> (elements [0, 4, 3] >>= genBytes)
    KindTime        -> lengthEncoded <$> (elements [0, 8, 12, 9] >>= genBytes)
    KindGeometry    -> lengthEncoded <$> arbitrary
    KindText        -> lengthEncoded . T.encodeUtf8 <$> arbitrary
    KindBytes       -> lengthEncoded <$> arbitrary
    KindBit         -> lengthEncoded <$> (choose (1, 10) >>= genBytes)
    KindUnsupported -> lengthEncoded <$> arbitrary

genBytes :: Int -> Gen ByteString
genBytes count = B.pack <$> vector count

lengthEncoded :: ByteString -> ByteString
lengthEncoded fieldBytes =
    L.toStrict (runPut (putLenEncInt (B.length fieldBytes) >> putByteString fieldBytes))

-- | The 0x00 header, the NULL map with column @i@ at bit @i + 2@, then the
-- non-NULL fields.
encodeBinaryRow :: [Maybe ByteString] -> ByteString
encodeBinaryRow fields = B.concat
    (B.singleton 0x00 : binaryNullMap fields : map (fromMaybe B.empty) fields)

binaryNullMap :: [Maybe ByteString] -> ByteString
binaryNullMap fields = B.pack
    (map (nullMapByte (nullBits fields)) [0 .. (length fields + 7 + 2) `div` 8 - 1])

nullBits :: [Maybe ByteString] -> [Int]
nullBits fields = map fst (filter (isNothing . snd) (zip [2 ..] fields))

nullMapByte :: [Int] -> Int -> Word8
nullMapByte bits byteIndex =
    foldr (flip setBit) 0 (map (subtract (8 * byteIndex)) (filter ((== byteIndex) . (`div` 8)) bits))
