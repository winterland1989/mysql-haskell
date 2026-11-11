{-# LANGUAGE TypeApplications #-}

-- | 'FieldRoundtrip' for the typed decoders of "Database.MySQL.Decoder": the
-- same types, the same decode rules. The roundtrip goes through the bytes a
-- prepared statement's row carries: a value 'toField' gives, encoded by the
-- old 'putBinaryField', must decode back to itself.
module DecoderRoundtrip (tests) where

import           Data.Binary.Put                    (runPut)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as LazyByteString
import           Data.Fixed                         (Fixed (MkFixed), Pico)
import           Data.Scientific                    (Scientific, scientific)
import qualified Data.Text                          as Text
import qualified Data.Text.Lazy                     as LazyText
import           Data.Time.Calendar                 (Day, fromGregorian)
import           Data.Time.LocalTime                (LocalTime (..),
                                                     TimeOfDay (..))
import qualified Data.Vector                        as V
import           Data.Word                          (Word16)
import qualified Database.MySQL.Decoder             as Decode
import           Database.MySQL.Field
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue (ColumnNumber (..),
                                                     MySQLValue (..),
                                                     putBinaryField)
import           Test.QuickCheck                    (Gen, Property, arbitrary,
                                                     choose, forAll, (===))
import           Test.QuickCheck.Instances          ()
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (testProperty)

tests :: TestTree
tests = testGroup "Decoder"
    [ testGroup "roundtrip through a prepared statement's row"
        [ testProperty "Int8" (roundtrips Decode.int8 (column mySQLTypeTiny signed binary))
        , testProperty "Word8" (roundtrips Decode.word8 (column mySQLTypeTiny unsigned binary))
        , testProperty "Int16" (roundtrips Decode.int16 (column mySQLTypeShort signed binary))
        , testProperty "Word16" (roundtrips Decode.word16 (column mySQLTypeShort unsigned binary))
        , testProperty "Int32" (roundtrips Decode.int32 (column mySQLTypeLong signed binary))
        , testProperty "Word32" (roundtrips Decode.word32 (column mySQLTypeLong unsigned binary))
        , testProperty "Int64" (roundtrips Decode.int64 (column mySQLTypeLongLong signed binary))
        , testProperty "Word64" (roundtrips Decode.word64 (column mySQLTypeLongLong unsigned binary))
        , testProperty "Float" (roundtrips Decode.float (column mySQLTypeFloat signed binary))
        , testProperty "Double" (roundtrips Decode.double (column mySQLTypeDouble signed binary))
        , testProperty "Scientific" $ forAll genDecimal $
            roundtrips Decode.scientific (column mySQLTypeNewDecimal signed binary)
        , testProperty "Text" (roundtrips Decode.text textColumn)
        , testProperty "lazy Text" (roundtrips (LazyText.fromStrict <$> Decode.text) textColumn)
        , testProperty "String" (roundtrips (Text.unpack <$> Decode.text) textColumn)
        , testProperty "ByteString" (roundtrips Decode.bytes bytesColumn)
        , testProperty "lazy ByteString"
            (roundtrips (LazyByteString.fromStrict <$> Decode.bytes) bytesColumn)
        , testProperty "Day" $ forAll genDay $
            roundtrips Decode.day (column mySQLTypeDate signed binary)
        , testProperty "LocalTime" $ forAll genLocalTime $
            roundtrips Decode.localTime (column mySQLTypeDateTime signed binary)
        , testProperty "TimeOfDay" $ forAll genTimeOfDay $
            roundtrips Decode.timeOfDay (column mySQLTypeTime signed binary)
        , testProperty "Bool" (roundtrips Decode.bool (column mySQLTypeTiny signed binary))
        , testProperty "Maybe Int32"
            (roundtrips (Decode.nullable Decode.int32) (column mySQLTypeLong signed binary))
        , testProperty "Maybe Text" (roundtrips (Decode.nullable Decode.text) textColumn)
        ]
    , testGroup "decode failures"
        [ testCase "NULL into a non-Maybe decoder is FieldUnexpectedNull" $
            decodeBinary Decode.text textColumn MySQLNull
                @?= Left (show (Decode.FieldError (ColumnNumber 0) Decode.FieldUnexpectedNull))
        , testCase "a column of another type is ColumnTypeMismatch" $
            decodeBinary Decode.int8 textColumn (MySQLText "hi")
                @?= Left (show (Decode.ColumnTypeMismatch (ColumnNumber 0) "" mySQLTypeVarString "Int8"))
        , testCase "no implicit widening: an INT column into int64 fails" $
            decodeBinary Decode.int64 (column mySQLTypeLong signed binary) (MySQLInt32 42)
                @?= Left (show (Decode.ColumnTypeMismatch (ColumnNumber 0) "" mySQLTypeLong "Int64"))
        , testCase "negative TIME into timeOfDay fails" $
            decodeBinary Decode.timeOfDay (column mySQLTypeTime signed binary)
                (MySQLTime 1 (TimeOfDay 1 2 3))
                @?= Left (show (Decode.FieldError (ColumnNumber 0)
                                    (Decode.FieldNegativeTime
                                        (B.drop 1 (encodedField (MySQLTime 1 (TimeOfDay 1 2 3)))))))
        ]
    , testGroup "decode alternatives"
        [ testCase "NULL into nullable is Nothing" $
            decodeBinary (Decode.nullable Decode.int32) (column mySQLTypeLong signed binary) MySQLNull
                @?= Right Nothing
        , testCase "TIMESTAMP decodes into LocalTime" $
            decodeBinary Decode.localTime (column mySQLTypeTimestamp signed binary)
                (MySQLTimeStamp (read "2026-07-21 12:00:00"))
                @?= Right (read "2026-07-21 12:00:00")
        , testCase "nonzero TINYINT decodes as True" $
            decodeBinary Decode.bool (column mySQLTypeTiny signed binary) (MySQLInt8 5)
                @?= Right True
        , testCase "unsigned TINYINT decodes into Bool" $
            decodeBinary Decode.bool (column mySQLTypeTiny unsigned binary) (MySQLInt8U 0)
                @?= Right False
        , testCase "nested nullable collapses a NULL to the outer Nothing (as Field does)" $
            decodeBinary (Decode.nullable (Decode.nullable Decode.int32))
                (column mySQLTypeLong signed binary) MySQLNull
                @?= Right Nothing
        , testCase "NaN decodes losslessly" $
            fmap isNaN (decodeBinary Decode.double (column mySQLTypeDouble signed binary)
                            (MySQLDouble (0 / 0)))
                @?= Right True
        ]
    ]

-- | 'toField', encoded as a prepared statement's row and decoded, gives the
-- value back.
roundtrips :: (Field a, Eq a, Show a) => Decode.FieldDecoder a -> ColumnDef -> a -> Property
roundtrips decoder definition value =
    decodeBinary decoder definition (toField value) === Right value

-- | A one-column binary-protocol row holding the value as 'putBinaryField'
-- encodes it, decoded with the decoder; errors are shown, so that column
-- mismatches and field errors compare in one type.
decodeBinary :: Decode.FieldDecoder a -> ColumnDef -> MySQLValue -> Either String a
decodeBinary decoder definition value =
    case Decode.prepareRowDecoder @Decode.BinaryProtocol (Decode.field decoder) (V.singleton definition) of
        Left mismatch  -> Left (show mismatch)
        Right prepared -> either (Left . show) Right (Decode.runPreparedRow prepared (binaryRow value))

-- | The 0x00 header and the NULL map (column 0 is bit 2), then the field.
binaryRow :: MySQLValue -> ByteString
binaryRow value = case value of
    MySQLNull -> B.pack [0x00, 0x04]
    _         -> B.concat [B.pack [0x00, 0x00], encodedField value]

-- | The value as 'putBinaryField' writes it; for the length-prefixed types
-- that includes the length byte, which a decoder's field bytes do not.
encodedField :: MySQLValue -> ByteString
encodedField value = LazyByteString.toStrict (runPut (putBinaryField value))

column :: FieldType -> Word16 -> Word16 -> ColumnDef
column fieldType flags charSet = ColumnDef
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

signed, unsigned :: Word16
signed = 0
unsigned = 32

binary, utf8 :: Word16
binary = 63
utf8 = 33

textColumn, bytesColumn :: ColumnDef
textColumn = column mySQLTypeVarString signed utf8
bytesColumn = column mySQLTypeBlob signed binary

-- | DECIMALs as MySQL holds them: a coefficient and a modest exponent, which
-- 'putBinaryField' writes out in full.
genDecimal :: Gen Scientific
genDecimal = scientific <$> arbitrary <*> choose (-30, 30)

-- | Days the 2-byte year of the binary protocol carries.
genDay :: Gen Day
genDay = fromGregorian <$> choose (0, 9999) <*> choose (1, 12) <*> choose (1, 28)

-- | DATETIMEs to the microsecond, the binary protocol's resolution.
genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> (TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> genSeconds)

-- | TIMEs up to MySQL's 838 hours, to the microsecond.
genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = TimeOfDay <$> choose (0, 838) <*> choose (0, 59) <*> genSeconds

genSeconds :: Gen Pico
genSeconds = MkFixed <$> ((* 1000000) <$> choose (0, 59999999))
