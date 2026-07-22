{-# LANGUAGE TypeApplications #-}

-- | Roundtrip law for every 'Field' instance
-- (@fromField (toField x) == Right x@), plus the decode-failure behaviour
-- that a roundtrip cannot exercise.
module FieldRoundtrip (tests) where

import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as LazyByteString
import           Data.Int                           (Int16, Int32, Int64, Int8)
import           Data.Scientific                    (Scientific)
import           Data.Text                          (Text)
import qualified Data.Text.Lazy                     as LazyText
import           Data.Time.Calendar                 (Day)
import           Data.Time.LocalTime                (LocalTime,
                                                     TimeOfDay (..))
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)
import           Database.MySQL.Field
import           Database.MySQL.Protocol.MySQLValue (MySQLValue (..))
import           Test.QuickCheck.Instances          ()
import           Test.Tasty
import           Test.Tasty.HUnit                   (testCase, (@?=))
import           Test.Tasty.QuickCheck              (Property, testProperty,
                                                     (===))

roundtrips :: (Field a, Eq a, Show a) => a -> Property
roundtrips value = fromField (toField value) === Right value

tests :: TestTree
tests = testGroup "Field"
    [ testGroup "roundtrip"
        [ testProperty "Int8" (roundtrips @Int8)
        , testProperty "Word8" (roundtrips @Word8)
        , testProperty "Int16" (roundtrips @Int16)
        , testProperty "Word16" (roundtrips @Word16)
        , testProperty "Int32" (roundtrips @Int32)
        , testProperty "Word32" (roundtrips @Word32)
        , testProperty "Int64" (roundtrips @Int64)
        , testProperty "Word64" (roundtrips @Word64)
        , testProperty "Float" (roundtrips @Float)
        , testProperty "Double" (roundtrips @Double)
        , testProperty "Scientific" (roundtrips @Scientific)
        , testProperty "Text" (roundtrips @Text)
        , testProperty "lazy Text" (roundtrips @LazyText.Text)
        , testProperty "String" (roundtrips @String)
        , testProperty "ByteString" (roundtrips @ByteString)
        , testProperty "lazy ByteString" (roundtrips @LazyByteString.ByteString)
        , testProperty "Day" (roundtrips @Day)
        , testProperty "LocalTime" (roundtrips @LocalTime)
        , testProperty "TimeOfDay" (roundtrips @TimeOfDay)
        , testProperty "Bool" (roundtrips @Bool)
        , testProperty "Maybe Int32" (roundtrips @(Maybe Int32))
        , testProperty "Maybe Text" (roundtrips @(Maybe Text))
        ]
    , testGroup "decode failures"
        [ testCase "NULL into a non-Maybe type is DecodeUnexpectedNull" $
            decodeText MySQLNull
                @?= Left (DecodeUnexpectedNull (ExpectedTypeName "Text"))
        , testCase "wrong constructor is DecodeTypeMismatch" $
            decodeInt8 (MySQLText "hi")
                @?= Left (DecodeTypeMismatch (ExpectedTypeName "Int8")
                                             (MySQLText "hi"))
        , testCase "no implicit widening: INT32 value into Int64 fails" $
            decodeInt64 (MySQLInt32 42)
                @?= Left (DecodeTypeMismatch (ExpectedTypeName "Int64")
                                             (MySQLInt32 42))
        , testCase "negative TIME into TimeOfDay fails" $
            decodeTimeOfDay (MySQLTime 1 (TimeOfDay 1 2 3))
                @?= Left (DecodeTypeMismatch (ExpectedTypeName "TimeOfDay")
                                             (MySQLTime 1 (TimeOfDay 1 2 3)))
        ]
    , testGroup "decode alternatives"
        [ testCase "NULL into Maybe is Nothing" $
            decodeMaybe @Int32 MySQLNull @?= Right Nothing
        , testCase "TIMESTAMP decodes into LocalTime" $
            decodeLocalTime (MySQLTimeStamp (read "2026-07-21 12:00:00"))
                @?= Right (read "2026-07-21 12:00:00")
        , testCase "nonzero TINYINT decodes as True" $
            decodeBool (MySQLInt8 5) @?= Right True
        , testCase "unsigned TINYINT decodes into Bool" $
            decodeBool (MySQLInt8U 0) @?= Right False
        , testCase "nested Maybe collapses Just Nothing to Nothing (documented NULL lossiness)" $
            fromField @(Maybe (Maybe Int32)) (toField (Just (Nothing :: Maybe Int32)))
                @?= Right Nothing
        , testCase "NaN encodes losslessly (roundtrip == fails only since NaN /= NaN)" $
            fmap isNaN (decodeDouble (encodeDouble (0 / 0))) @?= Right True
        , testCase "lone surrogate in String is rejected by encodeString" $
            encodeString ['\xD800'] @?= Left (EncodeStringLoneSurrogate ['\xD800'])
        ]
    ]
