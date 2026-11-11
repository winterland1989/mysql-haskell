-- | Results in a charset other than UTF-8 or latin1, here gbk after
-- @SET NAMES gbk@. 'query_' hands the rows over and a string whose bytes are
-- not UTF-8 raises 'InvalidUtf8Text' when used; the typed decoders refuse the
-- row while reading it. 1.4.0 refused the row in 'query_' too.
module NonUtf8Results (tests) where

import           Control.Exception      (evaluate, try)
import           Control.Monad          (void)
import           Data.Word              (Word16)
import           Database.MySQL.Base
import qualified Database.MySQL.Decoder as Decode
import qualified System.IO.Streams      as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser               (withTestConnection)

tests :: TestTree
tests = testGroup "results in gbk"
    [ testCase "query_ reads the row; the string that is not UTF-8 raises when used" $
        withTestConnection $ \c -> do
            void (execute_ c "SET NAMES gbk")
            (_, rows) <- query_ c gbkQuery
            values <- Stream.toList rows
            case values of
                [[ok, nihao]] -> do
                    ok @?= MySQLText "ok"
                    used <- try (evaluate nihao)
                    used @?= Left (InvalidUtf8Text gbkChineseCi "\xC4\xE3")
                other -> assertFailure ("expected one row of two values, got " ++ show (length other))
    , testCase "queryRows_ with Decode.text refuses the row while reading it" $
        withTestConnection $ \c -> do
            void (execute_ c "SET NAMES gbk")
            (_, rows) <- queryRows_ ((,) <$> Decode.field Decode.text <*> Decode.field Decode.text) c gbkQuery
            refused <- try (Stream.toList rows)
            case refused of
                Left (Decode.FieldError (ColumnNumber 1) (Decode.FieldInvalidUtf8 bytes)) -> bytes @?= "\xC4\xE3"
                Left other -> assertFailure ("refused with " ++ show other)
                Right decoded -> assertFailure ("decoded " ++ show decoded)
    ]

-- | 你, which gbk sends as the two bytes C4 E3.
gbkQuery :: Query
gbkQuery = "SELECT 'ok', CONVERT(_utf8mb4 X'E4BDA0' USING gbk)"

-- | The id the server gives gbk results' default collation.
gbkChineseCi :: Word16
gbkChineseCi = 28
