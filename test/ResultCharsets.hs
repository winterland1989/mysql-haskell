-- | String columns in the character set the server sent them in. latin1 bytes
-- used to go through a UTF-8 decoder that threw an impure exception. Bytes
-- that are not UTF-8 in any other charset raise 'InvalidUtf8Text' when the
-- value is used, not while the row is read (1.4.0 refused the row).
module ResultCharsets (tests) where

import           Control.Exception              (evaluate, try)
import           Data.Binary.Parser             (parseOnly)
import           Data.Binary.Put                (runPut)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           Data.Word                      (Word16)
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet  (putLenEncBytes)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "result character sets"
    [ testCase "latin1 (collation 8) is Windows-1252" $
        assertEqual "value" (Right (MySQLText "\x20AC\x00E9\x0081"))
            (parseOnly (getTextField (varchar latin1SwedishCi)) (lenEnc "\x80\xE9\x81"))
    , testCase "another charset decodes ASCII, and other bytes raise when the value is used" $ do
        assertEqual "ASCII value" (Right (MySQLText "abc"))
            (parseOnly (getBinaryField (varchar gbkChineseCi)) (lenEnc "abc"))
        text <- either (assertFailure . ("refused the field: " <>)) pure
            (parseOnly (getTextField (varchar gbkChineseCi)) (lenEnc "\xC4\xE3"))
        used <- try (evaluate text)
        used @?= Left (InvalidUtf8Text gbkChineseCi "\xC4\xE3")
    , testCase "a text row with one invalid string decodes, and only that value raises" $ do
        let kinds = map columnKind [varchar gbkChineseCi, varchar gbkChineseCi]
        values <- either (assertFailure . ("refused the row: " <>) . show) pure
            (decodeTextRow kinds (lenEnc "ok" <> lenEnc "\xC4\xE3"))
        case values of
            [ok, invalid] -> do
                ok @?= MySQLText "ok"
                used <- try (evaluate invalid)
                used @?= Left (InvalidUtf8Text gbkChineseCi "\xC4\xE3")
            other -> assertFailure ("expected two values, got " <> show (length other))
    ]

latin1SwedishCi, gbkChineseCi :: Word16
latin1SwedishCi = 8
gbkChineseCi = 28

varchar :: Word16 -> ColumnDef
varchar charset = ColumnDef "" "" "" "c" "c" charset 0 mySQLTypeVarString 0 0

lenEnc :: B.ByteString -> B.ByteString
lenEnc value = L.toStrict (runPut (putLenEncBytes value))
