-- | String columns in the character set the server sent them in. latin1 bytes
-- used to go through a UTF-8 decoder that threw an impure exception.
module ResultCharsets (tests) where

import           Control.Exception              (SomeException, evaluate, try)
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
    , testCase "another charset decodes ASCII and fails to parse other bytes" $ do
        assertEqual "ASCII value" (Right (MySQLText "abc"))
            (parseOnly (getBinaryField (varchar gbkChineseCi)) (lenEnc "abc"))
        parsed <- try (evaluate (forced (parseOnly (getTextField (varchar gbkChineseCi)) (lenEnc "\xC4\xE3"))))
        case parsed of
            Right (Left _) -> pure ()
            Right (Right value) -> assertFailure ("decoded as " <> show value)
            Left (failure :: SomeException) -> assertFailure ("threw instead of failing the parse: " <> show failure)
    ]

-- | The value itself evaluated, so a decoder that throws does so here.
forced :: Either String MySQLValue -> Either String MySQLValue
forced parsed = case parsed of
    Left failure -> Left failure
    Right value -> value `seq` Right value

latin1SwedishCi, gbkChineseCi :: Word16
latin1SwedishCi = 8
gbkChineseCi = 28

varchar :: Word16 -> ColumnDef
varchar charset = ColumnDef "" "" "" "c" "c" charset 0 mySQLTypeVarString 0 0

lenEnc :: B.ByteString -> B.ByteString
lenEnc value = L.toStrict (runPut (putLenEncBytes value))
