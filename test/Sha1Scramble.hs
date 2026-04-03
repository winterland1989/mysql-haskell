module Sha1Scramble (tests) where

import qualified Data.ByteString as B
import           Data.Word (Word8)
import           Data.Char (digitToInt)
import           Database.MySQL.Connection (scrambleSHA1)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Decode a hex string to bytes (test helper to avoid base16-bytestring dep).
hexToBytes :: String -> B.ByteString
hexToBytes [] = B.empty
hexToBytes (hi:lo:rest) = B.cons (fromIntegral $ digitToInt hi * 16 + digitToInt lo :: Word8) (hexToBytes rest)
hexToBytes [_] = error "hexToBytes: odd-length hex string"

tests :: TestTree
tests = testGroup "SHA1 Scramble"
    [ testCase "empty password returns empty" $ do
        let result = scrambleSHA1 "some_salt" ""
        assertEqual "empty password" B.empty result

    , testCase "non-empty password returns 20 bytes" $ do
        let result = scrambleSHA1 "12345678901234567890" "password"
        assertEqual "scramble length" 20 (B.length result)

    , testCase "different salts produce different scrambles" $ do
        let r1 = scrambleSHA1 "salt1_______________" "password"
            r2 = scrambleSHA1 "salt2_______________" "password"
        assertBool "different salts should produce different results" (r1 /= r2)

    , testCase "different passwords produce different scrambles" $ do
        let salt = "12345678901234567890"
            r1 = scrambleSHA1 salt "password1"
            r2 = scrambleSHA1 salt "password2"
        assertBool "different passwords should produce different results" (r1 /= r2)

    , testCase "scramble is deterministic" $ do
        let salt = "12345678901234567890"
            r1 = scrambleSHA1 salt "testPassword123"
            r2 = scrambleSHA1 salt "testPassword123"
        assertEqual "same inputs produce same output" r1 r2

    , testCase "golden: salt=12345678901234567890 pass=password" $ do
        let result = scrambleSHA1 "12345678901234567890" "password"
            expected = hexToBytes "1957dce2724282e018f40d905824cb6361f88d41"
        assertEqual "golden vector 1" expected result

    , testCase "golden: salt=ABCDEFGHIJKLMNOPQRST pass=secret" $ do
        let result = scrambleSHA1 "ABCDEFGHIJKLMNOPQRST" "secret"
            expected = hexToBytes "28441590674285e7d03cae7af237504797f70e91"
        assertEqual "golden vector 2" expected result
    ]
