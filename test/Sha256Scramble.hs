module Sha256Scramble (tests) where

import qualified Data.ByteString as B
import           Database.MySQL.Connection (scrambleSHA256)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "SHA256 Scramble"
    [ testCase "empty password returns empty" $ do
        let result = scrambleSHA256 "some_salt" ""
        assertEqual "empty password" B.empty result

    , testCase "non-empty password returns 32 bytes" $ do
        let result = scrambleSHA256 "12345678901234567890" "password"
        assertEqual "scramble length" 32 (B.length result)

    , testCase "different salts produce different scrambles" $ do
        let r1 = scrambleSHA256 "salt1_______________" "password"
            r2 = scrambleSHA256 "salt2_______________" "password"
        assertBool "different salts should produce different results" (r1 /= r2)

    , testCase "different passwords produce different scrambles" $ do
        let salt = "12345678901234567890"
            r1 = scrambleSHA256 salt "password1"
            r2 = scrambleSHA256 salt "password2"
        assertBool "different passwords should produce different results" (r1 /= r2)

    , testCase "scramble is deterministic" $ do
        let salt = "12345678901234567890"
            r1 = scrambleSHA256 salt "testPassword123"
            r2 = scrambleSHA256 salt "testPassword123"
        assertEqual "same inputs produce same output" r1 r2
    ]
