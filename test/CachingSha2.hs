{-# LANGUAGE ScopedTypeVariables #-}

module CachingSha2 (tests) where

import Database.MySQL.Base
import qualified System.IO.Streams as Stream
import Test.Tasty
import Test.Tasty.HUnit

-- | These tests exercise two different authentication paths in 'completeAuth'.
-- Both tests connect and run @SELECT 1@, but the server-side auth protocol
-- differs based on which MySQL user is used. The users are created in
-- @nix/ci.nix@ (integrated-checks-mysql80) with different auth plugins:
--
-- * @testMySQLHaskellSha2@ — created with @caching_sha2_password@ (the MySQL 8.0
--   default). The client sends a SHA256 scramble, the server responds with
--   AuthMoreData (0x01, byte 2 = 0x03) indicating fast auth success.
--   The CI script pre-caches the verifier via a unix socket login so the
--   fast path is guaranteed.
--
-- * @testMySQLHaskellNative@ — created with @mysql_native_password@. The server
--   advertises @caching_sha2_password@ in its Greeting, so the client initially
--   sends a SHA256 scramble. The server then responds with AuthSwitchRequest
--   (0xFE) telling the client to re-authenticate using @mysql_native_password@
--   with a new salt. The client re-scrambles with SHA1 and sends the response.
tests :: TestTree
tests = testGroup "caching_sha2_password"
    [ testCaseSteps "SHA256 fast auth" $ \step -> do
        step "connecting as testMySQLHaskellSha2 (caching_sha2_password)..."
        (_, c) <- connectDetail defaultConnectInfo
            { ciUser = "testMySQLHaskellSha2"
            , ciPassword = "testPassword123"
            , ciDatabase = "testMySQLHaskell"
            }

        step "executing SELECT 1..."
        (_, is) <- query_ c "SELECT 1"
        Just row <- Stream.read is
        assertBool "SELECT 1 returns 1" (row == [MySQLInt32 1] || row == [MySQLInt64 1])
        Stream.skipToEof is

        close c

    , testCaseSteps "AuthSwitchRequest handling (mysql_native_password on sha2 server)" $ \step -> do
        step "connecting as testMySQLHaskellNative (mysql_native_password)..."
        (_, c) <- connectDetail defaultConnectInfo
            { ciUser = "testMySQLHaskellNative"
            , ciPassword = "nativePass123"
            , ciDatabase = "testMySQLHaskell"
            }

        step "executing SELECT 1..."
        (_, is) <- query_ c "SELECT 1"
        Just row <- Stream.read is
        assertBool "SELECT 1 returns 1" (row == [MySQLInt32 1] || row == [MySQLInt64 1])
        Stream.skipToEof is

        close c
    ]
