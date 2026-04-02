{-# LANGUAGE ScopedTypeVariables #-}

module TLSConnection (tests) where

import qualified Data.ByteString       as B
import qualified Data.Text             as T
import           Database.MySQL.Base
import qualified Database.MySQL.TLS    as TLS
import           Database.MySQL.TLS    (makeClientParams, TrustedCAStore(..))
import qualified System.IO.Streams     as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: FilePath -> TestTree
tests caPath = testGroup "tls-connection"
    [ testCaseSteps "TLS connectDetail: SELECT 1" $ \step -> do
        step "creating TLS client params..."
        cparams <- makeClientParams (CustomCAStore caPath)

        step "connecting via TLS..."
        (greet, conn) <- TLS.connectDetail
            defaultConnectInfo { ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell" }
            (cparams, "Winter")

        step "checking greeting version..."
        let ver = greetingVersion greet
        assertBool "greeting version is not empty" (not $ B.null ver)

        step "executing SELECT 1..."
        (_, is) <- query_ conn "SELECT 1"
        Just row <- Stream.read is
        assertBool "SELECT 1 returns 1"
            (row == [MySQLInt32 1] || row == [MySQLInt64 1])
        Stream.skipToEof is

        close conn

    , testCaseSteps "TLS connection: verify encryption active" $ \step -> do
        step "creating TLS client params..."
        cparams <- makeClientParams (CustomCAStore caPath)

        step "connecting via TLS..."
        conn <- TLS.connect
            defaultConnectInfo { ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell" }
            (cparams, "Winter")

        step "checking SSL cipher..."
        (_, is) <- query_ conn "SHOW STATUS LIKE 'Ssl_cipher'"
        Just row <- Stream.read is
        case row of
            [_name, MySQLText cipher] ->
                assertBool "SSL cipher is not empty" (not $ T.null cipher)
            _ -> assertFailure ("unexpected row shape: " ++ show row)
        Stream.skipToEof is

        close conn

    , testCaseSteps "TLS connection: prepared statement roundtrip" $ \step -> do
        step "creating TLS client params..."
        cparams <- makeClientParams (CustomCAStore caPath)

        step "connecting via TLS..."
        conn <- TLS.connect
            defaultConnectInfo { ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell" }
            (cparams, "Winter")

        step "executing prepared statement..."
        stmt <- prepareStmt conn "SELECT ? + 1"
        (_, is) <- queryStmt conn stmt [MySQLInt32 41]
        Just row <- Stream.read is
        assertBool "41 + 1 = 42"
            (row == [MySQLInt32 42] || row == [MySQLInt64 42])
        Stream.skipToEof is

        close conn
    ]
