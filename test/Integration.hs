module Main (main) where

import qualified Data.ByteString as B
import           Database.MySQL.Base
import           System.Environment (lookupEnv)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import qualified CachingSha2
import qualified MultipleResults
import qualified MysqlTests
import           QueryApi
import qualified QueryWithoutResultSet
import qualified RoundtripBit
import qualified RoundtripYear
import qualified RowDecoderQueries
import qualified SelectOne
import qualified TLSConnection
import qualified UnixSocket

main :: IO ()
main = do
    (greet, c) <- connectDetail defaultConnectInfo
        {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    close c
    let ver = greetingVersion greet
        isMySql80 = "8." `B.isPrefixOf` ver
                 || "9." `B.isPrefixOf` ver

    -- Probe for a Unix domain socket (MYSQL_UNIX_SOCKET env var, then common paths).
    mSockPath <- UnixSocket.findSocketPath

    -- Probe for TLS CA certificate path (set in NixOS VM tests).
    mTlsCaPath <- lookupEnv "MYSQL_TLS_CA_PATH"

    defaultMain $ testGroup "mysql-integration" $
        [ MysqlTests.tests
        , MultipleResults.tests
        , RowDecoderQueries.tests
        ]
        ++ map (apiTests isMySql80 mSockPath mTlsCaPath) queryApis

-- | The suites that read rows, through one of the 'QueryApi's.
apiTests :: Bool -> Maybe FilePath -> Maybe FilePath -> QueryApi -> TestTree
apiTests isMySql80 mSockPath mTlsCaPath api = testGroup (apiName api) $
    [ SelectOne.tests api
    , RoundtripBit.tests api
    , RoundtripYear.tests api
    , QueryWithoutResultSet.tests api
    , MultipleResults.rowTests api
    ]
    -- caching_sha2_password is MySQL 8.0+ only (MariaDB does not support it).
    -- The sha2 test users are created by the nix CI config for the MySQL 8.0 VM.
    ++ [ CachingSha2.tests api | isMySql80 ]
    -- Unix socket tests are included only when a socket file is found.
    ++ maybe [] (\p -> [UnixSocket.tests api p]) mSockPath
    -- TLS tests are included only when a CA certificate path is provided.
    ++ maybe [] (\p -> [TLSConnection.tests api p]) mTlsCaPath
