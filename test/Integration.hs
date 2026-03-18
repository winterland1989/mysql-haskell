module Main (main) where

import qualified Data.ByteString as B
import           Database.MySQL.Base
import           Test.Tasty (defaultMain, testGroup)
import qualified CachingSha2
import qualified MysqlTests
import qualified RoundtripBit
import qualified RoundtripYear
import qualified SelectOne
import qualified UnixSocket

main :: IO ()
main = do
    -- Connect briefly to detect the server version, so we can conditionally
    -- include tests that only apply to certain servers (e.g. caching_sha2 on MySQL 8.0+).
    (greet, c) <- connectDetail defaultConnectInfo
        {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    close c
    let ver = greetingVersion greet
        isMySql80 = "8." `B.isPrefixOf` ver
                 || "9." `B.isPrefixOf` ver

    -- Probe for a Unix domain socket (MYSQL_UNIX_SOCKET env var, then common paths).
    mSockPath <- UnixSocket.findSocketPath

    defaultMain $ testGroup "mysql-integration" $
        [ SelectOne.tests
        , RoundtripBit.tests
        , RoundtripYear.tests
        , MysqlTests.tests
        ]
        -- caching_sha2_password is MySQL 8.0+ only (MariaDB does not support it).
        -- The sha2 test users are created by the nix CI config for the MySQL 8.0 VM.
        ++ [ CachingSha2.tests | isMySql80 ]
        -- Unix socket tests are included only when a socket file is found.
        ++ [ UnixSocket.tests p | Just p <- [mSockPath] ]
