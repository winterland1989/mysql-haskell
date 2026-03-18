module Main (main) where

import           Control.Exception (IOException, try)
import qualified Data.ByteString as B
import           Database.MySQL.Base
import           System.IO (hPutStrLn, stderr)
import           Test.Tasty (defaultMain, testGroup)
import qualified CachingSha2
import qualified MysqlTests
import qualified RoundtripBit
import qualified RoundtripYear
import qualified SelectOne
import qualified UnixSocket

main :: IO ()
main = do
    -- Try to connect to MySQL; skip all integration tests if the server is unreachable.
    result <- try $ connectDetail defaultConnectInfo
        {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    case result of
        Left e -> do
            hPutStrLn stderr $ "\n*** WARNING: No MySQL server available, skipping ALL integration tests ***\n    " ++ show (e :: IOException) ++ "\n"
            defaultMain $ testGroup "mysql-integration" []
        Right (greet, c) -> do
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
