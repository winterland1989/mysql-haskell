{-# LANGUAGE ScopedTypeVariables #-}

module UnixSocket (tests, findSocketPath) where

import qualified Data.ByteString       as B
import           Database.MySQL.Base
import qualified System.IO.Streams     as Stream
import           System.Directory      (doesFileExist)
import           System.Environment    (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Common Unix socket paths for MySQL/MariaDB.
defaultSocketPaths :: [FilePath]
defaultSocketPaths =
    [ "/tmp/mysql.sock"                    -- macOS Homebrew
    , "/var/run/mysqld/mysqld.sock"        -- Debian/Ubuntu
    , "/run/mysqld/mysqld.sock"            -- NixOS, Arch
    , "/var/lib/mysql/mysql.sock"          -- RHEL/CentOS
    , "/tmp/mysqld.sock"                   -- some macOS installs
    ]

-- | Find the MySQL Unix socket path.  Checks the @MYSQL_UNIX_SOCKET@
-- environment variable first, then probes common default locations.
findSocketPath :: IO (Maybe FilePath)
findSocketPath = do
    envPath <- lookupEnv "MYSQL_UNIX_SOCKET"
    case envPath of
        Just p  -> do
            exists <- doesFileExist p
            return $ if exists then Just p else Nothing
        Nothing -> firstExisting defaultSocketPaths
  where
    firstExisting []     = return Nothing
    firstExisting (p:ps) = do
        exists <- doesFileExist p
        if exists then return (Just p) else firstExisting ps

tests :: FilePath -> TestTree
tests socketPath = testGroup "unix-socket"
    [ testCaseSteps "connectUnixSocket: SELECT 1" $ \step -> do
        step "connecting via unix socket..."
        c <- connectUnixSocket socketPath defaultConnectInfo
            { ciUser     = "testMySQLHaskell"
            , ciDatabase = "testMySQLHaskell"
            }

        step "executing SELECT 1..."
        (_, is) <- query_ c "SELECT 1"
        Just row <- Stream.read is
        assertBool "SELECT 1 returns 1"
            (row == [MySQLInt32 1] || row == [MySQLInt64 1])
        Stream.skipToEof is

        close c

    , testCaseSteps "connectUnixSocketDetail: returns greeting" $ \step -> do
        step "connecting via unix socket with detail..."
        (greet, c) <- connectUnixSocketDetail socketPath defaultConnectInfo
            { ciUser     = "testMySQLHaskell"
            , ciDatabase = "testMySQLHaskell"
            }

        step "checking greeting version..."
        let ver = greetingVersion greet
        assertBool "greeting version is not empty" (not $ B.null ver)

        step "executing query to verify connection..."
        (_, is) <- query_ c "SELECT 1 + 1"
        Just row <- Stream.read is
        assertBool "SELECT 1+1 returns 2"
            (row == [MySQLInt32 2] || row == [MySQLInt64 2])
        Stream.skipToEof is

        close c

    , testCaseSteps "unix socket: prepared statement roundtrip" $ \step -> do
        step "connecting via unix socket..."
        c <- connectUnixSocket socketPath defaultConnectInfo
            { ciUser     = "testMySQLHaskell"
            , ciDatabase = "testMySQLHaskell"
            }

        step "executing prepared statement..."
        stmt <- prepareStmt c "SELECT ? + 1"
        (_, is) <- queryStmt c stmt [MySQLInt32 41]
        Just row <- Stream.read is
        assertBool "41 + 1 = 42"
            (row == [MySQLInt32 42] || row == [MySQLInt64 42])
        Stream.skipToEof is

        close c
    ]
