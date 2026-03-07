{-# LANGUAGE ScopedTypeVariables #-}

module SelectOne (tests) where

import Database.MySQL.Base
import qualified System.IO.Streams as Stream
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCaseSteps "select 1" $ \step -> do
    step "connecting..."
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }

    step "executing SELECT 1..."
    (_, is) <- query_ c "SELECT 1"
    Just row <- Stream.read is
    -- MySQL 8.0 returns MySQLInt64 for integer literals, older versions return MySQLInt32
    assertBool "SELECT 1 returns 1" (row == [MySQLInt32 1] || row == [MySQLInt64 1])
    Stream.skipToEof is

    close c
