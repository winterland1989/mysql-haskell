module RoundtripYear (tests) where

import Database.MySQL.Base
import qualified System.IO.Streams as Stream
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCaseSteps "roundtrip MySQLYear" $ \step -> do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }

    execute_ c "CREATE TEMPORARY TABLE test_year (__id INT, __val YEAR(4))"
    execute_ c "INSERT INTO test_year VALUES (1, NULL)"

    step "roundtrip MySQLYear via binary protocol"
    updStmt <- prepareStmt c "UPDATE test_year SET __val = ? WHERE __id = 1"
    selStmt <- prepareStmt c "SELECT __val FROM test_year WHERE __id = 1"

    executeStmt c updStmt [MySQLYear 1999]

    (_, is) <- queryStmt c selStmt []
    Just [v] <- Stream.read is
    Stream.skipToEof is
    assertEqual "MySQLYear 1999 roundtrips" (MySQLYear 1999) v

    step "roundtrip MySQLYear min (1901)"
    executeStmt c updStmt [MySQLYear 1901]

    (_, is2) <- queryStmt c selStmt []
    Just [v2] <- Stream.read is2
    Stream.skipToEof is2
    assertEqual "MySQLYear 1901 roundtrips" (MySQLYear 1901) v2

    step "roundtrip MySQLYear max (2155)"
    executeStmt c updStmt [MySQLYear 2155]

    (_, is3) <- queryStmt c selStmt []
    Just [v3] <- Stream.read is3
    Stream.skipToEof is3
    assertEqual "MySQLYear 2155 roundtrips" (MySQLYear 2155) v3

    step "roundtrip MySQLYear zero"
    executeStmt c updStmt [MySQLYear 0]

    (_, is4) <- queryStmt c selStmt []
    Just [v4] <- Stream.read is4
    Stream.skipToEof is4
    assertEqual "MySQLYear 0 roundtrips" (MySQLYear 0) v4

    close c
