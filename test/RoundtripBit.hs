module RoundtripBit (tests) where

import Database.MySQL.Base
import qualified System.IO.Streams as Stream
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCaseSteps "roundtrip MySQLBit" $ \step -> do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }

    execute_ c "CREATE TEMPORARY TABLE test_bit (__id INT, __val BIT(16))"
    execute_ c "INSERT INTO test_bit VALUES (1, NULL)"

    step "roundtrip MySQLBit via binary protocol"
    updStmt <- prepareStmt c "UPDATE test_bit SET __val = ? WHERE __id = 1"
    selStmt <- prepareStmt c "SELECT __val FROM test_bit WHERE __id = 1"

    let bitVal = 43744 -- 0b1010101011100000
    executeStmt c updStmt [MySQLBit bitVal]

    (_, is) <- queryStmt c selStmt []
    Just [v] <- Stream.read is
    Stream.skipToEof is
    assertEqual "MySQLBit roundtrips through BIT(16)" (MySQLBit bitVal) v

    step "roundtrip MySQLBit zero"
    executeStmt c updStmt [MySQLBit 0]

    (_, is2) <- queryStmt c selStmt []
    Just [v2] <- Stream.read is2
    Stream.skipToEof is2
    assertEqual "MySQLBit 0 roundtrips" (MySQLBit 0) v2

    step "roundtrip MySQLBit max for BIT(16)"
    executeStmt c updStmt [MySQLBit 65535]

    (_, is3) <- queryStmt c selStmt []
    Just [v3] <- Stream.read is3
    Stream.skipToEof is3
    assertEqual "MySQLBit 65535 roundtrips" (MySQLBit 65535) v3

    close c
