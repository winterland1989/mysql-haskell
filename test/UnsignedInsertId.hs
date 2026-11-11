-- | Insert IDs above Int's range: BIGINT UNSIGNED AUTO_INCREMENT goes up to
-- 2^64 - 1, and OK packets carry it unsigned.
module UnsignedInsertId (tests) where

import           Database.MySQL.Base
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: TestTree
tests = testGroup "insert IDs above 2^63"
    [ testCase "execute_" $ withUnsignedIds $ \c -> do
        ok <- execute_ c "INSERT INTO unsigned_ids (v) VALUES (1)"
        assertEqual "last insert ID" firstId (toInteger (okLastInsertID ok))
    , testCase "executeStmt" $ withUnsignedIds $ \c -> do
        stmt <- prepareStmt c "INSERT INTO unsigned_ids (v) VALUES (?)"
        ok <- executeStmt c stmt [MySQLInt32 1]
        assertEqual "last insert ID" firstId (toInteger (okLastInsertID ok))
    ]

firstId :: Integer
firstId = 18446744073709551000

withUnsignedIds :: (MySQLConn -> IO ()) -> Assertion
withUnsignedIds run = withTestConnection $ \c -> do
    _ <- execute_ c "CREATE TEMPORARY TABLE unsigned_ids \
                    \(id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY, v INT) \
                    \AUTO_INCREMENT = 18446744073709551000"
    run c
