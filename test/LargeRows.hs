-- | Text-protocol rows whose first value is 16 MiB or longer: its length
-- prefix starts with the same byte as an EOF packet, so the row was taken for
-- the end of the result. Binary-protocol rows start with 0x00 and never were.
module LargeRows (tests) where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as T
import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams          as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser                   (withTestConnection)

tests :: QueryApi -> TestTree
tests api = testGroup "values of 16 MiB and longer" (map (firstValueOf api) [16777216, 16777217, 33554430])

firstValueOf :: QueryApi -> Int -> TestTree
firstValueOf api size = testCase (show size ++ " bytes") $ withTestConnection $ \c -> do
    (_, rows) <- apiQuery_ api (ColumnCount 2) c (Query (LC.pack ("SELECT REPEAT('a', " ++ show size ++ "), 'after'")))
    Stream.toList rows >>= assertOneRow size

assertOneRow :: Int -> [[MySQLValue]] -> Assertion
assertOneRow size rows = case rows of
    [[MySQLText value, MySQLText after]] -> do
        assertEqual "length of the long value" size (T.length value)
        assertEqual "value after it" "after" after
    other -> assertFailure ("rows: " ++ take 200 (show other))
