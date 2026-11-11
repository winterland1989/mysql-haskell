-- | Text-protocol rows whose first value is 16 MiB or longer: its length
-- prefix starts with the same byte as an EOF packet, so the row was taken for
-- the end of the result. Binary-protocol rows start with 0x00 and never were.
module LargeRows (tests) where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as T
import           Database.MySQL.Base
import qualified System.IO.Streams          as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser                   (withTestConnection)

tests :: TestTree
tests = testGroup "values of 16 MiB and longer" (map firstValueOf [16777216, 16777217, 33554430])

firstValueOf :: Int -> TestTree
firstValueOf size = testCase (show size ++ " bytes") $ withTestConnection $ \c -> do
    (_, rows) <- query_ c (Query (LC.pack ("SELECT REPEAT('a', " ++ show size ++ "), 'after'")))
    Stream.toList rows >>= assertOneRow size

assertOneRow :: Int -> [[MySQLValue]] -> Assertion
assertOneRow size rows = case rows of
    [[MySQLText value, MySQLText after]] -> do
        assertEqual "length of the long value" size (T.length value)
        assertEqual "value after it" "after" after
    other -> assertFailure ("rows: " ++ take 200 (show other))
