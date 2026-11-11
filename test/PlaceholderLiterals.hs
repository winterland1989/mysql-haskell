-- | A question mark inside a string literal or a comment sent to the server
-- through 'query', where it used to count as a placeholder.
module PlaceholderLiterals (tests) where

import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser            (withTestConnection)

tests :: QueryApi -> TestTree
tests api = testCase "a question mark in a literal or comment is not a placeholder" $
    withTestConnection $ \c -> do
        (_, rows) <- apiQuery api (ColumnCount 2) c "SELECT ?, '?' -- is this a placeholder?\n"
            [One (MySQLText "filled")]
        result <- Stream.toList rows
        assertEqual "rows" [[MySQLText "filled", MySQLText "?"]] result
