-- | FLOAT and DOUBLE values MySQL writes in exponent form. The text protocol
-- sends large ones as, for example, @1e20@; reading them as plain decimals
-- stopped at the @e@ and gave 1.
module FloatingPoint (tests) where

import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

exponentForms :: Query
exponentForms =
    "SELECT CAST(1e20 AS DOUBLE), CAST(-1e20 AS DOUBLE), CAST(1.5e300 AS DOUBLE), \
    \CAST(2.5e-7 AS DOUBLE), CAST(1e20 AS FLOAT)"

expected :: [MySQLValue]
expected = [MySQLDouble 1e20, MySQLDouble (-1e20), MySQLDouble 1.5e300, MySQLDouble 2.5e-7, MySQLFloat 1e20]

tests :: QueryApi -> TestTree
tests api = testGroup "FLOAT and DOUBLE in exponent form"
    [ testCase "text protocol" $ withConnection $ \c -> do
        (_, rows) <- apiQuery_ api (ColumnCount 5) c exponentForms
        values <- Stream.toList rows
        assertEqual "values" [expected] values
    , testCase "binary protocol" $ withConnection $ \c -> do
        stmt <- prepareStmt c exponentForms
        (_, rows) <- apiQueryStmt api (ColumnCount 5) c stmt []
        values <- Stream.toList rows
        assertEqual "values" [expected] values
    ]

withConnection :: (MySQLConn -> Assertion) -> Assertion
withConnection body = do
    (_, c) <- connectDetail defaultConnectInfo
        { ciUser = "testMySQLHaskell"
        , ciDatabase = "testMySQLHaskell"
        }
    body c
    close c
