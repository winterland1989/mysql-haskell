module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import qualified MysqlTests
import qualified SelectOne

main :: IO ()
main =
    defaultMain $ testGroup "mysql-integration" [
        SelectOne.tests
        -- TODO: the full test suite has a binary protocol bug
        -- on modern MySQL/MariaDB (error 1210).
        -- Fix the binary protocol and remove expectFail.
      , expectFail MysqlTests.tests
      ]
