module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified MysqlTests
import qualified SelectOne

main :: IO ()
main =
    defaultMain $ testGroup "mysql-integration" [
        SelectOne.tests
      , MysqlTests.tests
      ]
