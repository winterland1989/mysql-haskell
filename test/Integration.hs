module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified MysqlTests
import qualified RoundtripBit
import qualified RoundtripYear
import qualified SelectOne

main :: IO ()
main =
    defaultMain $ testGroup "mysql-integration" [
        SelectOne.tests
      , RoundtripBit.tests
      , RoundtripYear.tests
      , MysqlTests.tests
      ]
