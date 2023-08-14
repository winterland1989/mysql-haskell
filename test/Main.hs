module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import Test.Tasty (defaultMain, testGroup)
import qualified JSON
import qualified MysqlTests
import qualified WireStreams
import qualified Word24
import qualified TCPStreams

main :: IO ()
main = do
    jsonTests <- JSON.tests
    defaultMain $ testGroup "tests" [
      testGroup "binary-parser" [
        testGroup "bs" ByteString.tests
      , testGroup "combinator" Combinator.tests
      , testGroup "JSON" jsonTests
      ],
      testGroup "wire-stream" [
         WireStreams.tests
      ],
      testGroup "mysql" [
          -- TODO figure out how to run the tests that need a mysql
          -- db
          -- MysqlTests.tests
      ],
      testGroup "word24"
         Word24.tests
      testGroup "tcp-streams"
         TCPStreams.tests
      ]
