module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import qualified JSON
import qualified MysqlTests
import qualified SelectOne
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
      testGroup "mysql" [
          SelectOne.tests
          -- TODO: the full test suite has a binary protocol bug
          -- on modern MySQL/MariaDB (error 1210).
          -- Fix the binary protocol and remove expectFail.
        , expectFail MysqlTests.tests
      ],
      testGroup "word24"
         Word24.tests
      , testGroup "tcp-streams"
         [
           TCPStreams.tests
         ]
      ]
