module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import Test.Tasty (defaultMain, testGroup)
import qualified JSON
import qualified MysqlTests
import qualified WireStreams

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
          MysqlTests.tests
      ]
      ]
