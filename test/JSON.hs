module JSON where

import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B
import Test.Tasty.HUnit
import Test.Tasty (TestTree)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import qualified Aeson as A
import qualified Data.Attoparsec.ByteString as A
import qualified AesonBP as B
import qualified Data.Binary.Parser as B
import Data.List


pathTo :: String -> IO FilePath
pathTo wat = do
  exists <- doesDirectoryExist "tests"
  return $ if exists
           then "tests" </> wat
           else wat

tests :: IO [TestTree]
tests = do
    path <- pathTo "json-data"
    names <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents path
    forM names $ \name -> do
        bs <- B.readFile (path </> name)
        return $ testCase name $
            assertEqual name (A.parseOnly A.jsonEOF' bs) (B.parseOnly B.jsonEOF' bs)


