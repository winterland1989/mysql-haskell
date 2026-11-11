-- | The Mozilla CA bundle behind 'MozillaCAStore', which an installed library
-- lacked: it was packaged as an extra source file, not a data file.
module CABundle (tests) where

import           Data.TLSSetting   (mozillaCAStorePath)
import           System.Directory  (doesFileExist)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testCase "the Mozilla CA bundle is installed" $ do
    path <- mozillaCAStorePath
    exists <- doesFileExist path
    assertBool ("no CA bundle at " ++ path) exists
