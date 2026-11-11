-- | Which question marks 'renderParams' fills: not those inside quotes or
-- comments, which used to count as placeholders.
module Placeholders (tests) where

import           Control.Exception              (evaluate, try)
import qualified Data.ByteString.Lazy           as L
import           Database.MySQL.Base
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "placeholders"
    [ renders "single-quoted string" "SELECT '?', ?" [one] "SELECT '?', 1"
    , renders "double-quoted string and quoted identifier"
        "SELECT \"?\", `a?b`, ?" [one] "SELECT \"?\", `a?b`, 1"
    , renders "doubled and escaped quotes stay inside the string"
        "SELECT 'it''s ?', 'C\\'s ?', ?" [one] "SELECT 'it''s ?', 'C\\'s ?', 1"
    , renders "a backslash does not escape in a quoted identifier"
        "SELECT `a\\`, ?, '?'" [one] "SELECT `a\\`, 1, '?'"
    , renders "dash comment" "SELECT ? -- why?\n, ?" [one, two] "SELECT 1 -- why?\n, 2"
    , renders "hash comment" "SELECT ? # why?\n, ?" [one, two] "SELECT 1 # why?\n, 2"
    , renders "block comment" "SELECT /* ? */ ?" [one] "SELECT /* ? */ 1"
    , renders "two dashes without a space are arithmetic" "SELECT ?--?, '?'" [one, two] "SELECT 1--2, '?'"
    , renders "an executable comment is SQL" "SELECT /*! ? */ '?'" [one] "SELECT /*! 1 */ '?'"
    , testCase "more parameters than placeholders outside quotes" $ do
        rendered <- try (evaluate (L.length (fromQuery (renderParams "SELECT ?, '?'" [one, two]))))
        case rendered of
            Left WrongParamsCount -> pure ()
            Right _ -> assertFailure "rendered the second parameter into the string literal"
    ]

renders :: TestName -> Query -> [MySQLValue] -> L.ByteString -> TestTree
renders name sql params expected = testCase name $
    assertEqual "rendered SQL" expected (fromQuery (renderParams sql params))

one, two :: MySQLValue
one = MySQLInt32 1
two = MySQLInt32 2
