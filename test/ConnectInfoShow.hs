module ConnectInfoShow (tests) where

import           Database.MySQL.Base (ConnectInfo (..))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Show ConnectInfo"
    [ testCase "the password is redacted, every other field shown" $
        show connectInfo @?= shown
    , testCase "parenthesised as a constructor argument" $
        show (Just connectInfo) @?= "Just (" ++ shown ++ ")"
    ]

connectInfo :: ConnectInfo
connectInfo = ConnectInfo
    { ciHost = "db.example.test"
    , ciPort = 3307
    , ciDatabase = "shop"
    , ciUser = "app"
    , ciPassword = "correct horse battery staple"
    , ciCharset = 224
    }

shown :: String
shown = "ConnectInfo {ciHost = \"db.example.test\", ciPort = 3307, ciDatabase = \"shop\", ciUser = \"app\", ciPassword = <redacted>, ciCharset = 224}"
