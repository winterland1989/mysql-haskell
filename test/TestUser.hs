-- | The account nix/ci.nix creates for the integration suite.
module TestUser (testConnectInfo, withTestConnection) where

import           Control.Exception   (bracket)
import           Database.MySQL.Base

testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfoMB4
    { ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell" }

withTestConnection :: (MySQLConn -> IO a) -> IO a
withTestConnection = bracket (connect testConnectInfo) close
