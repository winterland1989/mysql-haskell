-- | Logins as an account identified via MariaDB's ed25519 plugin, which
-- nix/ci.nix creates in the MariaDB VM. The server greets with
-- mysql_native_password and switches the client to client_ed25519 once it
-- knows the account.
module MariaDBEd25519 (tests) where

import           Database.MySQL.Base
import qualified Database.MySQL.TLS  as TLS
import qualified System.IO.Streams   as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

ed25519ConnectInfo :: ConnectInfo
ed25519ConnectInfo = defaultConnectInfo
    { ciUser = "testMySQLHaskellEd25519"
    , ciPassword = "ed25519Pass123"
    , ciDatabase = "testMySQLHaskell"
    }

-- | The account the server authenticated, so a login that fell back to some
-- other account would not pass.
assertEd25519Account :: MySQLConn -> Assertion
assertEd25519Account conn = do
    (_, rows) <- query_ conn "SELECT CURRENT_USER()"
    users <- Stream.toList rows
    users @?= [[MySQLText "testMySQLHaskellEd25519@localhost"]]
    close conn

tests :: Maybe FilePath -> Maybe FilePath -> TestTree
tests socketPath caPath = testGroup "ed25519 authentication" $
    [ testCase "logs in over TCP" $
        connect ed25519ConnectInfo >>= assertEd25519Account
    ]
    ++ maybe [] (pure . unixSocketTest) socketPath
    ++ maybe [] (pure . tlsTest) caPath

unixSocketTest :: FilePath -> TestTree
unixSocketTest path = testCase "logs in over a Unix socket" $
    connectUnixSocket path ed25519ConnectInfo >>= assertEd25519Account

tlsTest :: FilePath -> TestTree
tlsTest caPath = testCase "logs in over TLS" $ do
    cparams <- TLS.makeClientParams (TLS.CustomCAStore caPath)
    TLS.connect ed25519ConnectInfo (cparams, "Winter") >>= assertEd25519Account
