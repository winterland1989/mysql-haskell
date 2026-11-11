-- | Tests of "Database.MariaDB" that need a MariaDB server; nix/ci.nix
-- runs them in the MariaDB VM only. Capability negotiation against MySQL is
-- covered by the shared integration suite.
module Main (main) where

import           System.Environment (lookupEnv)
import           Test.Tasty (defaultMain, testGroup)
import qualified MariaDBBulk
import qualified MariaDBEd25519
import qualified MariaDBExtendedMetadata
import qualified UnixSocket

main :: IO ()
main = do
    socketPath <- UnixSocket.findSocketPath
    caPath <- lookupEnv "MYSQL_TLS_CA_PATH"
    defaultMain $ testGroup "mariadb-integration"
        [ MariaDBExtendedMetadata.tests socketPath caPath
        , MariaDBBulk.tests
        , MariaDBEd25519.tests socketPath caPath
        ]
