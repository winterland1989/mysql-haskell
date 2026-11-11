-- | Logging in as an account that requires a client certificate, which
-- 'makeClientParams'' loaded but never sent before 1.4.2.
module TLSClientCertificate (tests) where

import           Control.Exception     (bracket_)
import           Database.MySQL.Base
import qualified Database.MySQL.TLS    as TLS
import           Database.MySQL.TLS    (makeClientParams', TrustedCAStore(..))
import qualified System.IO.Streams     as Stream
import           System.FilePath       (replaceFileName)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser              (withTestConnection)

-- | The certificate and key sit next to the CA at @caPath@, which signed them.
tests :: FilePath -> TestTree
tests caPath = testCase "an account requiring a client certificate logs in" $
    withTestConnection $ \admin -> bracket_
        (execute_ admin createCertificateUser)
        (execute_ admin "DROP USER 'tlsClientCert'@'localhost'") $ do
            params <- makeClientParams'
                (replaceFileName caPath "client-cert.pem")
                []
                (replaceFileName caPath "client-key.pem")
                (CustomCAStore caPath)
            conn <- TLS.connect certificateConnectInfo (params, "Winter")
            (_, rows) <- query_ conn "SELECT CURRENT_USER()"
            loggedInAs <- Stream.toList rows
            close conn
            loggedInAs @?= [[MySQLText "tlsClientCert@localhost"]]

-- | The subject is the one test/cert/client-cert.pem carries, so only that
-- certificate opens the account.
createCertificateUser :: Query
createCertificateUser =
    "CREATE USER 'tlsClientCert'@'localhost' IDENTIFIED WITH mysql_native_password \
    \REQUIRE SUBJECT '/O=mysql-haskell/CN=testClientCert'"

certificateConnectInfo :: ConnectInfo
certificateConnectInfo = defaultConnectInfo { ciUser = "tlsClientCert", ciDatabase = "" }
