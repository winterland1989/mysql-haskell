-- | Helpers for setting up a tls connection with @tls@ package,
-- for further customization, please refer to @tls@ package.
--
-- Note, functions in this module will throw error if can't load certificates or CA store.
--
module Data.TLSSetting
    ( -- * Choose a CAStore
      TrustedCAStore(..)
      -- * Make TLS settings
    , makeClientParams
    , makeClientParams'
    , makeServerParams
    , makeServerParams'
      -- * Internal
    , mozillaCAStorePath
    ) where

import qualified Data.ByteString            as B
import           Data.Default.Class         (def)
import qualified Data.PEM                   as X509
import qualified Data.X509                  as X509
import qualified Data.X509.CertificateStore as X509
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLS
import           Paths_mysql_pure          (getDataFileName)
import qualified System.X509                as X509

-- | The whole point of TLS is that: a peer should have already trusted
-- some certificates, which can be used for validating other peer's certificates.
-- if the certificates sent by other side form a chain. and one of them is issued
-- by one of 'TrustedCAStore', Then the peer will be trusted.
--
data TrustedCAStore
    = SystemCAStore                   -- ^ provided by your operating system.
    | MozillaCAStore                  -- ^ provided by <https://curl.haxx.se/docs/caextract.html Mozilla>.
    | CustomCAStore FilePath          -- ^ provided by your self, the CA file can contain multiple certificates.
  deriving (Show, Eq)

-- | Get the built-in mozilla CA's path.
mozillaCAStorePath :: IO FilePath
mozillaCAStorePath = getDataFileName "mozillaCAStore.pem"

makeCAStore :: TrustedCAStore -> IO X509.CertificateStore
makeCAStore SystemCAStore       = X509.getSystemCertificateStore
makeCAStore MozillaCAStore      = makeCAStore . CustomCAStore =<< mozillaCAStorePath
makeCAStore (CustomCAStore fp)  = do
    bs <- B.readFile fp
    let Right pems = X509.pemParseBS bs
    case mapM (X509.decodeSignedCertificate . X509.pemContent) pems of
        Right cas -> return (X509.makeCertificateStore cas)
        Left err  -> error err

-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- without providing client's own certificate. suitable for connecting server which don't
-- validate clients.
--
-- we defer setting of 'TLS.clientServerIdentification' to connecting phase.
--
-- Note, tls's default validating method require server has v3 certificate.
-- you can use openssl's V3 extension to issue such a certificate. or change 'TLS.ClientParams'
-- before connecting.
--
makeClientParams :: TrustedCAStore          -- ^ trusted certificates.
                 -> IO TLS.ClientParams
makeClientParams tca = do
    caStore <- makeCAStore tca
    return (TLS.defaultParamsClient "" B.empty)
        {   TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_all }
        ,   TLS.clientShared    = def
            {   TLS.sharedCAStore         = caStore
            ,   TLS.sharedValidationCache = def
            }
        }

-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- while providing client's own certificate as well. suitable for connecting server which
-- validate clients.
--
-- Also only accept v3 certificate.
--
makeClientParams' :: FilePath       -- ^ public certificate (X.509 format).
                  -> [FilePath]     -- ^ chain certificates (X.509 format).
                                    --   the root of your certificate chain should be
                                    --   already trusted by server, or tls will fail.
                  -> FilePath       -- ^ private key associated.
                  -> TrustedCAStore -- ^ trusted certificates.
                  -> IO TLS.ClientParams
makeClientParams' pub certs priv tca = do
    p <- makeClientParams tca
    c <- TLS.credentialLoadX509Chain pub certs priv
    case c of
        Right c' ->
            return p
                {   TLS.clientShared = (TLS.clientShared p)
                    {
                        TLS.sharedCredentials = TLS.Credentials [c']
                    }
                }
        Left err -> error err

-- | make a simple tls 'TLS.ServerParams' without validating client's certificate.
--
makeServerParams :: FilePath        -- ^ public certificate (X.509 format).
                 -> [FilePath]      -- ^ chain certificates (X.509 format).
                                    --   the root of your certificate chain should be
                                    --   already trusted by client, or tls will fail.
                 -> FilePath        -- ^ private key associated.
                 -> IO TLS.ServerParams
makeServerParams pub certs priv = do
    c <- TLS.credentialLoadX509Chain pub certs priv
    case c of
        Right c'@(X509.CertificateChain c'', _) ->
            return def
                {   TLS.serverCACertificates =  c''
                ,   TLS.serverShared = def
                    {
                        TLS.sharedCredentials = TLS.Credentials [c']
                    }
                ,   TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                }
        Left err -> error err

-- | make a tls 'TLS.ServerParams' that also validating client's certificate.
--
makeServerParams' :: FilePath       -- ^ public certificate (X.509 format).
                  -> [FilePath]     -- ^ chain certificates (X.509 format).
                  -> FilePath       -- ^ private key associated.
                  -> TrustedCAStore -- ^ server will use these certificates to validate clients.
                  -> IO TLS.ServerParams
makeServerParams' pub certs priv tca = do
    caStore <- makeCAStore tca
    p <- makeServerParams pub certs priv
    return p
        {   TLS.serverWantClientCert = True
        ,   TLS.serverShared = (TLS.serverShared p)
            {   TLS.sharedCAStore = caStore
            }
        }
