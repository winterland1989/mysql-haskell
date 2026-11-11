-- | MariaDB's ed25519 authentication, checked against MariaDB's own answers:
-- the keys its server stores and the signatures its client library sends.
module Ed25519Scramble (tests) where

import           Control.Exception              (try)
import qualified Crypto.PubKey.Ed25519          as Ed25519
import           Crypto.Error                   (eitherCryptoError)
import           Data.Binary.Put                (putByteString, putWord8)
import qualified Data.ByteArray.Encoding        as Encoding
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base16         as Base16
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Lazy           as L
import           Data.IORef                     (modifyIORef', newIORef, readIORef)
import           Database.MySQL.Connection      (UnexpectedPacket (..), completeAuth,
                                                 ed25519PublicKey, scrambleEd25519)
import           Database.MySQL.Protocol.Packet (Packet (..), putToPacket)
import qualified System.IO.Streams              as Stream
import           Test.QuickCheck.Instances      ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

hex :: String -> IO B.ByteString
hex = either (assertFailure . ("not hex: " ++)) pure . Base16.decode . BC.pack

-- | The key as MariaDB shows it: base64 without its one padding character.
mariadbKey :: B.ByteString -> B.ByteString
mariadbKey = B.dropWhileEnd (== 0x3D) . Encoding.convertToBase Encoding.Base64

-- | What the server sends when an account needs client_ed25519.
authSwitch :: B.ByteString -> Packet
authSwitch nonce = putToPacket 2 $ do
    putWord8 0xFE
    putByteString "client_ed25519"
    putWord8 0
    putByteString nonce

okPacket :: Packet
okPacket = putToPacket 4 (putByteString "\x00\x00\x00\x02\x00\x00\x00")

-- | The packets 'completeAuth' writes while answering an AuthSwitchRequest for
-- this nonce, and whether it refused the request.
answerSwitch :: B.ByteString -> B.ByteString -> IO ([Packet], Either UnexpectedPacket ())
answerSwitch nonce pass = do
    written <- newIORef []
    replies <- Stream.fromList [okPacket]
    outcome <- try $ completeAuth replies (\p -> modifyIORef' written (p :)) pass (authSwitch nonce)
        (\_ _ _ _ -> assertFailure "ed25519 asked for full authentication")
    sent <- readIORef written
    pure (reverse sent, outcome)

newtype Nonce = Nonce B.ByteString deriving Show

instance Arbitrary Nonce where
    arbitrary = Nonce . B.pack <$> vectorOf 32 arbitrary

-- | MariaDB's server checks a reply with standard Ed25519 verification, so
-- every reply must pass it for the key MariaDB stores.
verifiesAsEd25519 :: B.ByteString -> Nonce -> Property
verifiesAsEd25519 pass (Nonce nonce) =
    either (\failure -> counterexample (show failure) False) property $ do
        key <- ed25519PublicKey pass >>= eitherCryptoError . Ed25519.publicKey
        signature <- scrambleEd25519 nonce pass >>= eitherCryptoError . Ed25519.signature
        pure (Ed25519.verify key nonce signature)

tests :: TestTree
tests = testGroup "ed25519 scramble"
    [ testCase "public keys are the ones MariaDB stores" $ do
        -- From MariaDB 11.4.9's mysql-test/suite/plugins/r/auth_ed25519.result
        -- and its ed25519_password(); the last is the account nix/ci.nix makes.
        mariadbKey <$> ed25519PublicKey "foo" @?= Right "vubFBzIrapbfHct1/J72dnUryz5VS7lA6XHH8sIx4TI"
        mariadbKey <$> ed25519PublicKey "foobar" @?= Right "qv2mG6HWCuy32Slb5xhV4THStewNz2VINVPbgk+XAJ8"
        mariadbKey <$> ed25519PublicKey "foo bar" @?= Right "Y5fV74JAVRMOK2cdnUsYS+WW9sXaaL/o+6WGKOgqnzc"
        mariadbKey <$> ed25519PublicKey "secret" @?= Right "ZIgUREUg5PVgQ6LskhXmO+eZLS0nC8be6HPjYWR4YJY"
        mariadbKey <$> ed25519PublicKey "ed25519Pass123" @?= Right "j2VKy3t+tdtEQXXes18uKRvK1XL9crguh3AregMR6kk"
      -- Signatures from ma_crypto_sign of mariadb-connector-c 3.4's ref10.
    , testCase "an AuthSwitchRequest is answered with libmariadb's signature, a nonce ending in 0 included" $ do
        nonce <- hex "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f00"
        signature <- hex "0c9e13ccd52e9dbb7957836f1c48ae216bf28da17c4b027f93534b0e5a348917\
                         \0096c63db819a35af8a62c976faa5bc436cb047911dcb3d28301cd07d5cd8303"
        (sent, outcome) <- answerSwitch nonce "ed25519Pass123"
        sent @?= [Packet 64 3 (L.fromStrict signature)]
        either (\(UnexpectedPacket p) -> assertFailure ("refused " ++ show p)) pure outcome
    , testCase "signature of another password and nonce" $ do
        nonce <- hex "ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100"
        signature <- hex "c0999e2c61f2fe2d679307b2b236930e1efc3680e43d4bb2b46f4250d6e11d55\
                         \047e75a777f4db9d60aa3ab64eebb3197a827df920f5c852bdd7be70ba0d6e0c"
        scrambleEd25519 nonce "secret" @?= Right signature
    , testCase "a nonce that is not 32 bytes is refused unsigned, as libmariadb does" $ do
        let request = authSwitch (B.replicate 20 7)
        (sent, outcome) <- answerSwitch (B.replicate 20 7) "ed25519Pass123"
        sent @?= []
        either (\(UnexpectedPacket p) -> p @?= request) (\() -> assertFailure "the nonce was accepted") outcome
    , testProperty "every reply verifies as Ed25519 under the stored key" verifiesAsEd25519
    ]
