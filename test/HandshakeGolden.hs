module HandshakeGolden (tests) where

import qualified Data.Binary                as Binary
import           Data.Bits                  ((.|.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as Hex
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import           Database.MySQL.Connection  (handshakeResponsePacket, sslRequestPacket)
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Packet (Packet, getFromPacket, putToPacket)
import           Data.Binary.Put            (putByteString)
import           Test.Tasty
import           Test.Tasty.HUnit

-- Greetings captured from the servers nix/ci.nix tests against, and the
-- packets mysql-haskell 1.3.3 (92a9401) answered them with for user and
-- schema testMySQLHaskell, password "secret" and charset 33.
mariadbGreeting, mysqlGreeting :: B.ByteString
mariadbGreeting = hex "0a31312e342e392d4d6172696144422d6c6f67000a00000068513b714856583500feffe00200ff81150000000000001d0000003e78646c727473657e6b7a47006d7973716c5f6e61746976655f70617373776f726400"
mysqlGreeting = hex "0a382e302e3435001200000073454f634d16264e00ffffff0200ffdf15000000000000000000005d2658501266396b0d7546110063616368696e675f736861325f70617373776f726400"

masterAuthMariaDB, masterTLSAuthMariaDB, masterAuthMySQL, masterTLSAuthMySQL, masterSSLRequest :: B.ByteString
masterAuthMariaDB = hex "6d0000010da30b00ffffff00210000000000000000000000000000000000000000000000746573744d7953514c4861736b656c6c00148993d478ffb390507c14ce8d6da10baff99ac06c746573744d7953514c4861736b656c6c006d7973716c5f6e61746976655f70617373776f726400"
masterTLSAuthMariaDB = hex "6d0000020da30b00ffffff00210000000000000000000000000000000000000000000000746573744d7953514c4861736b656c6c00148993d478ffb390507c14ce8d6da10baff99ac06c746573744d7953514c4861736b656c6c006d7973716c5f6e61746976655f70617373776f726400"
masterAuthMySQL = hex "790000010da30b00ffffff00210000000000000000000000000000000000000000000000746573744d7953514c4861736b656c6c00208d6350e1245ca38229fd5cc86234ea67d2d19056795007230dbec8bd483e4b89746573744d7953514c4861736b656c6c0063616368696e675f736861325f70617373776f726400"
masterTLSAuthMySQL = hex "790000020da30b00ffffff00210000000000000000000000000000000000000000000000746573744d7953514c4861736b656c6c00208d6350e1245ca38229fd5cc86234ea67d2d19056795007230dbec8bd483e4b89746573744d7953514c4861736b656c6c0063616368696e675f736861325f70617373776f726400"
masterSSLRequest = hex "200000010dab0b00ffffff00210000000000000000000000000000000000000000000000"

hex :: String -> B.ByteString
hex = either error id . Hex.decode . BC.pack

parseGreeting :: B.ByteString -> IO (Greeting, ExtendedCapabilities)
parseGreeting = getFromPacket getGreetingExtended . putToPacket 0 . putByteString

wire :: Packet -> B.ByteString
wire = L.toStrict . Binary.encode

-- | Every byte one handshake sends: SSL request, response after it, and the
-- response without TLS.
handshakeBytes :: ExtendedCapabilities -> (Greeting, ExtendedCapabilities) -> (B.ByteString, B.ByteString, B.ByteString)
handshakeBytes requested (greeting, offered) =
    let negotiated = negotiateExtended requested greeting offered
        response seqN = wire (handshakeResponsePacket seqN negotiated "testMySQLHaskell" "testMySQLHaskell" "secret" 33 greeting)
    in (wire (sslRequestPacket negotiated 33), response 2, response 1)

extendedMetadata, everyCapability :: ExtendedCapabilities
extendedMetadata = ExtendedCapabilities 0x08
everyCapability = ExtendedCapabilities 0xffffffff

tests :: TestTree
tests = testGroup "handshake"
    [ testCase "greetings: MariaDB offers extended capabilities, MySQL none" $ do
        (_, mariadbOffer) <- parseGreeting mariadbGreeting
        (_, mysqlOffer) <- parseGreeting mysqlGreeting
        mariadbOffer @?= ExtendedCapabilities 0x1d
        mysqlOffer @?= noExtendedCapabilities
    , testCase "MariaDB is told apart by its greeting's capabilities, not its version string" $ do
        (mariadb, _) <- parseGreeting mariadbGreeting
        (mysql, _) <- parseGreeting mysqlGreeting
        offersExtendedCapabilities mariadb @?= True
        offersExtendedCapabilities mysql @?= False
        -- mariadbd --version=8.0.36 sends exactly this greeting.
        offersExtendedCapabilities mariadb {greetingVersion = "8.0.36"} @?= True
    , testCase "MySQL: 1.3.3's bytes whatever is requested" $ do
        greeting <- parseGreeting mysqlGreeting
        let master = (masterSSLRequest, masterTLSAuthMySQL, masterAuthMySQL)
        handshakeBytes noExtendedCapabilities greeting @?= master
        handshakeBytes extendedMetadata greeting @?= master
        handshakeBytes everyCapability greeting @?= master
    , testCase "MariaDB, nothing requested: 1.3.3's bytes" $ do
        greeting <- parseGreeting mariadbGreeting
        handshakeBytes noExtendedCapabilities greeting
            @?= (masterSSLRequest, masterTLSAuthMariaDB, masterAuthMariaDB)
    , testCase "MariaDB, a capability it does not offer: 1.3.3's bytes" $ do
        greeting <- parseGreeting mariadbGreeting
        handshakeBytes (ExtendedCapabilities 0x02) greeting
            @?= (masterSSLRequest, masterTLSAuthMariaDB, masterAuthMariaDB)
    , testCase "a server setting CLIENT_MYSQL gets 1.3.3's bytes, whatever it offers" $ do
        (greeting, offered) <- parseGreeting mariadbGreeting
        let mysqlFlagged = greeting { greetingCaps = greetingCaps greeting .|. 1 }
        handshakeBytes extendedMetadata (mysqlFlagged, offered)
            @?= (masterSSLRequest, masterTLSAuthMariaDB, masterAuthMariaDB)
    , testCase "MariaDB, extended metadata: CLIENT_MYSQL cleared, bit 35 sent" $ do
        greeting <- parseGreeting mariadbGreeting
        let (ssl, tlsAuth, auth) = handshakeBytes extendedMetadata greeting
            -- 4 header bytes, then capabilities; the extended capabilities
            -- are the last four of the 23 reserved bytes.
            expect master = B.concat
                [ B.take 4 master, hex "0ca30b00", B.take 24 (B.drop 8 master)
                , hex "08000000", B.drop 36 master ]
        auth @?= expect masterAuthMariaDB
        tlsAuth @?= expect masterTLSAuthMariaDB
        ssl @?= B.concat [B.take 4 masterSSLRequest, hex "0cab0b00", B.take 24 (B.drop 8 masterSSLRequest), hex "08000000"]
    ]
