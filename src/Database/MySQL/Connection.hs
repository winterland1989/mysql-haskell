
{-|
Module      : Database.MySQL.Connection
Description : Connection managment
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This is an internal module, the 'MySQLConn' type should not directly acessed to user.

-}

module Database.MySQL.Connection
    ( -- * Connections
      MySQLConn (..)
    , ReplyState (..)
    , ConnectInfo (..)
    , defaultConnectInfo
    , defaultConnectInfoMB4
    , utf8_general_ci
    , utf8mb4_unicode_ci
    , bUFSIZE
    , connect
    , connectDetail
    , connectUnixSocket
    , connectUnixSocketDetail
    , connectDetailExtended
    , connectUnixSocketDetailExtended
      -- * Limits on what a server sends
    , ConnectionLimits
    , messageSizeLimit
    , defaultConnectionLimits
    , connectWithLimits
    , connectDetailWithLimits
    , connectDetailExtendedWithLimits
    , connectUnixSocketWithLimits
    , connectUnixSocketDetailWithLimits
    , connectUnixSocketDetailExtendedWithLimits
    , withoutNegotiated
    , authenticatedConnection
      -- * Authentication
    , mkAuth
    , handshakeResponsePacket
    , sslRequestPacket
    , scrambleForPlugin
    , scrambleSHA1
    , scrambleSHA256
    , scrambleEd25519
    , ed25519PublicKey
    , completeAuth
    , plainFullAuth
    , parseRSAPublicKey
    , xorPasswordNonce
    , doRSAAuth
    , AuthException (..)
      -- * Commands and replies
    , decodeInputStream
    , decodeInputStreamWithLimits
    , close
    , ping
    , resetConnection
    , command
    , waitCommandReply
    , waitCommandReplys
    , readPacket
    , writeCommand
    , writePayload
    , sendPacket
    , guardUnconsumed
    , runCommand
    , settleFailedExchange
    , endsReply
    , writeIORef'
      -- * Exceptions
    , NetworkException (..)
    , UnconsumedResultSet (..)
    , AbandonedReply (..)
    , ERRException (..)
    , UnexpectedPacket (..)
    , ExtraResultSets (..)
    , MessageTooLarge (..)
    , MoreResultsThanStatements (..)
    ) where

import           Control.Exception               (Exception (displayException),
                                                  bracketOnError, fromException,
                                                  mask, throwIO, catch, SomeException)
import           Control.Monad
import qualified Crypto.ECC.Edwards25519         as Ed
import           Crypto.Error                    (CryptoError, CryptoFailable, eitherCryptoError)
import qualified Crypto.Hash                     as Crypto
import           Crypto.Hash.Algorithms          (SHA1(..))
import qualified Crypto.Number.Serialize         as Serialize
import qualified Crypto.PubKey.RSA               as RSA
import qualified Crypto.PubKey.RSA.OAEP          as OAEP
import qualified Data.ASN1.BinaryEncoding        as ASN1B
import qualified Data.ASN1.BitArray              as ASN1BA
import qualified Data.ASN1.Encoding              as ASN1E
import qualified Data.ASN1.Types                 as ASN1
import qualified Data.Binary                     as Binary
import qualified Data.Binary.Put                 as Binary
import           Data.Bits
import qualified Data.ByteArray                  as BA
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as L
import qualified Data.ByteString.Unsafe          as B
import           Data.IORef                      (IORef, newIORef, readIORef,
                                                  writeIORef)
import           Data.Maybe                      (isJust)
import qualified Data.PEM                        as PEM
import           Data.Typeable
import           Data.Int                        (Int64)
import           Data.Word
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.Packet
import           Network.Socket                  (HostName, PortNumber)
import qualified Network.Socket                  as N
import           System.IO.Streams               (InputStream)
import qualified System.IO.Streams               as Stream
import qualified System.IO.Streams.TCP           as TCP
import qualified Data.Connection                 as TCP
import qualified Unwitch.Convert.Int             as Int
import qualified Unwitch.Convert.Word8           as Word8

--------------------------------------------------------------------------------

-- | 'MySQLConn' wrap both 'InputStream' and 'OutputStream' for MySQL 'Packet'.
--
-- You shouldn't use one 'MySQLConn' in different thread, if you do that,
-- consider protecting it with a @MVar@.
--
data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: (Packet -> IO ())
    ,   mysqlCloseSocket :: IO ()
    ,   replyState       :: {-# UNPACK #-} !(IORef ReplyState)
    }

-- | How far the replies to the commands sent on a connection have been read.
-- See Note [Abandoned replies].
--
-- @since 1.4.0
data ReplyState
    = RepliesRead
      -- ^ Every reply was read, so a command may be sent.
    | RowsUnread
      -- ^ A result set's rows are still to be read from its stream.
    | ReplyAbandoned
      -- ^ Reading a reply stopped part-way; the connection refuses commands.
    deriving (Show, Eq)

-- Note [Abandoned replies]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- The server answers commands in order on one socket, so a reply left unread
-- is read as the reply to the next command. A timeout around query_ used to
-- do that: interrupted before it read the column count, it left the
-- connection looking idle, and the next SELECT returned the earlier one's
-- rows (issue #10).
--
-- Decision: a command's exchange runs with the state at ReplyAbandoned until
-- its reply is read, so an exception part-way leaves the connection refusing
-- commands with AbandonedReply, and the caller reconnects. Draining the rest
-- of the reply instead would block on the server for as long as the
-- interrupted statement runs, which defeats the timeout. ERRException and
-- ExtraResultSets are only raised after a reply was read in full, so they
-- leave the connection usable, as does a DecodePacketException on a row,
-- whose packet was read before it failed to decode.

-- | Everything you need to establish a MySQL connection.
--
-- To setup a TLS connection, use module "Database.MySQL.TLS" or "Database.MySQL.OpenSSL".
--
data ConnectInfo = ConnectInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciCharset  :: Word8
    }

-- | Record syntax with the password left out, so a logged 'ConnectInfo'
-- cannot leak it.
--
-- Decision: the password shows as @<redacted>@ even when empty, so the output
-- does not tell whether the account has one either. It is not a Haskell
-- expression, unlike a placeholder string, which would read as a password.
instance Show ConnectInfo where
    showsPrec precedence (ConnectInfo host port database user _password charset) =
        showParen (precedence >= 11)
            $ showString "ConnectInfo {ciHost = " . shows host
            . showString ", ciPort = " . shows port
            . showString ", ciDatabase = " . shows database
            . showString ", ciUser = " . shows user
            . showString ", ciPassword = <redacted>"
            . showString ", ciCharset = " . shows charset
            . showChar '}'

-- | A simple 'ConnectInfo' targeting localhost with @user=root@ and empty password.
--
--  Default charset is set to @utf8_general_ci@ to support older(< 5.5.3) MySQL versions,
--  but be aware this is a partial utf8 encoding, you may want to use 'defaultConnectInfoMB4'
--  instead to support full utf8 charset(emoji, etc.). You can query your server's support
--  with @SELECT id, collation_name FROM information_schema.collations ORDER BY id;@
--
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo "127.0.0.1" 3306 "" "root" "" utf8_general_ci

-- | 'defaultConnectInfo' with charset set to @utf8mb4_unicode_ci@
--
-- This is recommanded on any MySQL server version >= 5.5.3.
--
defaultConnectInfoMB4 :: ConnectInfo
defaultConnectInfoMB4 = ConnectInfo "127.0.0.1" 3306 "" "root" "" utf8mb4_unicode_ci

utf8_general_ci :: Word8
utf8_general_ci = 33

utf8mb4_unicode_ci :: Word8
utf8mb4_unicode_ci = 224

-- | Bounds on what a server can make the client read, given to the
-- @WithLimits@ connect functions; see Note [Bounded replies]. Change a limit
-- by updating 'defaultConnectionLimits':
--
-- > defaultConnectionLimits { messageSizeLimit = 64 * 1024 * 1024 }
--
-- The constructor is not exported, so that limits can be added later.
--
-- @since 1.6.0
data ConnectionLimits = ConnectionLimits
    { messageSizeLimit :: !Int64
      -- ^ The largest message the server may send, in bytes: one packet, or
      -- one together with the packets continuing it. A larger message raises
      -- t'MessageTooLarge' before more than the limit is read of it.
    } deriving (Show, Eq)

-- | A 'messageSizeLimit' of 1 GiB plus 64 KiB, which refuses no reply a
-- server sends and no binlog event a replica accepts.
--
-- @since 1.6.0
defaultConnectionLimits :: ConnectionLimits
defaultConnectionLimits = ConnectionLimits { messageSizeLimit = 1073741824 + 65536 }

-- Note [Bounded replies]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- What the client reads is up to the server, or to whoever sits between them
-- on a connection without TLS. Three things in a reply let them decide how
-- much memory the client takes:
--
-- * A packet of 16 MiB says another continues its message, without end. The
--   packet stream refuses the packet that would take a message past
--   'messageSizeLimit', before reading its body. The default is 1 GiB plus
--   64 KiB. 1 GiB is the most max_allowed_packet can be set to on MySQL and
--   MariaDB, and MySQL's manual says the server sends a client no more than
--   it. Binlog events may exceed that by their header, so replicas accept
--   1 GiB plus MAX_LOG_EVENT_HEADER (4197 bytes on MySQL 8.0, 1192 on MariaDB
--   11.4; set_replica_max_allowed_packet in MySQL's sql/rpl_replica.cc,
--   set_slave_max_allowed_packet in MariaDB's sql/slave.cc), and 64 KiB covers
--   that with room to spare.
--
-- * A column count comes before the column definitions. A count above
--   2^32 - 1, which neither server can send as both hold it in an unsigned
--   int, is refused, and the definitions are read one at a time instead of
--   into a vector of the count's size.
--
-- * The functions returning a whole reply keep its results while the server
--   flags another one (SERVER_MORE_RESULTS_EXISTS). A statement gives at most
--   one OK packet and a query holds at most one statement more than it has
--   semicolons, so more OK packets than that raise
--   'MoreResultsThanStatements'. The execute functions skip a result set
--   instead of keeping it, as they raise 'ExtraResultSets' for it anyway.
--   The rows queryMulti_ returns, and the result sets a CALL returns, are not
--   bounded: no count follows from the query, and a limit set at connect
--   time cannot reach them.
--
-- Decision: the message limit lives in the packet stream, as 'MySQLConn' is
-- an exported record a new field would break. For the same reason the other
-- bounds are fixed rather than part of 'ConnectionLimits'.

--------------------------------------------------------------------------------

-- | Socket buffer size.
--
-- maybe exposed to 'ConnectInfo' laster?
--
bUFSIZE :: Int
bUFSIZE = 16384

-- | Establish a MySQL connection.
--
connect :: ConnectInfo -> IO MySQLConn
connect = connectWithLimits defaultConnectionLimits

-- | Establish a MySQL connection with 'Greeting' back, so you can find server's version .etc.
--
connectDetail :: ConnectInfo -> IO (Greeting, MySQLConn)
connectDetail = connectDetailWithLimits defaultConnectionLimits

-- | 'connectDetail' asking a MariaDB server for extended capabilities, also
-- returning those the server agreed to. Asking for none gives exactly
-- 'connectDetail'. See Note [MariaDB extended capabilities].
--
-- @since 1.4.0
connectDetailExtended :: ExtendedCapabilities -> ConnectInfo -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectDetailExtended = connectDetailExtendedWithLimits defaultConnectionLimits

-- | Connect to MySQL via a Unix domain socket.
--
connectUnixSocket :: FilePath -> ConnectInfo -> IO MySQLConn
connectUnixSocket = connectUnixSocketWithLimits defaultConnectionLimits

-- | Connect to MySQL via a Unix domain socket with 'Greeting' back,
-- so you can find server's version .etc.
--
connectUnixSocketDetail :: FilePath -> ConnectInfo -> IO (Greeting, MySQLConn)
connectUnixSocketDetail = connectUnixSocketDetailWithLimits defaultConnectionLimits

-- | 'connectUnixSocketDetail' asking for extended capabilities, as
-- 'connectDetailExtended' does.
--
-- @since 1.4.0
connectUnixSocketDetailExtended :: ExtendedCapabilities -> FilePath -> ConnectInfo
                                -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectUnixSocketDetailExtended = connectUnixSocketDetailExtendedWithLimits defaultConnectionLimits

-- | 'connect' refusing what the server sends beyond these limits.
--
-- > conn <- connectWithLimits defaultConnectionLimits { messageSizeLimit = 64 * 1024 * 1024 } defaultConnectInfo
--
-- @since 1.6.0
connectWithLimits :: ConnectionLimits -> ConnectInfo -> IO MySQLConn
connectWithLimits limits = fmap snd . connectDetailWithLimits limits

-- | 'connectDetail' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectDetailWithLimits :: ConnectionLimits -> ConnectInfo -> IO (Greeting, MySQLConn)
connectDetailWithLimits limits =
    fmap withoutNegotiated . connectDetailExtendedWithLimits limits noExtendedCapabilities

-- | 'connectDetailExtended' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectDetailExtendedWithLimits :: ConnectionLimits -> ExtendedCapabilities -> ConnectInfo
                                -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectDetailExtendedWithLimits limits requested connectInfo =
    bracketOnError
        (TCP.connectSocket (ciHost connectInfo) (ciPort connectInfo) >>= TCP.socketToConnection bUFSIZE)
        TCP.close
        (handshakePlain limits requested connectInfo)

-- | 'connectUnixSocket' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectUnixSocketWithLimits :: ConnectionLimits -> FilePath -> ConnectInfo -> IO MySQLConn
connectUnixSocketWithLimits limits socketPath = fmap snd . connectUnixSocketDetailWithLimits limits socketPath

-- | 'connectUnixSocketDetail' refusing what the server sends beyond these limits.
--
-- @since 1.6.0
connectUnixSocketDetailWithLimits :: ConnectionLimits -> FilePath -> ConnectInfo -> IO (Greeting, MySQLConn)
connectUnixSocketDetailWithLimits limits socketPath =
    fmap withoutNegotiated . connectUnixSocketDetailExtendedWithLimits limits noExtendedCapabilities socketPath

-- | 'connectUnixSocketDetailExtended' refusing what the server sends beyond
-- these limits.
--
-- @since 1.6.0
connectUnixSocketDetailExtendedWithLimits :: ConnectionLimits -> ExtendedCapabilities -> FilePath -> ConnectInfo
                                          -> IO (Greeting, ExtendedCapabilities, MySQLConn)
connectUnixSocketDetailExtendedWithLimits limits requested socketPath connectInfo =
    bracketOnError (openUnixSocket socketPath) TCP.close (handshakePlain limits requested connectInfo)

openUnixSocket :: FilePath -> IO TCP.TCPConnection
openUnixSocket socketPath = bracketOnError
    (N.socket N.AF_UNIX N.Stream 0)
    N.close
    (\sock -> do
        N.connect sock (N.SockAddrUnix socketPath)
        TCP.socketToConnection bUFSIZE (sock, N.SockAddrUnix socketPath)
    )

withoutNegotiated :: (Greeting, ExtendedCapabilities, MySQLConn) -> (Greeting, MySQLConn)
withoutNegotiated (greet, _, conn) = (greet, conn)

-- | Authenticate over a connection without TLS, TCP or Unix socket alike.
handshakePlain :: ConnectionLimits -> ExtendedCapabilities -> ConnectInfo -> TCP.TCPConnection
               -> IO (Greeting, ExtendedCapabilities, MySQLConn)
handshakePlain limits requested (ConnectInfo _host _port db user pass charset) c = do
    is' <- decodeInputStreamWithLimits limits (TCP.source c)
    p <- readPacket is'
    (greet, offered) <- getFromPacket getGreetingExtended p
    let salt = greetingSalt1 greet `B.append` greetingSalt2 greet
        negotiated = negotiateExtended requested greet offered
    sendPacket c (handshakeResponsePacket 1 negotiated db user pass charset greet)
    q <- readPacket is'
    completeAuth is' (sendPacket c) pass q (plainFullAuth salt)
    conn <- authenticatedConnection is' c
    return (greet, negotiated, conn)

-- | The 'MySQLConn' over a connection whose handshake completed.
--
-- @since 1.4.0
authenticatedConnection :: InputStream Packet -> TCP.Connection a -> IO MySQLConn
authenticatedConnection is' c = do
    state <- newIORef RepliesRead
    return $ MySQLConn is'
        (sendPacket c)
        (writeCommand COM_QUIT (sendPacket c) >> quitReply is' >> TCP.close c)
        state

-- | After COM_QUIT the server either replies an OK packet or closes the
-- connection, so a failed read is not an error here.
quitReply :: InputStream Packet -> IO ()
quitReply is' = catch
    (void (waitCommandReply is'))
    ((\_ -> return ()) :: SomeException -> IO ())

sendPacket :: TCP.Connection a -> Packet -> IO ()
sendPacket c = TCP.send c . Binary.runPut . Binary.put

-- | The handshake response for a connection using the negotiated extended
-- capabilities, numbered @seqN@ (1 after a plain greeting, 2 after an SSL
-- request). With 'noExtendedCapabilities' it is the packet mysql-haskell sent
-- before extended capabilities existed.
--
-- @since 1.4.0
handshakeResponsePacket :: Word8 -> ExtendedCapabilities -> ByteString -> ByteString -> ByteString
                        -> Word8 -> Greeting -> Packet
handshakeResponsePacket seqN negotiated db user pass charset greet =
    putToPacket seqN (putAuthExtended negotiated (mkAuthFor negotiated db user pass charset greet))

-- | The SSL request for a connection using the negotiated extended
-- capabilities, as 'handshakeResponsePacket'.
--
-- @since 1.4.0
sslRequestPacket :: ExtendedCapabilities -> Word8 -> Packet
sslRequestPacket negotiated charset =
    putToPacket 1 (putSSLRequestExtended negotiated (sslRequestFor negotiated charset))

mkAuth :: ByteString -> ByteString -> ByteString -> Word8 -> Greeting -> Auth
mkAuth = mkAuthFor noExtendedCapabilities

-- | 'mkAuth' for a connection using these extended capabilities.
--
-- @since 1.4.0
mkAuthFor :: ExtendedCapabilities -> ByteString -> ByteString -> ByteString -> Word8 -> Greeting -> Auth
mkAuthFor negotiated db user pass charset greet =
    let salt = greetingSalt1 greet `B.append` greetingSalt2 greet
        plugin = greetingAuthPlugin greet
        scambleBuf = scrambleForPlugin plugin salt pass
    in Auth (clientCapFor negotiated) clientMaxPacketSize charset user scambleBuf db plugin

-- | Dispatch scramble based on the authentication plugin name.
scrambleForPlugin :: ByteString -> ByteString -> ByteString -> ByteString
scrambleForPlugin plugin salt pass
    | plugin == "caching_sha2_password" = scrambleSHA256 salt pass
    | otherwise                         = scrambleSHA1 salt pass

-- | The reply to the salt of an AuthSwitchRequest for this plugin.
--
-- Decision: client_ed25519 is dispatched here rather than in
-- 'scrambleForPlugin', whose exported type has no room for the signer's
-- failure. Nothing is lost: MariaDB always greets with mysql_native_password
-- and asks for client_ed25519 only through an AuthSwitchRequest.
switchScramble :: ByteString -> ByteString -> ByteString -> Either CryptoError ByteString
switchScramble plugin salt pass = if
    | plugin == ed25519Plugin -> scrambleEd25519 salt pass
    | otherwise               -> Right (scrambleForPlugin plugin salt pass)

-- | The client plugin a MariaDB account @IDENTIFIED VIA ed25519@ switches to.
ed25519Plugin :: ByteString
ed25519Plugin = "client_ed25519"

-- | The nonce MariaDB's ed25519 plugin sends; libmariadb refuses any other
-- length.
ed25519NonceLength :: Int
ed25519NonceLength = 32

-- | The @client_ed25519@ reply to a nonce: an Ed25519 signature of the nonce,
-- valid for the key 'ed25519PublicKey' derives from the password. An empty
-- password is signed like any other, as libmariadb does. The 'Left' cannot
-- occur: crypton decodes a scalar from up to 64 bytes, and every scalar here
-- comes from a SHA-512 digest.
--
-- @since 1.5.0
scrambleEd25519 :: ByteString -> ByteString -> Either CryptoError ByteString
scrambleEd25519 nonce pass = eitherCryptoError $ do
    let digest = sha512 pass
    secret <- secretScalar digest
    ephemeral <- Ed.scalarDecodeLong (sha512 (B.drop 32 digest <> nonce))
    let commitment = Ed.pointEncode (Ed.toPoint ephemeral) :: ByteString
    challenge <- Ed.scalarDecodeLong (sha512 (B.concat [commitment, publicKey secret, nonce]))
    pure (commitment <> Ed.scalarEncode (Ed.scalarAdd ephemeral (Ed.scalarMul challenge secret)))

-- | The 32-byte public key MariaDB stores for an ed25519 account with this
-- password; @ed25519_password@ and @mysql.global_priv@ show it base64 encoded
-- without the padding. The 'Left' does not occur, as for 'scrambleEd25519'.
--
-- @since 1.5.0
ed25519PublicKey :: ByteString -> Either CryptoError ByteString
ed25519PublicKey pass = eitherCryptoError (publicKey <$> secretScalar (sha512 pass))

-- | The secret scalar of a password's SHA-512 digest. MariaDB's plugin
-- departs from Ed25519 only here: Ed25519 hashes a 32-byte seed, MariaDB the
-- whole password.
secretScalar :: ByteString -> CryptoFailable Ed.Scalar
secretScalar digest = Ed.scalarDecodeLong (B.pack (zipWith clampByte [0 ..] (B.unpack (B.take 32 digest))))

-- | Ed25519's clamping of the secret scalar's bytes, as MariaDB's ref10
-- @crypto_sign_keypair@ does it.
clampByte :: Int -> Word8 -> Word8
clampByte index byte = if
    | index == 0  -> byte .&. 248
    | index == 31 -> (byte .&. 63) .|. 64
    | otherwise   -> byte

publicKey :: Ed.Scalar -> ByteString
publicKey = Ed.pointEncode . Ed.toPoint

sha512 :: ByteString -> ByteString
sha512 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA512)

-- | SHA1-based scramble for @mysql_native_password@.
scrambleSHA1 :: ByteString -> ByteString -> ByteString
scrambleSHA1 salt pass
    | B.null pass = B.empty
    | otherwise   = B.pack (B.zipWith xor sha1pass withSalt)
    where sha1pass = sha1 pass
          withSalt = sha1 (salt `B.append` sha1 sha1pass)
          sha1 :: ByteString -> ByteString
          sha1 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA1)

-- | SHA256-based scramble for @caching_sha2_password@.
-- XOR(SHA256(password), SHA256(SHA256(SHA256(password)) + nonce))
scrambleSHA256 :: ByteString -> ByteString -> ByteString
scrambleSHA256 salt pass
    | B.null pass = B.empty
    | otherwise   = B.pack (B.zipWith xor sha256pass withSalt)
    where sha256pass = sha256 pass
          withSalt   = sha256 (sha256 sha256pass `B.append` salt)
          sha256 :: ByteString -> ByteString
          sha256 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA256)

-- | Handle multi-step authentication after sending the initial auth response.
--
-- This handles OK, ERR, AuthMoreData (0x01), and AuthSwitchRequest (0xFE).
-- The @fullAuth@ callback is invoked when the server requests full authentication
-- (e.g., cleartext password over TLS).
completeAuth :: InputStream Packet       -- ^ packet input stream
             -> (Packet -> IO ())        -- ^ packet writer
             -> ByteString               -- ^ password
             -> Packet                   -- ^ the first response packet from server
             -> (Word8 -> ByteString -> (Packet -> IO ()) -> InputStream Packet -> IO ())
                                         -- ^ full auth callback (seqN, password, writer, input)
             -> IO ()
completeAuth is writePacket pass p fullAuth
    | isOK p  = return ()
    | isERR p = decodeFromPacket p >>= throwIO . ERRException
    | isAuthMoreData p = do
        let body = L.toStrict (pBody p)
        case B.index body 1 of
            0x03 -> do  -- fast auth success, read the final OK
                ok <- readPacket is
                if isOK ok
                    then return ()
                    else decodeFromPacket ok >>= throwIO . ERRException
            0x04 -> do  -- full auth required
                fullAuth (pSeqN p + 1) pass writePacket is
            _    -> throwIO (UnexpectedPacket p)
    | isAuthSwitch p = do
        -- Parse AuthSwitchRequest: 0xFE, plugin name (NUL), salt
        let body = L.toStrict (pBody p)
            rest = B.drop 1 body  -- skip 0xFE
            (newPlugin, rest') = B.break (== 0) rest
            newSalt' = switchSalt newPlugin (B.drop 1 rest')
            seqN = pSeqN p + 1
        when (newPlugin == ed25519Plugin && B.length newSalt' /= ed25519NonceLength)
            (throwIO (UnexpectedPacket p))
        scrambled <- either (throwIO . AuthException . ("signing the client_ed25519 nonce failed: " ++) . show)
            pure (switchScramble newPlugin newSalt' pass)
        let responseBody = L.fromStrict scrambled
            responsePacket = Packet (fromIntegral (B.length scrambled)) seqN responseBody
        writePacket responsePacket
        q <- readPacket is
        completeAuth is writePacket pass q fullAuth
    | otherwise = throwIO (UnexpectedPacket p)

-- | The salt in the plugin data of an AuthSwitchRequest. The native and
-- SHA-256 scrambles may end in a NUL that is no part of the salt, but
-- MariaDB's ed25519 nonce is random bytes, so a last byte of 0 belongs to it.
switchSalt :: ByteString -> ByteString -> ByteString
switchSalt plugin pluginData = if
    | plugin == ed25519Plugin -> pluginData
    | otherwise -> case B.unsnoc pluginData of
        Just (salt, lastByte) -> if lastByte == 0 then salt else pluginData
        Nothing               -> pluginData

-- | Full auth handler for plain TCP connections using RSA encryption.
--
-- The nonce is captured via partial application at the call site:
-- @completeAuth is writePacket pass q (plainFullAuth salt)@
plainFullAuth :: ByteString -> Word8 -> ByteString -> (Packet -> IO ()) -> InputStream Packet -> IO ()
plainFullAuth nonce seqN pass writePacket is = do
    -- Request server's RSA public key
    writePacket (putToPacket seqN (Binary.putWord8 0x02))
    keyPkt <- readPacket is
    -- Key packet body: 0x01 <PEM bytes>
    let pemKey = L.toStrict (L.drop 1 (pBody keyPkt))
    doRSAAuth writePacket (pSeqN keyPkt + 1) pass nonce pemKey
    q <- readPacket is
    if isOK q then pure ()
              else decodeFromPacket q >>= throwIO . ERRException

-- | Parse a PEM-encoded SubjectPublicKeyInfo RSA public key (PKCS#8 / RFC 5480).
parseRSAPublicKey :: ByteString -> Either String RSA.PublicKey
parseRSAPublicKey pemBytes =
    either (Left . show) Right (PEM.pemParseBS pemBytes)
    >>= firstPEM
    >>= decodeDER
    >>= spkiBitString
    >>= decodeDER
    >>= rsaKey
  where
    firstPEM []    = Left "empty PEM"
    firstPEM (x:_) = Right (PEM.pemContent x)

    decodeDER bs = either (Left . show) Right (ASN1E.decodeASN1' ASN1B.DER bs)

    spkiBitString (ASN1.Start ASN1.Sequence
                   : ASN1.Start ASN1.Sequence
                   : ASN1.OID _
                   : ASN1.Null
                   : ASN1.End ASN1.Sequence
                   : ASN1.BitString bs
                   : ASN1.End ASN1.Sequence
                   : [])
        = Right (ASN1BA.bitArrayGetData bs)
    spkiBitString xs = Left ("unexpected SPKI ASN1: " ++ show xs)

    rsaKey (ASN1.Start ASN1.Sequence
            : ASN1.IntVal n
            : ASN1.IntVal e
            : ASN1.End ASN1.Sequence
            : [])
        = Right RSA.PublicKey
            { RSA.public_size = B.length (Serialize.i2osp n :: ByteString)
            , RSA.public_n    = n
            , RSA.public_e    = e
            }
    rsaKey xs = Left ("unexpected RSA key ASN1: " ++ show xs)

-- | XOR @(password ++ NUL)@ with a cyclically-extended nonce.
xorPasswordNonce :: ByteString -> ByteString -> ByteString
xorPasswordNonce pass nonce =
    let msg      = pass `B.snoc` 0x00
        nonceLen = B.length nonce
        cycled   = B.pack [nonce `B.index` (i `mod` nonceLen) | i <- [0 .. B.length msg - 1]]
    in B.pack (B.zipWith xor msg cycled)

-- | Encrypt password with the server's RSA public key and send it.
doRSAAuth :: (Packet -> IO ()) -> Word8 -> ByteString -> ByteString -> ByteString -> IO ()
doRSAAuth writePacket seqN pass nonce pemKey = do
    pubKey <- either (\e -> throwIO (userError ("RSA key parse: " ++ e))) pure
                     (parseRSAPublicKey pemKey)
    result <- OAEP.encrypt (OAEP.defaultOAEPParams SHA1) pubKey
                           (xorPasswordNonce pass nonce)
    ct <- either (\e -> throwIO (userError ("RSA encrypt: " ++ show e))) pure result
    writePacket (putToPacket seqN (Binary.putByteString ct))

data AuthException = AuthException String deriving (Typeable, Show)
instance Exception AuthException

-- | 'decodeInputStreamWithLimits' with 'defaultConnectionLimits'.
decodeInputStream :: InputStream ByteString -> IO (InputStream Packet)
decodeInputStream = decodeInputStreamWithLimits defaultConnectionLimits

-- | The packets of a connection's byte stream. A message larger than the
-- 'messageSizeLimit' raises t'MessageTooLarge' before more than the limit is
-- read of it, and so does every read after that, as the message's unread rest
-- would be taken for packets.
--
-- @since 1.6.0
decodeInputStreamWithLimits :: ConnectionLimits -> InputStream ByteString -> IO (InputStream Packet)
decodeInputStreamWithLimits limits inputStream = do
    progress <- newIORef (MessageSoFar 0)
    Stream.makeInputStream (readBoundedPacket (messageSizeLimit limits) progress inputStream)

-- | How far the packet stream is into the message it reads.
data MessageProgress
    = MessageSoFar !Int64
      -- ^ The bytes of the message read before its next packet; 0 between messages.
    | MessageRefused !MessageTooLarge

readBoundedPacket :: Int64 -> IORef MessageProgress -> InputStream ByteString -> IO (Maybe Packet)
readBoundedPacket limit progress inputStream = do
    current <- readIORef progress
    case current of
        MessageRefused refusal -> throwIO refusal
        MessageSoFar soFar -> do
            header <- Stream.readExactly 4 inputStream
            let bodyLength = packetBodyLength header
                len = Int.toInt64 bodyLength
            -- Compared as a difference, as the sum could overflow.
            if len > limit - soFar
            then do
                let refusal = MessageTooLarge limit (saturatingAdd soFar len)
                writeIORef progress (MessageRefused refusal)
                throwIO refusal
            else do
                body <- readPacketBody [] bodyLength inputStream
                let next = if len == continuedPacketLength then soFar + len else 0
                unless (next == soFar) (writeIORef progress (MessageSoFar next))
                pure (Just (Packet len (header `B.unsafeIndex` 3) body))

-- | The length of a packet whose message continues in the next packet.
continuedPacketLength :: Int64
continuedPacketLength = 16777215

packetBodyLength :: ByteString -> Int
packetBodyLength header =
        Word8.toInt (header `B.unsafeIndex` 0)
    .|. Word8.toInt (header `B.unsafeIndex` 1) `shiftL` 8
    .|. Word8.toInt (header `B.unsafeIndex` 2) `shiftL` 16

saturatingAdd :: Int64 -> Int64 -> Int64
saturatingAdd augend addend = if augend > maxBound - addend then maxBound else augend + addend

-- | The next @remaining@ bytes, after the chunks already read in reverse.
readPacketBody :: [ByteString] -> Int -> InputStream ByteString -> IO L.ByteString
readPacketBody earlierChunks remaining inputStream = if remaining == 0
    then return $! L.fromChunks (reverse earlierChunks)
    else do
        next <- Stream.read inputStream
        case next of
            Nothing -> throwIO NetworkException
            Just chunk -> if B.length chunk >= remaining
                then do
                    let (lastChunk, rest) = B.splitAt remaining chunk
                    unless (B.null rest) (Stream.unRead rest inputStream)
                    return $! L.fromChunks (reverse (lastChunk : earlierChunks))
                else do
                    let remaining' = remaining - B.length chunk
                    remaining' `seq` readPacketBody (chunk : earlierChunks) remaining' inputStream

-- | Close a MySQL connection.
--
close :: MySQLConn -> IO ()
close (MySQLConn _ _ closeSocket _) = closeSocket

-- | Send a 'COM_PING'.
--
ping :: MySQLConn -> IO OK
ping = flip command COM_PING

-- | Give the connection's session the state of a new one without reconnecting
-- or authenticating again, so a pool can hand the connection to its next user.
-- It sends COM_RESET_CONNECTION, which needs MySQL 5.7.3 or MariaDB 10.2.4; an
-- older server's refusal raises 'ERRException'.
--
-- The session stays the same one (@CONNECTION_ID()@ and the user do not
-- change), and the server
--
--   * rolls back an open transaction, and releases @LOCK TABLES@ locks and
--     @GET_LOCK@ locks;
--   * drops the session's temporary tables;
--   * sets every user variable to @NULL@;
--   * deallocates every prepared statement, so executing one of their
--     'StmtID's afterwards raises 'ERRException' with code 1243
--     (@ER_UNKNOWN_STMT_HANDLER@), while 'closeStmt' on one is harmless;
--   * sets the session variables, such as @sql_mode@, @time_zone@,
--     @autocommit@ and the transaction isolation level, to their global
--     values, and @LAST_INSERT_ID()@ to 0.
--
-- The current database stays, also one chosen with @USE@ after connecting.
--
-- Two things differ from a new connection, measured on MySQL 8.0.45 and
-- MariaDB 11.4.9. The session's @sql_mode@ loses the @IGNORE_SPACE@ that the
-- handshake adds, so a query with a space after a function name, as in
-- @COUNT (*)@, becomes a syntax error. And MariaDB returns to the character set
-- and collation of 'ciCharset', where MySQL takes the server's global
-- @character_set_client@ and @collation_connection@ (on 8.0 @utf8mb4@ with
-- @utf8mb4_0900_ai_ci@, whichever 'ciCharset' the connection was made with);
-- send @SET NAMES@ after the reset to keep 'ciCharset' on MySQL.
--
-- Like other commands it refuses with 'UnconsumedResultSet' while a result
-- set's rows are unread, and with 'AbandonedReply' after an interrupted reply,
-- whose unread rest would be taken for the reset's reply: such a connection
-- has to be closed, not reset.
--
-- @since 1.5.0
resetConnection :: MySQLConn -> IO ()
resetConnection conn@(MySQLConn is os _ _) = runCommand conn $ do
    writeResetConnection os
    void (waitCommandReply is)

--------------------------------------------------------------------------------
-- helpers

-- | Send a 'Command' which don't return a resultSet.
--
command :: MySQLConn -> Command -> IO OK
command conn@(MySQLConn is os _ _) cmd = runCommand conn $ do
    writeCommand cmd os
    waitCommandReply is
{-# INLINE command #-}

waitCommandReply :: InputStream Packet -> IO OK
waitCommandReply is = do
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> decodeFromPacket p
        | otherwise -> throwIO (UnexpectedPacket p)
{-# INLINE waitCommandReply #-}

waitCommandReplys :: InputStream Packet -> IO [OK]
waitCommandReplys is = do
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> do ok <- decodeFromPacket p
                        if isThereMore ok
                        then (ok :) <$> waitCommandReplys is
                        else return [ok]
        | otherwise -> throwIO (UnexpectedPacket p)
{-# INLINE waitCommandReplys #-}

readPacket :: InputStream Packet -> IO Packet
readPacket is = Stream.read is >>= maybe
    (throwIO NetworkException)
    (\ p@(Packet len _ bs) -> if len < 16777215 then return p else go len [bs])
  where
    go len acc = Stream.read is >>= maybe
        (throwIO NetworkException)
        (\ (Packet len' seqN bs) -> do
            let len'' = len + len'
                acc' = bs:acc
            if len' < 16777215
            then return (Packet len'' seqN (L.concat . reverse $ acc'))
            else len'' `seq` go len'' acc'
        )
{-# INLINE readPacket #-}

writeCommand :: Command -> (Packet -> IO ()) -> IO ()
writeCommand a = writePayload (Binary.runPut (putCommand a))
{-# INLINE writeCommand #-}

-- Decision: COM_RESET_CONNECTION (0x1F) is written here instead of as a
-- 'Command' constructor, because 'Command' is exported and a new constructor
-- would break exhaustive matches on it in code using this library.
writeResetConnection :: (Packet -> IO ()) -> IO ()
writeResetConnection = writePayload (Binary.runPut (Binary.putWord8 0x1F))

-- | Send one command's payload, split into packets as the protocol requires
-- for payloads of 16 MiB and more.
--
-- @since 1.4.0
writePayload :: L.ByteString -> (Packet -> IO ()) -> IO ()
writePayload bs = writePayloadFrom 0 (L.length bs) bs
{-# INLINE writePayload #-}

writePayloadFrom :: Word8 -> Int64 -> L.ByteString -> (Packet -> IO ()) -> IO ()
writePayloadFrom seqN len bs writePacket =
    if len < 16777215
    then writePacket (Packet len seqN bs)
    else do
        let (bs', rest) = L.splitAt 16777215 bs
        writePacket (Packet 16777215 seqN bs')
        writePayloadFrom (seqN + 1) (len - 16777215) rest writePacket

-- | Throws unless a command may be sent, see 'ReplyState'.
guardUnconsumed :: MySQLConn -> IO ()
guardUnconsumed (MySQLConn _ _ _ state) = do
    current <- readIORef state
    case current of
        RepliesRead -> pure ()
        RowsUnread -> throwIO UnconsumedResultSet
        ReplyAbandoned -> throwIO AbandonedReply
{-# INLINE guardUnconsumed #-}

-- | Send a command and read its reply, the @exchange@, leaving the connection
-- at 'ReplyAbandoned' if an exception stops it part-way. An exchange that hands
-- out a row stream sets 'RowsUnread' itself. See Note [Abandoned replies].
runCommand :: MySQLConn -> IO a -> IO a
runCommand conn@(MySQLConn _ _ _ state) exchange = mask $ \restore -> do
    guardUnconsumed conn
    writeIORef state ReplyAbandoned
    result <- restore exchange `catch` settleFailedExchange state
    settled <- readIORef state
    when (settled == ReplyAbandoned) (writeIORef state RepliesRead)
    pure result

settleFailedExchange :: IORef ReplyState -> SomeException -> IO a
settleFailedExchange state failure = do
    when (endsReply failure) (writeIORef state RepliesRead)
    throwIO failure

-- | Raised only once the reply it came in was read to the end.
endsReply :: SomeException -> Bool
endsReply failure =
    isJust (fromException failure :: Maybe ERRException)
    || isJust (fromException failure :: Maybe ExtraResultSets)

writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref x = x `seq` writeIORef ref x
{-# INLINE writeIORef' #-}

--------------------------------------------------------------------------------
-- Exceptions

data NetworkException = NetworkException deriving (Typeable, Show)
instance Exception NetworkException

data UnconsumedResultSet = UnconsumedResultSet deriving (Typeable, Show)
instance Exception UnconsumedResultSet

-- | Reading the reply to an earlier command on this connection stopped part-way,
-- for example on a 'System.Timeout.timeout'. The rest of that reply is still
-- unread and would be taken for the reply to the next command, so the
-- connection refuses commands: close it and connect again.
--
-- @since 1.4.0
data AbandonedReply = AbandonedReply deriving (Typeable, Show)
instance Exception AbandonedReply where
    displayException AbandonedReply =
        "mysql-haskell: reading the reply to an earlier command on this connection stopped "
        ++ "part-way (for example on a timeout), so the rest of it is still unread and "
        ++ "would be taken for this command's reply. Close the connection and connect again."

data ERRException = ERRException ERR deriving (Typeable, Show)
instance Exception ERRException

data UnexpectedPacket = UnexpectedPacket Packet deriving (Typeable, Show)
instance Exception UnexpectedPacket

-- | A statement produced a result set the function running it could not
-- return: @execute@ and @executeStmt@ return none, the query functions only the
-- first. It was read and discarded, so the connection is still usable.
--
-- @since 1.3.2
data ExtraResultSets = ExtraResultSets deriving (Typeable, Show)
instance Exception ExtraResultSets where
    displayException ExtraResultSets =
        "mysql-haskell: the statement produced a result set this function could not "
        ++ "return (execute_ and executeStmt return none, query_ and queryStmt only the "
        ++ "first). It was read and discarded, so the connection is still usable. Run "
        ++ "statements that return rows through a query function, one result set per call."

-- | The server sent a message larger than the connection's 'messageSizeLimit':
-- the limit, and the size the message has at least, the bytes of its packets
-- read so far and the length its next packet announced. It was refused before
-- more than the limit was read, so the rest of it is still unread and the
-- connection refuses commands with 'AbandonedReply' or this exception: close
-- it, and connect with a larger limit if the message was expected. See
-- Note [Bounded replies].
--
-- @since 1.6.0
data MessageTooLarge = MessageTooLarge !Int64 !Int64
    deriving (Typeable, Show, Eq)
instance Exception MessageTooLarge where
    displayException (MessageTooLarge limit size) =
        "mysql-haskell: the server sent a message of at least " ++ show size ++ " bytes, more "
        ++ "than the connection's messageSizeLimit of " ++ show limit ++ " bytes, so it was not "
        ++ "read. Its unread rest would be taken for the next reply: close the connection, and "
        ++ "connect with a larger messageSizeLimit if such a message is expected."

-- | The server sent more OK packets in reply to a query than the query holds
-- statements, the most of which is carried. Neither MySQL nor MariaDB sends
-- more than one per statement. Reading stopped at the first one too many, so
-- the connection refuses commands with 'AbandonedReply': close it. See
-- Note [Bounded replies].
--
-- @since 1.6.0
newtype MoreResultsThanStatements = MoreResultsThanStatements Int64
    deriving (Typeable, Show, Eq)
instance Exception MoreResultsThanStatements where
    displayException (MoreResultsThanStatements statements) =
        "mysql-haskell: the server sent more OK packets than the query's at most "
        ++ show statements ++ " statements produce, one per statement. Reading stopped "
        ++ "there, so the rest of the reply is unread: close the connection."

