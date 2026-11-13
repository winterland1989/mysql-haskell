module TLSServerName (tests) where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, tryTakeMVar)
import           Control.Exception          (SomeException, bracket, bracketOnError, try)
import           Control.Monad              (void)
import qualified Data.Binary                as Binary
import           Data.Binary.Put            (putByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import           Data.TLSSetting            (TrustedCAStore (CustomCAStore),
                                             makeClientParams, makeServerParams)
import           Database.MySQL.Base        (ConnectInfo (ciHost, ciPort), defaultConnectInfo)
import           Database.MySQL.Protocol.Packet (putToPacket)
import qualified Database.MySQL.TLS         as MySQLTLS
import           HandshakeGolden            (mysqlGreeting)
import qualified Network.Socket             as N
import qualified Network.Socket.ByteString  as NB
import qualified Network.TLS                as TLS
import qualified System.IO.Streams.TLS      as StreamsTLS
import           System.Timeout             (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "TLS server name indication"
    [ testCase "MySQL: a host name subject is sent, an IP literal or a caller's opt-out is not" $ do
        params <- clientParams
        sent <- traverse (mysqlSent params . fst) subjects
        sent @?= subjects
        optedOut <- mysqlServerName params { TLS.clientUseServerNameIndication = False } "db.example.test"
        optedOut @?= Nothing
    , testCase "System.IO.Streams.TLS: an IP literal host is not sent" $ do
        params <- clientParams
        sent <- traverse (streamsServerName params) [Just "db.example.test", Nothing]
        sent @?= [Just "db.example.test", Nothing]
    ]

-- | Subject names, each with the server name it should be sent as.
subjects :: [(String, Maybe TLS.HostName)]
subjects =
    [ ("db.example.test", Just "db.example.test")
    , ("127.0.0.1", Nothing)
    , ("127.0.0.1.", Nothing)
    , ("0x7f000001", Nothing)
    , ("0x7f.0.0.1", Nothing)
    , ("::1", Nothing)
    , ("[::1]", Nothing)
    ]

clientParams :: IO TLS.ClientParams
clientParams = makeClientParams (CustomCAStore "./test/cert/ca.pem")

mysqlSent :: TLS.ClientParams -> String -> IO (String, Maybe TLS.HostName)
mysqlSent params subject = (,) subject <$> mysqlServerName params subject

-- | The server name that 'MySQLTLS.connect' sends in its ClientHello when it
-- connects to 127.0.0.1.
mysqlServerName :: TLS.ClientParams -> String -> IO (Maybe TLS.HostName)
mysqlServerName params subject =
    indicatedServerName greetAsMySQL $ \ port ->
        void (MySQLTLS.connect defaultConnectInfo { ciHost = "127.0.0.1", ciPort = port } (params, subject))

-- | The server name that 'StreamsTLS.connect' sends in its ClientHello when
-- it connects to 127.0.0.1.
streamsServerName :: TLS.ClientParams -> Maybe String -> IO (Maybe TLS.HostName)
streamsServerName params subject =
    indicatedServerName (const (pure ())) $ \ port ->
        void (StreamsTLS.connect params subject "127.0.0.1" port)

-- | The server name in the ClientHello of a client that connects to the
-- given port, after @beforeTLS@ ran on the accepted socket. The client fails
-- once the server has read the ClientHello, as no real server answers it.
indicatedServerName :: (N.Socket -> IO ()) -> (N.PortNumber -> IO ()) -> IO (Maybe TLS.HostName)
indicatedServerName beforeTLS client = do
    outcome <- timeout 10000000 $ bracket listenOnLoopback N.close $ \ listener -> do
        port <- N.socketPort listener
        _ <- forkIO (void (try @SomeException (client port)))
        bracket (fst <$> N.accept listener) N.close $ \ sock -> do
            beforeTLS sock
            serverParams <- makeServerParams "./test/cert/server-cert.pem" [] "./test/cert/server-key.pem"
            indicated <- newEmptyMVar
            let hooks = (TLS.serverHooks serverParams)
                    { TLS.onServerNameIndication = \ name -> putMVar indicated name >> pure mempty }
            context <- TLS.contextNew sock serverParams { TLS.serverHooks = hooks }
            -- The client gives up on the certificate or the MySQL handshake,
            -- after the hook ran.
            _ <- try @SomeException (TLS.handshake context)
            tryTakeMVar indicated
    case outcome of
        Nothing -> assertFailure "no TLS ClientHello within 10 seconds"
        Just Nothing -> assertFailure "the TLS handshake ended before the server read a ClientHello"
        Just (Just name) -> pure name

-- | Greets as a MySQL server that offers TLS and reads the client's
-- SSLRequest, which leaves the socket at the start of the ClientHello.
greetAsMySQL :: N.Socket -> IO ()
greetAsMySQL sock = do
    NB.sendAll sock (L.toStrict (Binary.encode (putToPacket 0 (putByteString mysqlGreeting))))
    receiveExactly sock sslRequestLength

-- | A 4 byte packet header and a 32 byte payload.
sslRequestLength :: Int
sslRequestLength = 36

-- | Reads no more than it has to, so none of the ClientHello is consumed.
receiveExactly :: N.Socket -> Int -> IO ()
receiveExactly sock remaining =
    if remaining <= 0
    then pure ()
    else do
        received <- NB.recv sock remaining
        if B.null received
        then assertFailure "the client closed the connection before its SSLRequest"
        else receiveExactly sock (remaining - B.length received)

listenOnLoopback :: IO N.Socket
listenOnLoopback =
    bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $ \ sock -> do
        N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
        N.listen sock 1
        pure sock
