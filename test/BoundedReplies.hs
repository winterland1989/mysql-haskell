-- | What a server, or whoever sits between it and the client, could make the
-- client read without end: a message continued packet after packet, a column
-- count no server sends, OK packets that keep flagging another result. See
-- Note [Bounded replies] in "Database.MySQL.Connection".
module BoundedReplies (tests) where

import           Control.Exception          (Exception, try)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import           Data.Int                   (Int64)
import           Data.IORef                 (newIORef)
import           Data.Word                  (Word8)
import           Database.MySQL.Base
import           Database.MySQL.Connection  (MessageTooLarge (..), MoreResultsThanStatements (..),
                                             MySQLConn (..), ReplyState (..),
                                             decodeInputStreamWithLimits, defaultConnectionLimits,
                                             messageSizeLimit, readPacket)
import           System.IO.Streams          (InputStream)
import qualified System.IO.Streams          as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "bounded replies"
    [ testCase "endless continuation packets are refused at the message size limit" $ do
        (inputStream, bytesRead) <- packetStream 20971520 (cycle [continuationHeader, continuationBody])
        refusal <- expectException (readPacket inputStream)
        assertEqual "refusal" (MessageTooLarge 20971520 33554430) refusal
        assertEqual "bytes read: one packet and the next header" (4 + 16777215 + 4) =<< bytesRead
        again <- expectException (readPacket inputStream)
        assertEqual "a later read" (MessageTooLarge 20971520 33554430) again
        assertEqual "bytes read after the later read" (4 + 16777215 + 4) =<< bytesRead
    , testCase "a packet larger than the limit is refused before its body is read" $ do
        (inputStream, bytesRead) <- packetStream 65536 ["\x00\x00\x10\x01", B.replicate 1048576 0x61]
        refusal <- expectException (readPacket inputStream)
        assertEqual "refusal" (MessageTooLarge 65536 1048576) refusal
        assertEqual "bytes read" 4 =<< bytesRead
    , testCase "a message of exactly the limit is read, and so is the next one" $ do
        (inputStream, _) <- packetStream 16777215
            [continuationHeader, continuationBody, "\x00\x00\x00\x01", "\x03\x00\x00\x02", "abc"]
        message <- readPacket inputStream
        assertEqual "length of the message" 16777215 (pLen message)
        next <- readPacket inputStream
        assertEqual "next message" "abc" (pBody next)
    , testCase "a column count above 2^32 - 1 is refused before reading on" $ do
        let afterCount = [packet 2 ("\x03" <> "def"), packet 3 "\xFE\x00\x00\x02\x00"]
        (conn, unread) <- fakeConnection (packet 1 ("\xFE" <> "\x00\x00\x00\x00\x00\x01\x00\x00") : afterCount)
        failure <- try (queryVector_ conn "SELECT 1")
        case failure of
            Left (DecodePacketFailed _ _ _) -> pure ()
            Right _ -> assertFailure "the column count was accepted"
        assertEqual "packets left unread" afterCount =<< unread
    , testCase "executeMany_ refuses OK packets flagging more results beyond its statements" $ do
        (conn, unread) <- fakeConnection (replicate 1000 okFlaggingMore)
        refusal <- expectException (executeMany_ conn "DO 1")
        assertEqual "refusal" (MoreResultsThanStatements 1) refusal
        assertEqual "packets left unread" 998 . length =<< unread
    , testCase "queryMulti_ refuses OK packets flagging more results beyond its statements" $ do
        (conn, unread) <- fakeConnection (replicate 1000 okFlaggingMore)
        refusal <- expectException (queryMulti_ conn "DO 1; DO 2")
        assertEqual "refusal" (MoreResultsThanStatements 2) refusal
        assertEqual "packets left unread" 997 . length =<< unread
    , testCase "executeMany takes an OK for each parameter set" $ do
        (conn, _) <- fakeConnection [okFlaggingMore, okLast]
        oks <- executeMany conn "DO ?" [[MySQLInt32 1], [MySQLInt32 2]]
        assertEqual "OKs" 2 (length oks)
    ]

-- | The packets of this wire input under this 'messageSizeLimit', with the
-- count of bytes read off the input so far.
packetStream :: Int64 -> [ByteString] -> IO (InputStream Packet, IO Int64)
packetStream limit wire = do
    (bytes, bytesRead) <- Stream.fromList wire >>= Stream.countInput
    inputStream <- decodeInputStreamWithLimits defaultConnectionLimits { messageSizeLimit = limit } bytes
    pure (inputStream, bytesRead)

-- | A connection whose replies are these packets, with the packets left unread.
fakeConnection :: [Packet] -> IO (MySQLConn, IO [Packet])
fakeConnection packets = do
    inputStream <- Stream.fromList packets
    state <- newIORef RepliesRead
    pure (MySQLConn inputStream (\_ -> pure ()) (pure ()) state, Stream.toList inputStream)

expectException :: Exception e => IO a -> IO e
expectException action = do
    outcome <- try action
    case outcome of
        Left failure -> pure failure
        Right _ -> assertFailure "no exception"

continuationHeader :: ByteString
continuationHeader = "\xFF\xFF\xFF\x01"

continuationBody :: ByteString
continuationBody = B.replicate 16777215 0x61

packet :: Word8 -> L.ByteString -> Packet
packet sequenceNumber body = Packet (L.length body) sequenceNumber body

okFlaggingMore :: Packet
okFlaggingMore = packet 1 "\x00\x00\x00\x08\x00\x00\x00"

okLast :: Packet
okLast = packet 2 "\x00\x00\x00\x00\x00\x00\x00"
