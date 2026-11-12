-- | The CRC32 checksum that ends each binlog event when binlog_checksum is
-- CRC32, which the decoder used to drop unchecked, decoding a corrupted event
-- as if it were intact.
module BinLogChecksum (tests) where

import           Control.Exception                    (try)
import           Data.Binary.Put                      (putByteString, putWord16le,
                                                       putWord32le, putWord64le,
                                                       putWord8, runPut)
import           Data.Bits                            (xor)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Base16               as Base16
import qualified Data.ByteString.Lazy                 as L
import           Data.Word                            (Word32, Word64, Word8)
import           Database.MySQL.BinLog
import           Database.MySQL.BinLogProtocol.CRC32  (crc32)
import           Database.MySQL.Protocol.Packet
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Unwitch.Convert.Int64                as Int64

tests :: TestTree
tests = testGroup "binlog checksums"
    [ testCase "CRC32 matches the IEEE/zlib check values" $ do
        assertChecksum "check string" 0xCBF43926 (crc32 "123456789")
        assertChecksum "empty" 0x00000000 (crc32 "")
        assertChecksum "pangram" 0x414FA339 (crc32 "The quick brown fox jumps over the lazy dog")
        assertChecksum "every byte value" 0x29058C73 (crc32 (L.pack [0 .. 255]))
    , testCase "an intact event decodes" $ do
        xid <- mysqlXidEvent
        event <- decodeBinLogPacket True False fileName (eventPacket xid)
        assertEqual "body" (L.fromStrict (B.take 8 (B.drop 19 xid))) (blBody event)
    , testCase "an event with one byte flipped raises BinLogChecksumMismatch" $ do
        xid <- mysqlXidEvent
        decoded <- try (decodeBinLogPacket True False fileName (eventPacket (flipByte 20 xid)))
        assertEqual "mismatch"
            (Left (BinLogChecksumMismatch BINLOG_XID_EVENT fileName 3125
                (BinLogChecksum 0x73C5F461) (BinLogChecksum 0xD5B2FFD5)))
            (blEventType <$> decoded)
    , testCase "a format description event with the binlog-in-use flag set decodes" $ do
        description <- mysqlFormatDescription
        event <- decodeBinLogPacket True False fileName (eventPacket description)
        assertEqual "event type" BINLOG_FORMAT_DESCRIPTION_EVENT (blEventType event)
    , testCase "a corrupted format description event raises BinLogChecksumMismatch" $ do
        description <- mysqlFormatDescription
        decoded <- try (decodeBinLogPacket True False fileName (eventPacket (flipByte 23 description)))
        assertEqual "mismatch"
            (Left (BinLogChecksumMismatch BINLOG_FORMAT_DESCRIPTION_EVENT fileName 126
                (BinLogChecksum 0xBEB97E09) (BinLogChecksum 0xD8DB7FA5)))
            (blEventType <$> decoded)
    , testCase "a format description event naming no checksum algorithm is not checked" $ do
        description <- mysqlFormatDescription
        let algorithmOff = B.length description - 5
        event <- decodeBinLogPacket True False fileName (eventPacket (flipByte algorithmOff description))
        assertEqual "event type" BINLOG_FORMAT_DESCRIPTION_EVENT (blEventType event)
    , testCase "the semi-sync header is not part of the checksummed event" $ do
        xid <- mysqlXidEvent
        let body = L.fromStrict ("\x00\xEF\x01" <> xid)
        event <- decodeBinLogPacket True True fileName (Packet (L.length body) 1 body)
        assertEqual "semi-sync ack" True (blSemiAck event)
    , testCase "events of a dump without checksums are not checked" $ do
        xid <- mysqlXidEvent
        event <- decodeBinLogPacket False False fileName (eventPacket (flipByte 20 xid))
        assertEqual "event type" BINLOG_XID_EVENT (blEventType event)
    , followingTests
    ]

-- | A binlog file keeps the binlog_checksum it was written under, which its
-- format description event names, so one dump can read files of both.
followingTests :: TestTree
followingTests = testGroup "checksums follow the format description event"
    [ testCase "a dump from a file without checksums into one with them" $
        assertDumpDecodes BinLogChecksumCRC32
            [ (WithChecksum, rotateEvent, rotateBody "mysql-bin.000002")
            , (WithChecksum, formatDescriptionEvent, descriptionBody "8.0.45" (Just BinLogChecksumNone))
            , (WithoutChecksum, xidEvent, xidBody 1)
            , (WithoutChecksum, rotateEvent, rotateBody "mysql-bin.000003")
            -- The server makes up a rotate event before each file, by the
            -- algorithm of the file before.
            , (WithoutChecksum, rotateEvent, rotateBody "mysql-bin.000003")
            , (WithChecksum, formatDescriptionEvent, descriptionBody "8.0.45" (Just BinLogChecksumCRC32))
            , (WithChecksum, xidEvent, xidBody 2)
            ]
    , testCase "a dump from a file with checksums into one without" $
        assertDumpDecodes BinLogChecksumNone
            [ (WithoutChecksum, rotateEvent, rotateBody "mysql-bin.000001")
            , (WithChecksum, formatDescriptionEvent, descriptionBody "8.0.45" (Just BinLogChecksumCRC32))
            , (WithChecksum, xidEvent, xidBody 1)
            , (WithChecksum, rotateEvent, rotateBody "mysql-bin.000002")
            , (WithChecksum, rotateEvent, rotateBody "mysql-bin.000002")
            , (WithChecksum, formatDescriptionEvent, descriptionBody "8.0.45" (Just BinLogChecksumNone))
            , (WithoutChecksum, xidEvent, xidBody 2)
            ]
    , testCase "a corrupted event after a description naming CRC32 raises BinLogChecksumMismatch" $ do
        description <- writtenEvent WithChecksum formatDescriptionEvent
            (descriptionBody "8.0.45" (Just BinLogChecksumCRC32))
        xid <- writtenEvent WithChecksum xidEvent (xidBody 1)
        decoded <- try (decodeDump BinLogChecksumNone [description, flipByte 20 xid])
        assertEqual "mismatched event type" (Left BINLOG_XID_EVENT)
            (either (Left . mismatchEventType) (Right . map blBody) decoded)
    , testCase "a description from MySQL before 5.6.1 names no algorithm" $
        assertDumpDecodes BinLogChecksumNone
            [ (WithoutChecksum, formatDescriptionEvent, descriptionBody "5.5.62-log" Nothing)
            , (WithoutChecksum, xidEvent, xidBody 1)
            ]
    , testCase "a description from MariaDB 5.5 names its algorithm" $
        assertDumpDecodes BinLogChecksumNone
            [ (WithChecksum, formatDescriptionEvent, descriptionBody "5.5.62-MariaDB-log" (Just BinLogChecksumCRC32))
            , (WithChecksum, xidEvent, xidBody 1)
            ]
    ]

-- | Whether a server writes an event with the CRC32 of its bytes after them.
-- A format description event of a server that knows checksums always is.
data Footer = WithChecksum | WithoutChecksum

-- | Decodes the events, given the algorithm the dump announced, and expects
-- each event's body back.
assertDumpDecodes :: BinLogChecksumAlgorithm -> [(Footer, Word8, L.ByteString)] -> Assertion
assertDumpDecodes announced events = do
    written <- mapM writeEvent events
    decoded <- decodeDump announced written
    assertEqual "bodies" (map eventBody events) (map blBody decoded)

writeEvent :: (Footer, Word8, L.ByteString) -> IO B.ByteString
writeEvent (footer, typeCode, body) = writtenEvent footer typeCode body

eventBody :: (Footer, Word8, L.ByteString) -> L.ByteString
eventBody (_, _, body) = body

-- | Decodes the events in order, as 'dumpBinLog' reads its packets.
decodeDump :: BinLogChecksumAlgorithm -> [B.ByteString] -> IO [BinLogPacket]
decodeDump algorithm written = case written of
    [] -> pure []
    event : rest -> do
        (decoded, next) <- decodeBinLogPacketFollowing algorithm False fileName (eventPacket event)
        (decoded :) <$> decodeDump next rest

writtenEvent :: Footer -> Word8 -> L.ByteString -> IO B.ByteString
writtenEvent footer typeCode body = do
    let checksumLength = case footer of
            WithChecksum -> 4
            WithoutChecksum -> 0
    size <- case Int64.toWord32 (19 + L.length body + checksumLength) of
        Just size -> pure size
        Nothing -> assertFailure "a test event is too long"
    let bytes = runPut $ do
            putWord32le 0
            putWord8 typeCode
            putWord32le 1
            putWord32le size
            putWord32le 0
            putWord16le 0
    pure (L.toStrict (case footer of
        WithChecksum -> bytes <> body <> runPut (putWord32le (crc32 (bytes <> body)))
        WithoutChecksum -> bytes <> body))

rotateEvent, formatDescriptionEvent, xidEvent :: Word8
rotateEvent = 4
formatDescriptionEvent = 15
xidEvent = 16

rotateBody :: B.ByteString -> L.ByteString
rotateBody name = runPut (putWord64le 4 >> putByteString name)

xidBody :: Word64 -> L.ByteString
xidBody xid = runPut (putWord64le xid)

-- | The body of a server's format description event, which ends in the
-- algorithm it names unless the server predates checksums.
descriptionBody :: B.ByteString -> Maybe BinLogChecksumAlgorithm -> L.ByteString
descriptionBody version algorithm = runPut $ do
    putWord16le 4
    putByteString (B.take 50 (version <> B.replicate 50 0))
    putWord32le 0
    putWord8 19
    -- Post-header lengths; none of them names an algorithm.
    putByteString (B.replicate 40 8)
    mapM_ (putWord8 . algorithmCode) algorithm

algorithmCode :: BinLogChecksumAlgorithm -> Word8
algorithmCode algorithm = case algorithm of
    BinLogChecksumNone -> 0
    BinLogChecksumCRC32 -> 1

assertChecksum :: String -> Word32 -> Word32 -> Assertion
assertChecksum = assertEqual

fileName :: B.ByteString
fileName = "mysql-bin.000001"

-- | An event as COM_BINLOG_DUMP streams it, after the OK byte.
eventPacket :: B.ByteString -> Packet
eventPacket event = Packet (L.length body) 1 body
  where
    body = L.fromStrict (B.cons 0x00 event)

flipByte :: Int -> B.ByteString -> B.ByteString
flipByte index bytes =
    B.take index bytes <> B.map (`xor` 0x01) (B.take 1 (B.drop index bytes)) <> B.drop (index + 1) bytes

-- | An XID event that MySQL 8.0.45 wrote, checksum included.
mysqlXidEvent :: IO B.ByteString
mysqlXidEvent = fromHex "e8baf46a10010000001f000000350c00000000250000000000000061f4c573"

-- | The format description event that MySQL 8.0.45 wrote at the start of its
-- binlog file, with LOG_EVENT_BINLOG_IN_USE_F set as the open file holds it.
mysqlFormatDescription :: IO B.ByteString
mysqlFormatDescription = fromHex
    "e5baf46a0f010000007a0000007e00000001000400382e302e34350000000000000000\
    \000000000000000000000000000000000000000000000000000000000000000000000000\
    \e5baf46a13000d0008000000000400040000006200041a08000000080808020000000a0a\
    \0a2a2a001234000a280001097eb9be"

fromHex :: B.ByteString -> IO B.ByteString
fromHex hex = case Base16.decode hex of
    Left failure -> assertFailure ("a test event is not hex: " ++ failure)
    Right bytes -> pure bytes
