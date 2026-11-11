-- | The CRC32 checksum that ends each binlog event when binlog_checksum is
-- CRC32, which the decoder used to drop unchecked, decoding a corrupted event
-- as if it were intact.
module BinLogChecksum (tests) where

import           Control.Exception                    (try)
import           Data.Bits                            (xor)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Base16               as Base16
import qualified Data.ByteString.Lazy                 as L
import           Data.Word                            (Word32)
import           Database.MySQL.BinLog
import           Database.MySQL.BinLogProtocol.CRC32  (crc32)
import           Database.MySQL.Protocol.Packet
import           Test.Tasty
import           Test.Tasty.HUnit

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
    ]

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
