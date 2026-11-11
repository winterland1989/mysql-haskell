-- | Binlog events of a type this library has no constructor for, such as
-- MySQL 8's compressed TRANSACTION_PAYLOAD_EVENT (40) or MariaDB's GTID
-- event (162), whose type code used to crash the decoder through toEnum.
module BinLogEventCodes (tests) where

import           Control.Exception              (try)
import qualified Data.ByteString.Lazy           as L
import           Data.Binary.Put                (runPut, putWord8, putWord16le, putWord32le, putLazyByteString)
import           Data.IORef                     (newIORef)
import           Data.Word                      (Word16, Word8)
import           Database.MySQL.BinLog
import           Database.MySQL.Protocol.Packet
import qualified System.IO.Streams              as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "binlog event codes"
    [ testCase "an event of an unknown type keeps its type code" $ do
        event <- getFromPacket (getBinLogPacket False False) (eventPacket 162 notIgnorable)
        assertEqual "type code" 162 (binLogEventTypeCode (blEventType event))
    , testCase "the row decoder refuses an unknown event not marked ignorable" $ do
        rows <- decodeEvents [eventPacket 40 notIgnorable]
        decoded <- try (Stream.read rows)
        case decoded of
            Left (UnsupportedEventType code _) -> assertEqual "type code" 40 code
            Left other -> assertFailure ("raised " <> show other)
            Right event -> assertFailure ("decoded " <> show event)
    , testCase "the row decoder skips an unknown event marked ignorable" $ do
        rows <- decodeEvents [eventPacket 40 ignorable]
        decoded <- Stream.read rows
        assertEqual "row events" Nothing decoded
    ]

decodeEvents :: [Packet] -> IO (Stream.InputStream RowBinLogEvent)
decodeEvents packets = do
    events <- traverse (getFromPacket (getBinLogPacket False False)) packets
    fileName <- newIORef "mysql-bin.000001"
    eventStream <- Stream.fromList events
    decodeRowBinLogEvent (FormatDescription 4 "" 0 "", fileName, eventStream)

-- | A binlog event packet as COM_BINLOG_DUMP streams it, without checksum.
eventPacket :: Word8 -> Word16 -> Packet
eventPacket typeCode flags = Packet (L.length body) 1 body
  where
    eventBody = "\x01\x02\x03"
    body = runPut $ do
        putWord8 0x00                      -- OK marker
        putWord32le 1700000000             -- timestamp
        putWord8 typeCode
        putWord32le 1                      -- server id
        putWord32le (19 + 3)               -- event size: header and body
        putWord32le 4096                   -- next position
        putWord16le flags
        putLazyByteString eventBody

notIgnorable, ignorable :: Word16
notIgnorable = 0x0000
ignorable = 0x0080
