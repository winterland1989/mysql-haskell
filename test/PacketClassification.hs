-- | Telling replies apart by their first byte. A row can start with the EOF
-- marker, and an empty packet has no first byte at all.
module PacketClassification (tests) where

import           Control.Exception              (SomeException, evaluate, try)
import qualified Data.ByteString.Lazy           as L
import           Database.MySQL.Protocol.Packet
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "packet classification"
    [ testCase "a row whose first value is 16 MiB or longer is not EOF" $ do
        -- 0xFE then the 8-byte length 16777216, then the value's first bytes.
        assertBool "row taken for EOF" (not (isEOF (packet ("\xFE\x00\x00\x00\x01\x00\x00\x00\x00" <> "abc"))))
        assertBool "EOF packet not recognised" (isEOF (packet "\xFE\x00\x00\x02\x00"))
    , testCase "an empty packet is neither OK, ERR nor EOF" $ do
        classified <- try (evaluate (isOK empty || isERR empty || isEOF empty))
        case classified of
            Left (failure :: SomeException) -> assertFailure ("classifying crashed: " <> show failure)
            Right anyMatch -> assertBool "empty packet classified" (not anyMatch)
    , testCase "an OK packet keeps an insert ID above Int's range" $ do
        ok <- decodeFromPacket (packet ("\x00\x01\xFE" <> "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF" <> "\x02\x00\x00\x00")) :: IO OK
        assertEqual "last insert ID" (18446744073709551615 :: Integer) (toInteger (okLastInsertID ok))
    , testCase "an OK packet with affected rows above Int's range fails to decode" $ do
        decoded <- try (decodeFromPacket (packet ("\x00\xFE" <> "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF" <> "\x00\x02\x00\x00\x00")) :: IO OK)
        case decoded of
            Left (DecodePacketFailed _ _ _) -> pure ()
            Right ok -> assertFailure ("decoded as " <> show ok)
    ]

packet :: L.ByteString -> Packet
packet body = Packet (L.length body) 1 body

empty :: Packet
empty = packet ""
