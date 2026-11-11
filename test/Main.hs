module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import Test.Tasty (defaultMain, testGroup)
import qualified JSON
import qualified Sha1Scramble
import qualified Sha256Scramble
import qualified Ed25519Scramble
import qualified Word24
import qualified TCPStreams
import qualified BoundsCheck
import qualified FieldRoundtrip
import qualified TextRowDecoding
import qualified DirectDecoding
import qualified DecoderRoundtrip
import qualified FloatingPointText
import qualified HandshakeGolden
import qualified BinLogEventCodes
import qualified PacketClassification
import qualified Placeholders
import qualified ResultCharsets
import qualified ZeroDates

main :: IO ()
main = do
    jsonTests <- JSON.tests
    defaultMain $ testGroup "tests" [
      testGroup "binary-parser" [
        testGroup "bs" ByteString.tests
      , testGroup "combinator" Combinator.tests
      , testGroup "JSON" jsonTests
      ],
      testGroup "word24"
         Word24.tests
      , testGroup "tcp-streams"
         [
           TCPStreams.tests
         ]
      , Sha1Scramble.tests
      , Sha256Scramble.tests
      , Ed25519Scramble.tests
      , BoundsCheck.tests
      , FieldRoundtrip.tests
      , TextRowDecoding.tests
      , DirectDecoding.tests
      , DecoderRoundtrip.tests
      , FloatingPointText.tests
      , HandshakeGolden.tests
      , PacketClassification.tests
      , Placeholders.tests
      , ZeroDates.tests
      , ResultCharsets.tests
      , BinLogEventCodes.tests
      ]
