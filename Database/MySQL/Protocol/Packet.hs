{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.Protocol.Packet
Description : MySQL packet type and various helpers.
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

MySQL packet decoder&encoder, and varities utility.

-}

module Database.MySQL.Protocol.Packet where

import           Data.Bits
import           Data.Word
import           GHC.Generics
import           Z.IO.Exception
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V

--------------------------------------------------------------------------------
-- | MySQL packet type
--
data Packet = Packet
    { pLen  :: {-# UNPACK #-} !Int
    , pSeqN :: {-# UNPACK #-} !Word8
    , pBody :: !V.Bytes
    } deriving (Show, Eq, Ord, Generic)
      deriving anyclass T.Print

encodePacket :: Packet -> B.Builder ()
encodePacket (Packet len seqN body)  = do
    encodeWord24LE (fromIntegral len)
    B.word8 seqN
    B.bytes body
{-# INLINE encodePacket #-}

decodePacket :: P.Parser Packet
decodePacket = do
    len <- fromIntegral <$> decodeWord24LE
    seqN <- P.anyWord8
    body <- P.take (fromIntegral len)
    return (Packet len seqN body)
{-# INLINE decodePacket #-}

isERR :: Packet -> Bool
isERR p = V.unsafeIndex (pBody p) 0 == 0xFF
{-# INLINE isERR #-}

isOK :: Packet -> Bool
isOK p  = V.unsafeIndex (pBody p) 0 == 0x00
{-# INLINE isOK #-}

isEOF :: Packet -> Bool
isEOF p = V.unsafeIndex (pBody p) 0 == 0xFE
{-# INLINE isEOF #-}

-- | Is there more packet to be read?
--
--  https://dev.mysql.com/doc/internals/en/status-flags.html
isThereMore :: OK -> Bool
isThereMore p  = okStatus p .&. 0x08 /= 0
{-# INLINE isThereMore #-}

-- | Decoding packet inside IO, throw 'OtherError' on fail parsing,
-- here we choose stability over correctness by omit incomplete consumed case:
-- if we successful parse a packet, then we don't care if there're bytes left.
--
decodeFromPacket :: P.Parser a -> Packet -> IO a
decodeFromPacket g (Packet _ _ body) = unwrap "EPARSE" $ P.parse' g body
{-# INLINE decodeFromPacket #-}


-- Encode a packet with a sequence number
encodeToPacket :: Word8 -> B.Builder () -> Packet
encodeToPacket seqN payload =
    let s = B.build payload
        l = V.length s
    in Packet (fromIntegral l) seqN s
{-# INLINE encodeToPacket #-}

--------------------------------------------------------------------------------
-- OK, ERR, EOF

-- | You may decode interested in 'OK' packet because it provides information about
-- successful operations.
--
data OK = OK
    { okAffectedRows :: {-# UNPACK #-} !Int      -- ^ affected row number
    , okLastInsertID :: {-# UNPACK #-} !Int      -- ^ last insert's ID
    , okStatus       :: {-# UNPACK #-} !Word16
    , okWarningCnt   :: {-# UNPACK #-} !Word16
    } deriving (Show, Eq, Ord, Generic)
      deriving anyclass T.Print

decodeOK :: P.Parser OK
decodeOK = OK <$ P.skip 1
           <*> decodeLenEncInt
           <*> decodeLenEncInt
           <*> P.decodePrimLE @Word16
           <*> P.decodePrimLE @Word16
{-# INLINE decodeOK #-}

encodeOK :: OK -> B.Builder ()
encodeOK (OK row lid stat wcnt) = do
    B.word8 0x00
    encodeLenEncInt row
    encodeLenEncInt lid
    B.encodePrimLE @Word16 stat
    B.encodePrimLE @Word16 wcnt
{-# INLINE encodeOK #-}

data ERR = ERR
    { errCode  :: {-# UNPACK #-} !Word16
    , errState :: !V.Bytes
    , errMsg   :: !V.Bytes
    } deriving (Show, Eq, Ord, Generic)
      deriving anyclass T.Print

decodeERR :: P.Parser ERR
decodeERR = ERR <$  P.skip 1
             <*> P.decodePrimLE @Word16
             <*  P.skip 1
             <*> P.take 5
             <*> P.takeRemaining
{-# INLINE decodeERR #-}

encodeERR :: ERR -> B.Builder ()
encodeERR (ERR code stat msg) = do
    B.word8 0xFF
    B.encodePrimLE @Word16 code
    B.word8 35 -- '#'
    B.bytes stat
    B.bytes msg
{-# INLINE encodeERR #-}

data EOF = EOF
    { eofWarningCnt :: {-# UNPACK #-} !Word16
    , eofStatus     :: {-# UNPACK #-} !Word16
    } deriving (Show, Eq, Ord, Generic)
      deriving anyclass T.Print

decodeEOF :: P.Parser EOF
decodeEOF = EOF <$  P.skip 1
             <*> P.decodePrimLE @Word16
             <*> P.decodePrimLE @Word16
{-# INLINE decodeEOF #-}

encodeEOF :: EOF -> B.Builder ()
encodeEOF (EOF wcnt stat) = do
    B.word8 0xFE
    B.encodePrimLE @Word16 wcnt
    B.encodePrimLE @Word16 stat
{-# INLINE encodeEOF #-}

--------------------------------------------------------------------------------
--  Helpers

decodeBytesNul :: P.Parser V.Bytes
decodeBytesNul = P.takeTill (== 0)
{-# INLINE decodeBytesNul #-}

encodeLenEncBytes :: V.Bytes -> B.Builder ()
encodeLenEncBytes c = do
    encodeLenEncInt (V.length c)
    B.bytes c
{-# INLINE encodeLenEncBytes #-}

decodeLenEncBytes :: P.Parser V.Bytes
decodeLenEncBytes = decodeLenEncInt >>= P.take
{-# INLINE decodeLenEncBytes #-}

-- | length encoded int
-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
decodeLenEncInt:: P.Parser Int
decodeLenEncInt = P.anyWord8 >>= word2Len
  where
    word2Len l
         | l <  0xFB  = pure (fromIntegral l)
         | l == 0xFC  = fromIntegral <$> P.decodePrimLE @Word16
         | l == 0xFD  = fromIntegral <$> decodeWord24LE
         | l == 0xFE  = fromIntegral <$> P.decodePrimLE @Word64
         | otherwise = fail $ "invalid length val " ++ show l
{-# INLINE decodeLenEncInt #-}

encodeLenEncInt:: Int -> B.Builder ()
encodeLenEncInt x
         | x <  251      = B.word8 (fromIntegral x)
         | x < 65536     = B.word8 0xFC >> B.encodePrimLE @Word16 (fromIntegral x)
         | x < 16777216  = B.word8 0xFD >> encodeWord24LE (fromIntegral x)
         | otherwise     = B.word8 0xFE >> B.encodePrimLE @Word64 (fromIntegral x)
{-# INLINE encodeLenEncInt #-}

encodeWord24LE :: Word32 -> B.Builder ()
encodeWord24LE v = do
    B.encodePrimLE @Word16 $ fromIntegral v
    B.word8 $ fromIntegral (v `shiftR` 16)
{-# INLINE encodeWord24LE #-}

decodeWord24LE :: P.Parser Word32
decodeWord24LE = do
    a <- fromIntegral <$> P.decodePrimLE @Word16
    b <- fromIntegral <$> P.anyWord8
    return $! a .|. (b `unsafeShiftL` 16)
{-# INLINE decodeWord24LE #-}

encodeWord48LE :: Word64 -> B.Builder ()
encodeWord48LE v = do
    B.encodePrimLE @Word32 $ fromIntegral v
    B.encodePrimLE @Word16 $ fromIntegral (v `shiftR` 32)
{-# INLINE encodeWord48LE #-}

decodeWord48LE :: P.Parser Word64
decodeWord48LE = do
    a <- fromIntegral <$> P.decodePrimLE @Word32
    b <- fromIntegral <$> P.decodePrimLE @Word16
    return $! a .|. (b `unsafeShiftL` 32)
{-# INLINE decodeWord48LE #-}

decodeWord24BE :: P.Parser Word32
decodeWord24BE = do
    a <- fromIntegral <$> P.decodePrimBE @Word16
    b <- fromIntegral <$> P.anyWord8
    return $! b .|. (a `unsafeShiftL` 8)
{-# INLINE decodeWord24BE #-}

decodeWord40BE, decodeWord48BE, decodeWord56BE :: P.Parser Word64
decodeWord40BE = do
    a <- fromIntegral <$> P.decodePrimBE @Word32
    b <- fromIntegral <$> P.anyWord8
    return $! (a `unsafeShiftL` 8) .|. b
decodeWord48BE = do
    a <- fromIntegral <$> P.decodePrimBE @Word32
    b <- fromIntegral <$> P.decodePrimBE @Word16
    return $! (a `unsafeShiftL` 16) .|. b
decodeWord56BE = do
    a <- fromIntegral <$> P.decodePrimBE @Word32
    b <- fromIntegral <$> decodeWord24BE
    return $! (a `unsafeShiftL` 24) .|. b
{-# INLINE decodeWord40BE #-}
{-# INLINE decodeWord48BE #-}
{-# INLINE decodeWord56BE #-}
