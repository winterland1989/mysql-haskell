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

import           Control.Applicative
import           Control.Exception     (Exception (..), throwIO)
import           Data.Binary.Parser
import           Data.Binary.Put
import           Data.Binary           (Binary(..), encode)
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Data.Int.Int24
import           Data.Int
import           Data.Word
import           Data.Typeable
import           Data.Word.Word24

--------------------------------------------------------------------------------
-- | MySQL packet type
--
data Packet = Packet
    { pLen  :: !Int64
    , pSeqN :: !Word8
    , pBody :: !L.ByteString
    } deriving (Show, Eq)

putPacket :: Packet -> Put
putPacket (Packet len seqN body)  = do
    putWord24le (fromIntegral len)
    putWord8 seqN
    putLazyByteString body
{-# INLINE putPacket #-}

getPacket :: Get Packet
getPacket = do
    len <- fromIntegral <$> getWord24le
    seqN <- getWord8
    body <- getLazyByteString (fromIntegral len)
    return (Packet len seqN body)
{-# INLINE getPacket #-}

instance Binary Packet where
    put = putPacket
    {-# INLINE put #-}
    get = getPacket
    {-# INLINE get #-}

isERR :: Packet -> Bool
isERR p = L.index (pBody p) 0 == 0xFF
{-# INLINE isERR #-}

isOK :: Packet -> Bool
isOK p  = L.index (pBody p) 0 == 0x00
{-# INLINE isOK #-}

isEOF :: Packet -> Bool
isEOF p = L.index (pBody p) 0 == 0xFE
{-# INLINE isEOF #-}

-- | Decoding packet inside IO, throw 'DecodePacketException' on fail parsing,
-- here we choose stability over correctness by omit incomplete consumed case:
-- if we successful parse a packet, then we don't care if there're bytes left.
--
decodeFromPacket :: Binary a => Packet -> IO a
decodeFromPacket = getFromPacket get
{-# INLINE decodeFromPacket #-}

getFromPacket :: Get a -> Packet -> IO a
getFromPacket g (Packet _ _ body) = case parseDetailLazy g body of
    Left  (buf, offset, errmsg) -> throwIO (DecodePacketFailed buf offset errmsg)
    Right (_,   _,      r     ) -> return r
{-# INLINE getFromPacket #-}

data DecodePacketException = DecodePacketFailed ByteString ByteOffset String
  deriving (Typeable, Show)
instance Exception DecodePacketException

encodeToPacket :: Binary a => Word8 -> a -> Packet
encodeToPacket seqN payload =
    let s = encode payload
        l = L.length s
    in Packet (fromIntegral l) seqN s
{-# INLINE encodeToPacket #-}

putToPacket :: Word8 -> Put -> Packet
putToPacket seqN payload =
    let s = runPut payload
        l = L.length s
    in Packet (fromIntegral l) seqN s
{-# INLINE putToPacket #-}

--------------------------------------------------------------------------------
-- OK, ERR, EOF

-- | You may get interested in 'OK' packet because it provides information about
-- successful operations.
--
data OK = OK
    { okAffectedRows :: !Int      -- ^ affected row number
    , okLastInsertID :: !Int      -- ^ last insert's ID
    , okStatus       :: !Word16
    , okWarningCnt   :: !Word16
    } deriving (Show, Eq)

getOK :: Get OK
getOK = OK <$ skipN 1
           <*> getLenEncInt
           <*> getLenEncInt
           <*> getWord16le
           <*> getWord16le
{-# INLINE getOK #-}

putOK :: OK -> Put
putOK (OK row lid stat wcnt) = do
    putWord8 0x00
    putLenEncInt row
    putLenEncInt lid
    putWord16le stat
    putWord16le wcnt
{-# INLINE putOK #-}

instance Binary OK where
    get = getOK
    {-# INLINE get #-}
    put = putOK
    {-# INLINE put #-}

data ERR = ERR
    { errCode  :: !Word16
    , errState :: !ByteString
    , errMsg   :: !ByteString
    } deriving (Show, Eq)

getERR :: Get ERR
getERR = ERR <$  skipN 1
             <*> getWord16le
             <*  skipN 1
             <*> getByteString 5
             <*> getRemainingByteString
{-# INLINE getERR #-}

putERR :: ERR -> Put
putERR (ERR code stat msg) = do
    putWord8 0xFF
    putWord16le code
    putWord8 35 -- '#'
    putByteString stat
    putByteString msg
{-# INLINE putERR #-}

instance Binary ERR where
    get = getERR
    {-# INLINE get #-}
    put = putERR
    {-# INLINE put #-}

data EOF = EOF
    { eofWarningCnt :: !Word16
    , eofStatus     :: !Word16
    } deriving (Show, Eq)

getEOF :: Get EOF
getEOF = EOF <$  skipN 1
             <*> getWord16le
             <*> getWord16le
{-# INLINE getEOF #-}

putEOF :: EOF -> Put
putEOF (EOF wcnt stat) = do
    putWord8 0xFE
    putWord16le wcnt
    putWord16le stat
{-# INLINE putEOF #-}

instance Binary EOF where
    get = getEOF
    {-# INLINE get #-}
    put = putEOF
    {-# INLINE put #-}

--------------------------------------------------------------------------------
--  Helpers

getByteStringNul :: Get ByteString
getByteStringNul = L.toStrict <$> getLazyByteStringNul
{-# INLINE getByteStringNul #-}

getRemainingByteString :: Get ByteString
getRemainingByteString = L.toStrict <$> getRemainingLazyByteString
{-# INLINE getRemainingByteString #-}

putLenEncBytes :: ByteString -> Put
putLenEncBytes c = do
    putLenEncInt (B.length c)
    putByteString c
{-# INLINE putLenEncBytes #-}

getLenEncBytes :: Get ByteString
getLenEncBytes = getLenEncInt >>= getByteString
{-# INLINE getLenEncBytes #-}

-- | length encoded int
-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
getLenEncInt:: Get Int
getLenEncInt = getWord8 >>= word2Len
  where
    word2Len l
         | l <  0xFB  = pure (fromIntegral l)
         | l == 0xFC  = fromIntegral <$> getWord16le
         | l == 0xFD  = fromIntegral <$> getWord24le
         | l == 0xFE  = fromIntegral <$> getWord64le
         | otherwise = fail $ "invalid length val " ++ show l
{-# INLINE getLenEncInt #-}

putLenEncInt:: Int -> Put
putLenEncInt x
         | x <  251      = putWord8 (fromIntegral x)
         | x < 65536     = putWord8 0xFC >> putWord16le (fromIntegral x)
         | x < 16777216  = putWord8 0xFD >> putWord24le (fromIntegral x)
         | otherwise     = putWord8 0xFE >> putWord64le (fromIntegral x)
{-# INLINE putLenEncInt #-}

putWord24le :: Word32 -> Put
putWord24le v = do
    putWord16le $ fromIntegral v
    putWord8 $ fromIntegral (v `shiftR` 16)
{-# INLINE putWord24le #-}

getWord24le :: Get Word32
getWord24le = do
    a <- fromIntegral <$> getWord16le
    b <- fromIntegral <$> getWord8
    return $! a .|. (b `shiftL` 16)
{-# INLINE getWord24le #-}

putWord48le :: Word64 -> Put
putWord48le v = do
    putWord32le $ fromIntegral v
    putWord16le $ fromIntegral (v `shiftR` 32)
{-# INLINE putWord48le #-}

getWord48le :: Get Word64
getWord48le = do
    a <- fromIntegral <$> getWord32le
    b <- fromIntegral <$> getWord16le
    return $! a .|. (b `shiftL` 32)
{-# INLINE getWord48le #-}

getWord24be :: Get Word24
getWord24be = do
    a <- fromIntegral <$> getWord16be
    b <- fromIntegral <$> getWord8
    return $! b .|. (a `shiftL` 8)
{-# INLINE getWord24be #-}

getInt24be :: Get Int24
getInt24be = do
    a <- fromIntegral <$> getWord16be
    b <- fromIntegral <$> getWord8
    return $! fromIntegral $ (b .|. (a `shiftL` 8) :: Word24)
{-# INLINE getInt24be #-}

getWord40be, getWord48be, getWord56be :: Get Word64
getWord40be = do
    a <- fromIntegral <$> getWord32be
    b <- fromIntegral <$> getWord8
    return $! (a `shiftL` 8) .|. b
getWord48be = do
    a <- fromIntegral <$> getWord32be
    b <- fromIntegral <$> getWord16be
    return $! (a `shiftL` 16) .|. b
getWord56be = do
    a <- fromIntegral <$> getWord32be
    b <- fromIntegral <$> getWord24be
    return $! (a `shiftL` 24) .|. b
{-# INLINE getWord40be #-}
{-# INLINE getWord48be #-}
{-# INLINE getWord56be #-}
