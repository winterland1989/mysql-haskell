{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.Protocol.Packet
Description : MySQL packet type
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
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Data.Int.Int24
import           Data.Int
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
decodeFromPacket (Packet _ _ body) = case pushEndOfInput $ pushChunks (runGetIncremental get) body of
    Done _  _ r             -> return r
    Fail buf offset errmsg  -> throwIO (DecodePacketFailed buf offset errmsg)
    Partial _               -> throwIO DecodePacketPartial
{-# INLINE decodeFromPacket #-}

getFromPacket :: Get a -> Packet -> IO a
getFromPacket g (Packet _ _ body) = case pushEndOfInput $ pushChunks (runGetIncremental g) body of
    Done _  _ r             -> return r
    Fail buf offset errmsg  -> throwIO (DecodePacketFailed buf offset errmsg)
    Partial _               -> throwIO DecodePacketPartial
{-# INLINE getFromPacket #-}

data DecodePacketException
    = DecodePacketFailed ByteString ByteOffset String
    | DecodePacketPartial
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
getOK = OK <$ skip 1
           <*> getLenEncInt
           <*> getLenEncInt
           <*> getWord16le
           <*> getWord16le

putOK :: OK -> Put
putOK (OK row lid stat wcnt) = do
    putWord8 0x00
    putLenEncInt row
    putLenEncInt lid
    putWord16le stat
    putWord16le wcnt

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
getERR = ERR <$  skip 1
             <*> getWord16le
             <*  skip 1
             <*> getByteString 5
             <*> getRemainingByteString

putERR :: ERR -> Put
putERR (ERR code stat msg) = do
    putWord8 0xFF
    putWord16le code
    putWord8 35 -- '#'
    putByteString stat
    putByteString msg

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
getEOF = EOF <$  skip 1
             <*> getWord16le
             <*> getWord16le

putEOF :: EOF -> Put
putEOF (EOF wcnt stat) = do
    putWord8 0xFE
    putWord16le wcnt
    putWord16le stat

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
        let l = B.length c
        putLenEncInt l
        putByteString c
{-# INLINE putLenEncBytes #-}

getLenEncBytes :: Get ByteString
getLenEncBytes = do
    b <- lookAhead getWord8
    if b == 0xfb
    then getWord8 >> return B.empty
    else getLenEncInt >>= getByteString
{-# INLINE getLenEncBytes #-}

-- | length encoded int
-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
getLenEncInt:: Get Int
getLenEncInt = getWord8 >>= word2Len
  where
    word2Len l
         | l <  0xfb  = return (fromIntegral l)
         | l == 0xfc  = fromIntegral <$> getWord16le
         | l == 0xfd  = fromIntegral <$> getWord24le
         | l == 0xfe  = fromIntegral <$> getWord64le
         | otherwise = fail $ "invalid length val " ++ show l
{-# INLINE getLenEncInt #-}

putLenEncInt:: Int -> Put
putLenEncInt x
         | x <  251      = putWord8    (fromIntegral x)
         | x < 65536     = putWord16le (fromIntegral x)
         | x < 16777216  = putWord24le (fromIntegral x)
         | otherwise     = putWord64le (fromIntegral x)
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
