{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol.Packet where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import Control.Exception (throwIO, SomeException, Exception(..))
import Data.Typeable (Typeable, cast)

--------------------------------------------------------------------------------
-- | packet tyoe
data Packet = Packet
    { pLen  :: !Int
    , pSeqN :: !Word8
    , pBody :: !L.ByteString
    } deriving Show

putPacket :: Packet -> Put
putPacket (Packet len seqN body)  = do
    putWord24le len
    putWord8 seqN
    putLazyByteString body

getPacket :: Get Packet
getPacket = do
    len <- getWord24le
    seqN <- getWord8
    body <- getLazyByteString (fromIntegral len)
    return (Packet len seqN body)

instance Binary Packet where
    put = putPacket
    get = getPacket


isERR :: Packet -> Bool
isERR p = L.index (pBody p) 0 == 0xFF

isOK :: Packet -> Bool
isOK p  = L.index (pBody p) 0 == 0x00

isEOF :: Packet -> Bool
isEOF p = L.index (pBody p) 0 == 0xFE

-- | get packet inside IO, throw 'DecodePacketException' on fail parsing.
-- here we choose stability over correctness by omit incomplete consumed case:
-- if we successful parse a packet, then we don't care if there're bytes left.
decodeFromPacket :: Binary a => Packet -> IO a
decodeFromPacket (Packet _ _ body) = case decodeOrFail body of
    Left (buf, offset, errmsg) -> throwIO (DecodePacketException (L.toStrict buf) offset errmsg)
    Right (_, _, r)            -> return r

getFromPacket :: Get a -> Packet -> IO a
getFromPacket g (Packet _ _ body) = case runGetOrFail g body of
    Left (buf, offset, errmsg) -> throwIO (DecodePacketException (L.toStrict buf) offset errmsg)
    Right (_, _, r)            -> return r

encodeToPacket :: Binary a => a -> Word8 -> Packet
encodeToPacket payload seqN =
    let s = encode payload
        l = L.length s
    in Packet (fromIntegral l) seqN s

putToPacket :: Put -> Word8 -> Packet
putToPacket payload seqN =
    let s = runPut payload
        l = L.length s
    in Packet (fromIntegral l) seqN s

--------------------------------------------------------------------------------
-- OK, ERR, EOF
data OK = OK
    { okAffectedRows :: Int
    , okLastInsertID :: Int
    , okStatus       :: Word16
    , okWarningCnt   :: Word16
    } deriving (Show, Eq)

getOK :: Get OK
getOK = do
    w <- lookAhead getWord8
    if w == 0x00
    then OK <$> getLenEncInt
            <*> getLenEncInt
            <*> getWord16le
            <*> getWord16le
    else fail "wrong OK packet"

putOK :: OK -> Put
putOK (OK row lid stat wcnt) = do
    putWord8 0x00
    putLenEncInt row
    putLenEncInt lid
    putWord16le stat
    putWord16le wcnt

instance Binary OK where
    get = getOK
    put = putOK

data ERR = ERR
    { errCode     :: !Word16
    , errState    :: !ByteString
    , errMsg      :: !ByteString
    } deriving (Show, Eq)

getERR :: Get ERR
getERR = do
    w <- lookAhead getWord8
    if w == 0xFF
    then ERR <$> getWord16le
             <*  skip 1
             <*> getByteString 5
             <*> getRemainingByteString
    else fail "wrong ERR packet"

putERR :: ERR -> Put
putERR (ERR code stat msg) = do
    putWord8 0xFF
    putWord16le code
    putWord8 35 -- '#'
    putByteString stat
    putByteString msg

instance Binary ERR where
    get = getERR
    put = putERR

data EOF = EOF
    { eofWarningCnt :: !Word16
    , eofStatus     :: !Word16
    } deriving (Show, Eq)

getEOF :: Get EOF
getEOF = do
    w <- lookAhead getWord8
    if w == 0xFE
    then EOF <$> getWord16le
             <*> getWord16le
    else fail "wrong EOF packet"

putEOF :: EOF -> Put
putEOF (EOF wcnt stat) = do
    putWord8 0xFE
    putWord16le wcnt
    putWord16le stat

instance Binary EOF where
    get = getEOF
    put = putEOF

--------------------------------------------------------------------------------
--  Helpers

putLenEncBytes :: ByteString -> Put
putLenEncBytes c = do
        let l = B.length c
        putWord8 $ fromIntegral l
        putByteString c

getLenEncBytes :: Get ByteString
getLenEncBytes = do
    b <- lookAhead getWord8
    if b == 0xfb
    then getWord8 >> return B.empty
    else do
        len <- getLenEncInt
        str <- getByteString len
        return str

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

putLenEncInt:: Int -> Put
putLenEncInt x
         | x <  251      = putWord8    (fromIntegral x)
         | x < 65536     = putWord16le (fromIntegral x)
         | x < 16777216  = putWord24le (fromIntegral x)
         | otherwise     = putWord64le (fromIntegral x)

putWord24le :: Int -> Put
putWord24le v = do
    putWord16le $ fromIntegral v
    putWord8 $ fromIntegral (v `shiftR` 16)

getWord24le :: Get Int
getWord24le = do
    a <- fromIntegral <$> getWord16le
    b <- fromIntegral <$> getWord8
    return $! a .|. (b `shiftL` 16)

putWord48le :: Word64 -> Put
putWord48le v = do
    putWord32le $ fromIntegral v
    putWord16le $ fromIntegral (v `shiftR` 32)

getWord48le :: Get Word64
getWord48le = do
    a <- fromIntegral <$> getWord32le
    b <- fromIntegral <$> getWord16le
    return $! a .|. (b `shiftL` 32)

getByteStringNul :: Get ByteString
getByteStringNul = L.toStrict <$> getLazyByteStringNul

getRemainingByteString :: Get ByteString
getRemainingByteString = L.toStrict <$> getRemainingLazyByteString

--------------------------------------------------------------------------------
-- |the root exception type for all the mysql exceptions
data MySQLException = forall e . Exception e => MySQLException e deriving Typeable
instance Show MySQLException where show (MySQLException e) = show e
instance Exception MySQLException

mysqlExceptionToException :: Exception e => e -> SomeException
mysqlExceptionToException = toException . MySQLException

mysqlExceptionFromException :: Exception e => SomeException -> Maybe e
mysqlExceptionFromException x = do
    MySQLException a <- fromException x
    cast a

data NetworkException = NetworkException deriving (Typeable, Show)
instance Exception NetworkException where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data DecodePacketException = DecodePacketException ByteString ByteOffset String deriving (Typeable, Show)
instance Exception DecodePacketException where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data ERRException = ERRException ERR deriving (Typeable, Show)
instance Exception ERRException where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data UnconsumedResultSet = UnconsumedResultSet deriving (Typeable, Show)
instance Exception UnconsumedResultSet where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data AuthException = AuthException ERR deriving (Typeable, Show)
instance Exception AuthException where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

data UnexpectedPacket = UnexpectedPacket Packet deriving (Typeable, Show)
instance Exception UnexpectedPacket where
    toException   = mysqlExceptionToException
    fromException = mysqlExceptionFromException

