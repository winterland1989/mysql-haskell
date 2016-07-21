{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L

import Debug.Trace

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
isERR p = L.index (pBody p) 0 == 0xff

isOK :: Packet -> Bool
isOK p  = L.index (pBody p) 0 == 0x00

isEOF :: Packet -> Bool
isEOF p = L.index (pBody p) 0 == 0xfe

getFromPacket :: Binary a => Packet -> a
getFromPacket (Packet _ _ body) = runGet get body

putToPacket :: Binary a => a -> Word8 -> Packet
putToPacket payload seqN =
    let s = encode payload
        l = L.length s
    in Packet (fromIntegral l) seqN s

--------------------------------------------------------------------------------
-- general packets
data OK = OK
    { okAffectedRows :: LenEncInt
    , okLastInsertID :: LenEncInt
    , okStatus       :: Word16
    , okWarnings     :: Word16
    } deriving (Show, Eq)

data ERR = ERR
    { errCode     :: !Word16
    , errState    :: !L.ByteString
    , errMsg      :: !L.ByteString
    } deriving (Show, Eq)

instance Binary OK where
    get = getOK
    put = putOK

getOK :: Get OK
getOK = do
    w <- getWord8
    if w == 0x00
    then OK
        <$> getLenEncInt
        <*> getLenEncInt
        <*> getWord16le
        <*> getWord16le
    else fail "wrong OK packet"

putOK :: OK -> Put
putOK (OK row lid status warn) = do
    putWord8 0x00
    putLenEncInt row
    putLenEncInt lid
    putWord16le status
    putWord16le warn

--
getERR :: Int -> Get ERR
getERR len = do
    _  <- getWord8    -- todo verify this
    c  <- getWord16le
    skip 1
    st  <- getLazyByteString 5
    msg <- getLazyByteString $ fromIntegral (len - 9)
    return (ERR c st msg)

--------------------------------------------------------------------------------
-- Authentications

data Greeting = Greeting
    { protocol :: !Word8
    , version  :: !B.ByteString
    , tid      :: !Word32
    , salt1    :: !B.ByteString --TODO rename
    , caps     :: !Word16
    , lang     :: !Word8
    , status   :: !Word16
    , salt2    :: !B.ByteString
    } deriving (Show, Eq)

putGreeting :: Greeting -> Put
putGreeting (Greeting p v t s1 c l st s2) = do
    putWord8 p
    putByteString v
    putWord8 0
    putWord32le t
    putByteString s1
    putWord16le c
    putWord8 l
    putWord16le st
    replicateM_ 13 $ putWord8 0
    putByteString s2

getGreeting :: Get Greeting
getGreeting = do
    p  <- getWord8
    v  <- getLazyByteStringNul
    t  <- getWord32le
    s1 <- getLazyByteStringNul
    c  <- getWord16le
    l  <- getWord8
    st <- getWord16le
    skip 13
    s2 <- getLazyByteStringNul
    _ <- getLazyByteStringNul
    return $ Greeting p (L.toStrict v) t (L.toStrict s1) c l st $ L.toStrict s2

instance Binary Greeting where
    get = getGreeting
    put = putGreeting

data Auth = Auth
    { acaps     :: !Word32
    , maxPacket :: !Word32
    , charset   :: !Word8
    , name      :: !ByteString
    , password  :: !ByteString
    , schema    :: !ByteString
    } deriving (Show, Eq)

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skip 23
    n <- getLazyByteStringNul --TODO buggy!
    return $ Auth a m c (L.toStrict n) B.empty B.empty

putAuth :: Auth -> Put
putAuth (Auth cap m c n p s) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 23 (putWord8 0)
    putByteString n >> putWord8 0
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s -- TODO buggy
    putWord8 0

instance Binary Auth where
    get = getAuth
    put = putAuth

--------------------------------------------------------------------------------
--  Commands

data Command
    = COM_QUIT                                    --  0x01
    | COM_INIT_DB        !ByteString              --  0x02
    | COM_QUERY          !ByteString              --  0x03
    | COM_PING                                    --  0x0E
    | COM_BINLOG_DUMP    !Word32 !Word32 !Word32 !ByteString
            -- ^ binlog-pos, flags(0x01), server-id, binlog-filename
    | COM_REGISTER_SLAVE !Word32 !LenEncBytes !LenEncBytes !LenEncBytes !Word16 !Word32 !Word32 --  0x15
            -- ^ server-id, slaves hostname, slaves user, slaves password,  slaves port, replication rank(ignored), master-id(usually 0)
    | COM_UNSUPPORTED
   deriving (Show, Eq)

getCommand :: Get Command
getCommand = do
    cmdId <- getWord8
    case cmdId of
        0x01  -> pure COM_QUIT
        0x02  -> COM_INIT_DB . L.toStrict <$> getRemainingLazyByteString
        0x03  -> COM_QUERY . L.toStrict <$> getRemainingLazyByteString
        0x0E  -> pure COM_PING
        0x12  -> COM_BINLOG_DUMP
                    <$> getWord32le <*> getWord32le <*> getWord32le <*> getRemainingByteString
        0x15  -> COM_REGISTER_SLAVE
                    <$> getWord32le <*> getLenEncoded <*> getLenEncoded <*> getLenEncoded
                    <*> getWord16le <*> getWord32le <*> getWord32le
        _     -> pure COM_UNSUPPORTED

putCommand :: Command -> Put
putCommand COM_QUIT              = putWord8 0x01
putCommand (COM_INIT_DB db)      = putWord8 0x02 >> putByteString db
putCommand (COM_QUERY q)         = putWord8 0x03 >> putByteString q
putCommand COM_PING              = putWord8 0x0E
putCommand (COM_BINLOG_DUMP pos flags mid fname) = do
    putWord8 0x0C
    putWord32le pos
    putWord32le flags
    putWord32be mid
    putByteString fname
putCommand (COM_REGISTER_SLAVE sid shost susr spass sport rrank mid) = do
    putWord32le sid
    putLenEncoded shost
    putLenEncoded susr
    putLenEncoded spass
    putWord16le sport
    putWord32le rrank
    putWord32le mid
putCommand COM_UNSUPPORTED       = fail "unsupported command"

instance Binary Command where
    get = getCommand
    put = putCommand

--------------------------------------------------------------------------------
--  Helpers

newtype LenEncBytes = LenEncBytes B.ByteString deriving (Show, Eq, Ord)

instance Binary LenEncBytes where
    put = putLenEncoded
    get = getLenEncoded

putLenEncoded :: LenEncBytes -> Put
putLenEncoded (LenEncBytes c) = do
        let l = B.length c
        putWord8 $ fromIntegral l
        putByteString c

getLenEncoded :: Get LenEncBytes
getLenEncoded = do
    b <- lookAhead getWord8
    if b == 0xfb
    then getWord8 >> return (LenEncBytes B.empty)
    else do
        (LenEncInt len) <- getLenEncInt
        str <- getByteString len
        return (LenEncBytes str)

newtype LenEncInt = LenEncInt Int deriving (Show, Eq, Ord)
-- | length encoded int
-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
--
getLenEncInt:: Get LenEncInt
getLenEncInt = getWord8 >>= word2Len
  where
    word2Len l
         | l <  0xfb  = return (LenEncInt (fromIntegral l))
         | l == 0xfc  = LenEncInt . fromIntegral <$> getWord16le
         | l == 0xfd  = LenEncInt . fromIntegral <$> getWord24le
         | l == 0xfe  = LenEncInt . fromIntegral <$> getWord64le
         | otherwise = fail $ "invalid length val " ++ show l

putLenEncInt:: LenEncInt -> Put
putLenEncInt (LenEncInt x)
         | x <  251      = putWord8    (fromIntegral x)
         | x < 65536     = putWord16le (fromIntegral x)
         | x < 16777216  = putWord24le (fromIntegral x)
         | otherwise     = putWord64le (fromIntegral x)

instance Binary LenEncInt where
    get = getLenEncInt
    put = putLenEncInt

putWord24le :: Int -> Put
putWord24le v = do
    putWord16le $ fromIntegral v
    putWord8 $ fromIntegral (v `shiftR` 16)

getWord24le :: Get Int
getWord24le = do
    a <- fromIntegral <$> getWord16le
    b <- fromIntegral <$> getWord8
    return $! a .|. (b `shiftL` 16)

getByteStringNul :: Get ByteString
getByteStringNul = L.toStrict <$> getLazyByteStringNul

getRemainingByteString :: Get ByteString
getRemainingByteString = L.toStrict <$> getRemainingLazyByteString

--------------------------------------------------------------------------------
-- default Capability Flags

#define CLIENT_LONG_PASSWORD     0x00000001
#define CLIENT_FOUND_ROWS        0x00000002
#define CLIENT_LONG_FLAG         0x00000004
#define CLIENT_CONNECT_WITH_DB   0x00000008

#define CLIENT_IGNORE_SPACE      0x00000100
#define CLIENT_PROTOCOL_41       0x00000200
#define CLIENT_TRANSACTIONS      0x00002000
#define CLIENT_SECURE_CONNECTION 0x00008000

#define CLIENT_MULTI_RESULTS     0x00020000
#define CLIENT_PS_MULTI_RESULTS  0x00040000
#define CLIENT_PLUGIN_AUTH       0x00080000


clientCap :: Word32
clientCap =  CLIENT_LONG_PASSWORD
                .|. CLIENT_LONG_FLAG
                .|. CLIENT_CONNECT_WITH_DB
                .|. CLIENT_IGNORE_SPACE
                .|. CLIENT_PROTOCOL_41
                .|. CLIENT_TRANSACTIONS
                .|. CLIENT_SECURE_CONNECTION

clientMaxPacketSize :: Word32
clientMaxPacketSize = 0x00ffffff :: Word32

clientCharset :: Word8
clientCharset = 0x21 :: Word8
