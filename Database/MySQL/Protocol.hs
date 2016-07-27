{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}


{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol where

import           Control.Monad
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
import Data.String (IsString(..))

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
-- Authentications

data Greeting = Greeting
    { protocol :: !Word8
    , version  :: !B.ByteString
    , tid      :: !Word32
    , salt1    :: !B.ByteString
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
    { authCaps      :: !Word32
    , authMaxPacket :: !Word32
    , authCharset   :: !Word8
    , authName      :: !ByteString
    , authPassword  :: !ByteString
    , authSchema    :: !ByteString
    } deriving (Show, Eq)

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skip 23
    n <- getByteStringNul
    return $ Auth a m c n B.empty B.empty

putAuth :: Auth -> Put
putAuth (Auth cap m c n p s) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 23 (putWord8 0)
    putByteString n >> putWord8 0
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s
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
    | COM_BINLOG_DUMP    !Word32 !Word16 !Word32 !ByteString
            -- ^ binlog-pos, flags(0x01), server-id, binlog-filename
    | COM_REGISTER_SLAVE !Word32 !ByteString !ByteString !ByteString !Word16 !Word32 !Word32 --  0x15
            -- ^ server-id, slaves hostname, slaves user, slaves password,  slaves port, replication rank(ignored), master-id(usually 0)
    | COM_STMT_PREPARE   !ByteString              -- 0x16
    | COM_STMT_EXECUTE                            -- 0x17
    | COM_STMT_SEND_LONG_DATA                     -- 0x18
    | COM_STMT_CLOSE                              -- 0x19
    | COM_STMT_RESET                              -- 0x1A
    | COM_STMT_FETCH                              -- 0x1C
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
                    <$> getWord32le <*> getWord16le <*> getWord32le <*> getRemainingByteString
        0x15  -> COM_REGISTER_SLAVE
                    <$> getWord32le <*> getLenEncBytes <*> getLenEncBytes <*> getLenEncBytes
                    <*> getWord16le <*> getWord32le <*> getWord32le
        _     -> pure COM_UNSUPPORTED

putCommand :: Command -> Put
putCommand COM_QUIT              = putWord8 0x01
putCommand (COM_INIT_DB db)      = putWord8 0x02 >> putByteString db
putCommand (COM_QUERY q)         = putWord8 0x03 >> putByteString q
putCommand COM_PING              = putWord8 0x0E
putCommand (COM_BINLOG_DUMP pos flags sid fname) = do
    putWord8 0x12
    putWord32le pos
    putWord16le flags
    putWord32le sid
    putByteString fname
putCommand (COM_REGISTER_SLAVE sid shost susr spass sport rrank mid) = do
    putWord8 0x15
    putWord32le sid
    putLenEncBytes shost
    putLenEncBytes susr
    putLenEncBytes spass
    putWord16le sport
    putWord32le rrank
    putWord32le mid
putCommand COM_UNSUPPORTED       = fail "unsupported command"

instance Binary Command where
    get = getCommand
    put = putCommand

--------------------------------------------------------------------------------
--  Resultset

-- | A description of a field (column) of a table.
data Field = Field
    { -- fieldCatalog :: !ByteString            -- ^ const 'def'
      fieldDB ::         !ByteString            -- ^ Database for table.
    , fieldTable ::      !ByteString            -- ^ Table of column, if column was a field.
    , fieldOrigTable ::  !ByteString            -- ^ Original table name, if table was an alias.
    , fieldName ::       !ByteString            -- ^ Name of column.
    , fieldOrigName ::   !ByteString            -- ^ Original column name, if an alias.
    -- fieldFixedLen ::  !LenEncInt              -- ^ const '0x0C'
    , fieldCharSet ::    !Word16                 -- ^ Character set number.
    , fieldLength ::     !Word32                 -- ^ Width of column (create length).
    , fieldType ::       !FieldType
    , fieldFlags ::      !Word16                 -- ^ Div flags.
    , fieldDecimals ::   !Word8                  -- ^ Number of decimals in field.
    -- fieldfiller :: Word16                     -- const 0x00 0x00
    } deriving (Show, Eq)


getField :: Get Field
getField = Field
        <$> (skip 4                 -- const "def"
         *> getLenEncBytes)         -- db
        <*> getLenEncBytes          -- table
        <*> getLenEncBytes          -- origTable
        <*> getLenEncBytes          -- name
        <*> getLenEncBytes          -- origName
        <*  skip 1                  -- const 0x0c
        <*> getWord16le             -- charset,
        <*> getWord32le             -- length
        <*> getFieldType            -- type
        <*> getWord16le             -- flags
        <*> getWord8                -- decimals
        <* skip 2                   -- const 0x00 0x00

putField :: Field -> Put
putField (Field db tbl otbl name oname charset len typ flags dec) = do
    putLenEncBytes "def"
    putLenEncBytes db
    putLenEncBytes tbl
    putLenEncBytes otbl
    putLenEncBytes name
    putLenEncBytes oname
    putWord16le charset
    putWord32le len
    putFieldType typ
    putWord16le  flags
    putWord8 dec
    putWord16le 0X0000

instance Binary Field where
    get = getField
    put = putField


data FieldType
    = DECIMAL     -- 0x00
    | TINY        -- 0x01
    | SHORT       -- 0x02
    | LONG        -- 0x03
    | FLOAT       -- 0x04
    | DOUBLE      -- 0x05
    | NULL        -- 0x06
    | TIMESTAMP   -- 0x07
    | LONGLONG    -- 0x08
    | INT24       -- 0x09
    | DATE        -- 0x0a
    | TIME        -- 0x0b
    | DATETIME    -- 0x0c
    | YEAR        -- 0x0d
    | NEWDATE     -- 0x0e
    | VARCHAR     -- 0x0f
    | BIT         -- 0x10
    | TIMESTAMP2  -- 0x11
    | DATETIME2   -- 0x12
    | TIME2       -- 0x13
    | NEWDECIMAL  -- 0xf6
    | ENUM        -- 0xf7
    | SET         -- 0xf8
    | TINY_BLOB   -- 0xf9
    | MEDIUM_BLOB -- 0xfa
    | LONG_BLOB   -- 0xfb
    | BLOB        -- 0xfc
    | VAR_STRING  -- 0xfd
    | STRING      -- 0xfe
    | GEOMETRY    -- 0xff
  deriving (Show, Eq, Enum)

getFieldType :: Get FieldType
getFieldType = do
    w <- getWord8
    case w of
        0x00 -> pure DECIMAL
        0x01 -> pure TINY
        0x02 -> pure SHORT
        0x03 -> pure LONG
        0x04 -> pure FLOAT
        0x05 -> pure DOUBLE
        0x06 -> pure NULL
        0x07 -> pure TIMESTAMP
        0x08 -> pure LONGLONG
        0x09 -> pure INT24
        0x0a -> pure DATE
        0x0b -> pure TIME
        0x0c -> pure DATETIME
        0x0d -> pure YEAR
        0x0e -> pure NEWDATE
        0x0f -> pure VARCHAR
        0x10 -> pure BIT
        0x11 -> pure TIMESTAMP2
        0x12 -> pure DATETIME2
        0x13 -> pure TIME2
        0xf6 -> pure NEWDECIMAL
        0xf7 -> pure ENUM
        0xf8 -> pure SET
        0xf9 -> pure TINY_BLOB
        0xfa -> pure MEDIUM_BLOB
        0xfb -> pure LONG_BLOB
        0xfc -> pure BLOB
        0xfd -> pure VAR_STRING
        0xfe -> pure STRING
        0xff -> pure GEOMETRY
        _    -> fail $ "wrong FieldType: " ++ show w

putFieldType :: FieldType -> Put
putFieldType DECIMAL    = putWord8 0x00
putFieldType TINY       = putWord8 0x01
putFieldType SHORT      = putWord8 0x02
putFieldType LONG       = putWord8 0x03
putFieldType FLOAT      = putWord8 0x04
putFieldType DOUBLE     = putWord8 0x05
putFieldType NULL       = putWord8 0x06
putFieldType TIMESTAMP  = putWord8 0x07
putFieldType LONGLONG   = putWord8 0x08
putFieldType INT24      = putWord8 0x09
putFieldType DATE       = putWord8 0x0a
putFieldType TIME       = putWord8 0x0b
putFieldType DATETIME   = putWord8 0x0c
putFieldType YEAR       = putWord8 0x0d
putFieldType NEWDATE    = putWord8 0x0e
putFieldType VARCHAR    = putWord8 0x0f
putFieldType BIT        = putWord8 0x10
putFieldType TIMESTAMP2 = putWord8 0x11
putFieldType DATETIME2  = putWord8 0x12
putFieldType TIME2      = putWord8 0x13
putFieldType NEWDECIMAL = putWord8 0xf6
putFieldType ENUM       = putWord8 0xf7
putFieldType SET        = putWord8 0xf8
putFieldType TINY_BLOB  = putWord8 0xf9
putFieldType MEDIUM_BLOB= putWord8 0xfa
putFieldType LONG_BLOB  = putWord8 0xfb
putFieldType BLOB       = putWord8 0xfc
putFieldType VAR_STRING = putWord8 0xfd
putFieldType STRING     = putWord8 0xfe
putFieldType GEOMETRY   = putWord8 0xff

instance Binary FieldType where
    get = getFieldType
    put = putFieldType

newtype TextRow   = TextRow   { runTextRow   :: [Maybe ByteString] } deriving (Show, Eq)
newtype BinaryRow = BinaryRow { runBinaryRow :: [Maybe ByteString] } deriving (Show, Eq)

getTextRow :: Int -> Get TextRow
getTextRow fieldCnt = TextRow <$> (replicateM fieldCnt $ do
        p <- lookAhead getWord8
        if p == 0x79
        then getWord8 >> return Nothing
        else Just <$> getLenEncBytes
    )

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

newtype LenEncInt = LenEncInt { runLenEncInt :: Int } deriving (Show, Eq, Ord)
-- | length encoded int
-- https://dev.mysql.com/doc/internals/en/integer.html#packet-Protocol::LengthEncodedInteger
--
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
-- default Capability Flags

#define CLIENT_LONG_PASSWORD 0x00000001
#define CLIENT_FOUND_ROWS 0x00000002
#define CLIENT_LONG_FLAG 0x00000004
#define CLIENT_CONNECT_WITH_DB 0x00000008
#define CLIENT_NO_SCHEMA 0x00000010
#define CLIENT_COMPRESS 0x00000020
#define CLIENT_ODBC 0x00000040
#define CLIENT_LOCAL_FILES 0x00000080
#define CLIENT_IGNORE_SPACE 0x00000100
#define CLIENT_PROTOCOL_41 0x00000200
#define CLIENT_INTERACTIVE 0x00000400
#define CLIENT_SSL 0x00000800
#define CLIENT_IGNORE_SIGPIPE 0x00001000
#define CLIENT_TRANSACTIONS 0x00002000
#define CLIENT_RESERVED 0x00004000
#define CLIENT_SECURE_CONNECTION 0x00008000
#define CLIENT_MULTI_STATEMENTS 0x00010000
#define CLIENT_MULTI_RESULTS 0x00020000
#define CLIENT_PS_MULTI_RESULTS 0x00040000
#define CLIENT_PLUGIN_AUTH 0x00080000
#define CLIENT_CONNECT_ATTRS 0x00100000
#define CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA 0x00200000

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
