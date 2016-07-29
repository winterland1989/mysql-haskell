{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol.Command where

import           Control.Monad
import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import   Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.Field
import           Database.MySQL.Protocol.MySQLValue
import Debug.Trace

--------------------------------------------------------------------------------
--  Commands

type StmtID = Word32

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
    | COM_STMT_EXECUTE   !StmtID ![Field] ![MySQLValue]    -- 0x17
            -- ^ paramDef, stmtId, params
    | COM_STMT_SEND_LONG_DATA                     -- 0x18
    | COM_STMT_CLOSE     !StmtID                        -- 0x19
    | COM_STMT_RESET     !StmtID                        -- 0x1A
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
putCommand (COM_STMT_PREPARE stmt) = putWord8 0x16 >> putByteString stmt
putCommand (COM_STMT_EXECUTE stmtId fields params) = do
    putWord8 0x17
    putWord32le stmtId
    putWord8 0x00 -- we only use @CURSOR_TYPE_NO_CURSOR@ here
    putWord32le 0  -- const 0
    unless (null params) $ do
        putByteString (makeNullMap params)
        putWord8 0x01    -- always use new-params-bound-flag
        let fts = map fieldType fields
        forM_ fts $ \ t -> put t >> putWord8 0x00 -- always use signed
        forM_ fields put
putCommand (COM_STMT_CLOSE stmtId) = putWord8 0x19 >> putWord32le stmtId
putCommand (COM_STMT_RESET stmtId) = putWord8 0x1A >> putWord32le stmtId
putCommand COM_UNSUPPORTED       = fail "unsupported command"

instance Binary Command where
    get = getCommand
    put = putCommand


--------------------------------------------------------------------------------
--  Prepared statment related

-- | call 'isOK' with this packet return true
data StmtPrepareOK = StmtPrepareOK
    { stmtId :: !Word32
    , stmtColumnCnt :: !Int
    , stmtParamCnt  :: !Int
    , stmtWarnCnt   :: !Int
    } deriving (Show, Eq)

getStmtPrepareOK :: Get StmtPrepareOK
getStmtPrepareOK = do
    _ <- getWord8 -- OK byte
    stmtid <- getWord32le
    cc <- fromIntegral <$> getWord16le
    pc <- fromIntegral <$> getWord16le
    _ <- getWord8 -- reserved
    wc <- fromIntegral <$> getWord16le
    return (StmtPrepareOK stmtid cc pc wc)
