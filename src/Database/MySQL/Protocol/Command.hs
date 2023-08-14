{-# OPTIONS_GHC -funbox-strict-fields #-}

{-|
Module      : Database.MySQL.Protocol.Command
Description : MySQL commands
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

Common MySQL commands supports.

-}

module Database.MySQL.Protocol.Command where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Parser
import           Data.Binary.Put
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as L
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
--  Commands

type StmtID = Word32

-- | All support MySQL commands.
--
data Command
    = COM_QUIT                                    -- ^ 0x01
    | COM_INIT_DB        !ByteString              -- ^ 0x02
    | COM_QUERY          !L.ByteString            -- ^ 0x03
    | COM_PING                                    -- ^ 0x0E
    | COM_BINLOG_DUMP    !Word32 !Word16 !Word32 !ByteString -- ^ 0x12
            -- binlog-pos, flags(0x01), server-id, binlog-filename
    | COM_REGISTER_SLAVE !Word32 !ByteString !ByteString !ByteString !Word16 !Word32 !Word32 -- ^ 0x15
            -- server-id, slaves hostname, slaves user, slaves password,  slaves port, replication rank(ignored), master-id(usually 0)
    | COM_STMT_PREPARE   !L.ByteString            -- ^ 0x16 statement
    | COM_STMT_EXECUTE   !StmtID ![MySQLValue] !BitMap -- ^ 0x17 stmtId, params
    | COM_STMT_CLOSE     !StmtID                  -- ^ 0x19 stmtId
    | COM_STMT_RESET     !StmtID                  -- ^ 0x1A stmtId
    | COM_UNSUPPORTED
   deriving (Show, Eq)

putCommand :: Command -> Put
putCommand COM_QUIT              = putWord8 0x01
putCommand (COM_INIT_DB db)      = putWord8 0x02 >> putByteString db
putCommand (COM_QUERY q)         = putWord8 0x03 >> putLazyByteString q
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
putCommand (COM_STMT_PREPARE stmt) = putWord8 0x16 >> putLazyByteString stmt
putCommand (COM_STMT_EXECUTE stid params nullmap) = do
    putWord8 0x17
    putWord32le stid
    putWord8 0x00 -- we only use @CURSOR_TYPE_NO_CURSOR@ here
    putWord32le 1 -- const 1
    unless (null params) $ do
        putByteString (fromBitMap nullmap)
        putWord8 0x01    -- always use new-params-bound-flag
        mapM_ putParamMySQLType params
        forM_ params putBinaryField

putCommand (COM_STMT_CLOSE stid) = putWord8 0x19 >> putWord32le stid
putCommand (COM_STMT_RESET stid) = putWord8 0x1A >> putWord32le stid
putCommand _                     = error "unsupported command"

--------------------------------------------------------------------------------
--  Prepared statment related

-- | call 'isOK' with this packet return true
data StmtPrepareOK = StmtPrepareOK
    { stmtId        :: !StmtID
    , stmtColumnCnt :: !Int
    , stmtParamCnt  :: !Int
    , stmtWarnCnt   :: !Int
    } deriving (Show, Eq)

getStmtPrepareOK :: Get StmtPrepareOK
getStmtPrepareOK = do
    skipN 1 -- OK byte
    stmtid <- getWord32le
    cc <- fromIntegral <$> getWord16le
    pc <- fromIntegral <$> getWord16le
    skipN 1 -- reserved
    wc <- fromIntegral <$> getWord16le
    return (StmtPrepareOK stmtid cc pc wc)
{-# INLINE getStmtPrepareOK #-}
