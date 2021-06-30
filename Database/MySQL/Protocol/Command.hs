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
import           Data.Bits
import           Data.Word
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet
import           GHC.Generics
import           Z.IO.Exception
import qualified Z.Data.Parser          as P
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V

--------------------------------------------------------------------------------
--  Commands

type StmtID = Word32

-- | All support MySQL commands.
--
data Command
    = COM_QUIT                                    -- ^ 0x01
    | COM_INIT_DB        !V.Bytes                 -- ^ 0x02
    | COM_QUERY          !V.Bytes                 -- ^ 0x03
    | COM_PING                                    -- ^ 0x0E
    | COM_BINLOG_DUMP    !Word32 !Word16 !Word32 !V.Bytes -- ^ 0x12
            -- binlog-pos, flags(0x01), server-id, binlog-filename
    | COM_REGISTER_SLAVE !Word32 !V.Bytes !V.Bytes !V.Bytes !Word16 !Word32 !Word32 -- ^ 0x15
            -- server-id, slaves hostname, slaves user, slaves password,  slaves port, replication rank(ignored), master-id(usually 0)
    | COM_STMT_PREPARE   !V.Bytes                 -- ^ 0x16 statement
    | COM_STMT_EXECUTE   !StmtID ![MySQLValue] !BitMap -- ^ 0x17 stmtId, params
    | COM_STMT_CLOSE     !StmtID                  -- ^ 0x19 stmtId
    | COM_STMT_RESET     !StmtID                  -- ^ 0x1A stmtId
    | COM_UNSUPPORTED
   deriving (Show, Eq)

encodeCommand :: Command -> B.Builder ()
encodeCommand COM_QUIT              = B.word8 0x01
encodeCommand (COM_INIT_DB db)      = B.word8 0x02 >> B.bytes db
encodeCommand (COM_QUERY q)         = B.word8 0x03 >> B.bytes q
encodeCommand COM_PING              = B.word8 0x0E
encodeCommand (COM_BINLOG_DUMP pos flags sid fname) = do
    B.word8 0x12
    B.encodePrimLE pos
    B.encodePrimLE flags
    B.encodePrimLE sid
    B.bytes fname
encodeCommand (COM_REGISTER_SLAVE sid shost susr spass sport rrank mid) = do
    B.word8 0x15
    B.encodePrimLE sid
    encodeLenEncBytes shost
    encodeLenEncBytes susr
    encodeLenEncBytes spass
    B.encodePrimLE sport
    B.encodePrimLE rrank
    B.encodePrimLE mid
encodeCommand (COM_STMT_PREPARE stmt) = B.word8 0x16 >> B.bytes stmt
encodeCommand (COM_STMT_EXECUTE stid params nullmap) = do
    B.word8 0x17
    B.encodePrimLE stid
    B.word8 0x00 -- we only use @CURSOR_TYPE_NO_CURSOR@ here
    B.encodePrimLE @Word32 1 -- const 1
    unless (null params) $ do
        B.bytes (fromBitMap nullmap)
        B.word8 0x01    -- always use new-params-bound-flag
        mapM_ encodeParamMySQLType params
        forM_ params encodeBinaryField

encodeCommand (COM_STMT_CLOSE stid) = B.word8 0x19 >> B.encodePrimLE stid
encodeCommand (COM_STMT_RESET stid) = B.word8 0x1A >> B.encodePrimLE stid
encodeCommand _                     = error "unsupported command"

--------------------------------------------------------------------------------
--  Prepared statment related

-- | call 'isOK' with this packet return true
data StmtPrepareOK = StmtPrepareOK
    { stmtId        :: !StmtID
    , stmtColumnCnt :: !Word16
    , stmtParamCnt  :: !Word16
    , stmtWarnCnt   :: !Word16
    } deriving (Show, Eq)

decodeStmtPrepareOK :: P.Parser StmtPrepareOK
{-# INLINE decodeStmtPrepareOK #-}
decodeStmtPrepareOK = do
    P.skipWord8 -- OK byte
    stmtid <- P.decodePrimLE
    cc <- P.decodePrimLE
    pc <- P.decodePrimLE
    P.skipWord8 -- reserved
    wc <- P.decodePrimLE
    return (StmtPrepareOK stmtid cc pc wc)
