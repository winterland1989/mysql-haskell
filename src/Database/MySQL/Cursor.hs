{-|
Module      : Database.MySQL.Cursor
Description : Fetching a prepared statement's rows from a server-side cursor
Copyright   : (c) Jappie Klooster, 2026
License     : BSD3
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

The packets of a server-side cursor and the stream that fetches its rows in
batches. "Database.MySQL.Base" runs the statement and exports the functions
built on it.

-}
module Database.MySQL.Cursor
    ( CursorBatch (..)
    , CursorRemainder (..)
    , requireRowsPerFetch
    , cursorRowStream
    , cursorExecutePayload
    , fetchPayload
    , cursorExists
    ) where

import           Control.Exception                  (catch, mask_, throwIO)
import           Control.Monad                      (when)
import qualified Data.Binary.Put                    as Binary
import           Data.Bits                          ((.&.))
import qualified Data.ByteString.Lazy               as L
import           Data.IORef                         (IORef, newIORef, readIORef, writeIORef)
import           Data.Word                          (Word8, Word16, Word32)
import           Database.MySQL.Connection
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet
import           System.IO.Streams                  (InputStream)
import qualified System.IO.Streams                  as Stream

-- Note [Server-side cursors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- COM_STMT_EXECUTE with CURSOR_TYPE_READ_ONLY makes MySQL and MariaDB copy the
-- result into a temporary table and answer with the column definitions alone,
-- their EOF packet flagged SERVER_STATUS_CURSOR_EXISTS. Each COM_STMT_FETCH
-- then answers with up to the asked number of rows and an EOF packet flagged
-- SERVER_STATUS_CURSOR_EXISTS, or SERVER_STATUS_LAST_ROW_SENT once the table is
-- used up, which also closes the cursor. A result that fills its last batch
-- exactly takes one more, empty fetch to learn that: 10,000 rows in batches of
-- 100 take 101 fetches. A fetch of 0 rows answers with none and keeps the
-- cursor open, so a stream asking for 0 would fetch forever.
--
-- Decision: each fetch is a command of its own, run through runCommand and
-- read to its end before its rows are handed out. Between fetches the
-- connection is then idle and takes any command, as the servers allow with a
-- cursor open; see Note [Abandoned replies] for an interrupted fetch. Handing
-- a batch's rows out as they arrive would hold the connection at RowsUnread
-- for no gain in memory, which the batch size already bounds.
--
-- Some statements get no cursor even when they ask: a CALL on both servers, an
-- EXPLAIN on MariaDB. Their rows follow the column definitions as without the
-- flag. Decision: read them before returning, as libmysqlclient does, so the
-- stream never holds the connection, rather than hand them out as queryStmt
-- does and break that promise for a few statements.

-- | Throws before anything is sent, see Note [Server-side cursors].
requireRowsPerFetch :: Word32 -> IO ()
requireRowsPerFetch rowsPerFetch = when (rowsPerFetch == 0) $ ioError $ userError $
    "mysql-haskell: a cursor was asked for 0 rows per fetch. The server answers such a "
    ++ "fetch with no rows and keeps the cursor open, so the rows would never come. "
    ++ "Ask for at least 1 row per fetch."

-- | The rows of a cursor fetched but not yet handed out, and whether the
-- server holds more.
data CursorBatch = CursorBatch [Packet] CursorRemainder

data CursorRemainder
    = RowsOnServer  -- ^ the cursor is open, the next fetch gives more rows
    | NoRowsLeft    -- ^ the last row was sent, or a fetch was refused

-- | The row packets of the statement's open cursor, starting with this batch
-- and fetching @rowsPerFetch@ more whenever a batch is used up.
cursorRowStream :: MySQLConn -> StmtID -> Word32 -> CursorBatch -> IO (InputStream Packet)
cursorRowStream conn stid rowsPerFetch firstBatch = do
    batch <- newIORef firstBatch
    Stream.makeInputStream (nextCursorRow conn stid rowsPerFetch batch)

-- | Under 'mask_' an exception cannot land between a fetch and storing its
-- rows, which would lose rows the server already sent; the fetch's blocking
-- reads still take a timeout.
nextCursorRow :: MySQLConn -> StmtID -> Word32 -> IORef CursorBatch -> IO (Maybe Packet)
nextCursorRow conn stid rowsPerFetch batch = mask_ $ do
    CursorBatch rows remainder <- readIORef batch
    case rows of
        row : later -> do
            writeIORef batch (CursorBatch later remainder)
            pure (Just row)
        [] -> case remainder of
            NoRowsLeft -> pure Nothing
            RowsOnServer -> do
                fetched <- fetchCursorRows conn stid rowsPerFetch `catch` endCursor batch
                writeIORef batch fetched
                nextCursorRow conn stid rowsPerFetch batch

-- | The server keeps no cursor open after refusing a fetch.
endCursor :: IORef CursorBatch -> ERRException -> IO a
endCursor batch refusal = do
    writeIORef batch (CursorBatch [] NoRowsLeft)
    throwIO refusal

fetchCursorRows :: MySQLConn -> StmtID -> Word32 -> IO CursorBatch
fetchCursorRows conn@(MySQLConn is os _ _) stid rowsPerFetch = runCommand conn $ do
    writePayload (fetchPayload stid rowsPerFetch) os
    readFetchedRows is []

-- | Read the reply to a COM_STMT_FETCH, rows up to an EOF packet; the rows
-- read so far are kept in reverse.
readFetchedRows :: InputStream Packet -> [Packet] -> IO CursorBatch
readFetchedRows is earlier = do
    q <- readPacket is
    if  | isEOF q -> do
            status <- eofStatus <$> decodeFromPacket q
            if  | lastRowSent status -> pure (CursorBatch (reverse earlier) NoRowsLeft)
                -- An open cursor that sends no rows would be fetched from forever.
                | cursorExists status && not (null earlier) ->
                    pure (CursorBatch (reverse earlier) RowsOnServer)
                | otherwise -> throwIO (UnexpectedPacket q)
        | isERR q -> decodeFromPacket q >>= throwIO . ERRException
        | otherwise -> readFetchedRows is (q : earlier)

-- Decision: COM_STMT_EXECUTE asking for a cursor and COM_STMT_FETCH are
-- written here, not as 'Command' constructors or a flags field: 'Command' is
-- exported, and either would break code matching on it.

-- | 'putCommand''s COM_STMT_EXECUTE with the flags byte, which follows the
-- command byte and the statement id, set to CURSOR_TYPE_READ_ONLY. Patching
-- the byte keeps one encoding of the parameters.
cursorExecutePayload :: StmtID -> [MySQLValue] -> L.ByteString
cursorExecutePayload stid params =
    let (commandAndStatement, flagsAndRest) =
            L.splitAt 5 (Binary.runPut (putCommand (COM_STMT_EXECUTE stid params (makeNullMap params))))
    in commandAndStatement <> L.cons cursorTypeReadOnly (L.drop 1 flagsAndRest)

cursorTypeReadOnly :: Word8
cursorTypeReadOnly = 0x01

fetchPayload :: StmtID -> Word32 -> L.ByteString
fetchPayload stid rowsPerFetch = Binary.runPut $ do
    Binary.putWord8 0x1C
    Binary.putWord32le stid
    Binary.putWord32le rowsPerFetch

-- | SERVER_STATUS_CURSOR_EXISTS
cursorExists :: Word16 -> Bool
cursorExists status = status .&. 0x40 /= 0

-- | SERVER_STATUS_LAST_ROW_SENT
lastRowSent :: Word16 -> Bool
lastRowSent status = status .&. 0x80 /= 0
