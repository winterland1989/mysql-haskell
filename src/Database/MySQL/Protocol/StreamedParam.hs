{-|
Module      : Database.MySQL.Protocol.StreamedParam
Description : Prepared statement parameters sent as long data
License     : BSD
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

The commands that execute a prepared statement with some of its parameters
read from streams, see Note [Streamed parameters].
-}
module Database.MySQL.Protocol.StreamedParam
    ( StreamedParam (..)
    , NumberedParams
    , numberParams
    , writeStreamedExecute
    ) where

import           Control.Monad                      (unless)
import           Data.Binary.Put
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.Text                          as T
import           Data.Word                          (Word16)
import           Database.MySQL.Connection          (writePayload)
import           Database.MySQL.Protocol.Command    (StmtID)
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet     (Packet)
import           System.IO.Streams                  (InputStream)
import qualified System.IO.Streams                  as Stream

-- Note [Streamed parameters]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A parameter can reach the server ahead of COM_STMT_EXECUTE, in any number of
-- COM_STMT_SEND_LONG_DATA commands, [0x18][statement id:4][parameter
-- number:2][data], each appending its data to the parameter's value. The
-- server answers none of them. The execute command then declares a string or
-- blob type for that parameter and carries no value for it. The server drops
-- the data once the statement ran, and on COM_STMT_RESET.
--
-- The execute command has to leave out exactly the values that were streamed.
-- One written anyway is taken for the next parameter's value: with data
-- streamed for the first parameter of @SELECT ?, ?@ and both values in the
-- execute command, MySQL 8.0.45 and MariaDB 11.4.9 answer the streamed data
-- and the first value.
--
-- Decision: one function streams the data and executes, taking each parameter
-- as a value or a stream, instead of a separate sendLongData beside an execute
-- told which parameters were sent. Apart, every caller has to keep the two in
-- step, and data sent for an execute that never came would shift the values
-- of the statement's next execute.
--
-- The server holds each streamed value to the session's max_allowed_packet:
-- data that would make it longer is dropped, and the next execute of the
-- statement fails with error 1105, as does every one after it until a
-- COM_STMT_RESET. Values in the execute command share one max_allowed_packet,
-- so streamed parameters can together exceed it where those cannot. Measured
-- on both servers above, and read in their sql_prepare.cc.

-- | A parameter of 'Database.MySQL.Base.executeStmtStreamed' and
-- 'Database.MySQL.Base.queryStmtStreamed': a value, or a stream whose chunks
-- are sent to the server as they are read, so the client need not hold the
-- whole value. The server does, and refuses a streamed value longer than its
-- @max_allowed_packet@.
--
-- @since 1.6.0
data StreamedParam
    = InlineParam !MySQLValue
      -- ^ A value sent with the execute command, as 'Database.MySQL.Base.executeStmt' sends it.
    | StreamedBytes !(InputStream ByteString)
      -- ^ A binary string, as 'MySQLBytes' is sent.
    | StreamedText !(InputStream ByteString)
      -- ^ A string, as 'MySQLText' is sent, in the connection's character set
      -- (@character_set_client@): UTF-8 on a connection made with
      -- 'Database.MySQL.Base.defaultConnectInfoMB4'.

-- | Parameters with their positions in the statement, which the long data
-- commands name as a 'Word16'.
newtype NumberedParams = NumberedParams [(Word16, StreamedParam)]

-- | Nothing for more parameters than the 65535 a prepared statement can have.
numberParams :: [StreamedParam] -> Maybe NumberedParams
numberParams params = NumberedParams <$> numberParamsFrom 0 params

numberParamsFrom :: Word16 -> [StreamedParam] -> Maybe [(Word16, StreamedParam)]
numberParamsFrom position params = case params of
    [] -> Just []
    param : rest ->
        if position == maxBound
        then Nothing
        else ((position, param) :) <$> numberParamsFrom (position + 1) rest

-- | The long data of every streamed parameter, then the execute command.
-- See Note [Streamed parameters].
writeStreamedExecute :: StmtID -> NumberedParams -> (Packet -> IO ()) -> IO ()
writeStreamedExecute stid (NumberedParams numbered) writePacket = do
    mapM_ (writeParamData stid writePacket) numbered
    writePayload (runPut (putStreamedExecute stid (map snd numbered))) writePacket

writeParamData :: StmtID -> (Packet -> IO ()) -> (Word16, StreamedParam) -> IO ()
writeParamData stid writePacket (position, param) = case param of
    InlineParam _ -> pure ()
    StreamedBytes chunks -> writeLongData stid position chunks writePacket
    StreamedText chunks -> writeLongData stid position chunks writePacket

writeLongData :: StmtID -> Word16 -> InputStream ByteString -> (Packet -> IO ()) -> IO ()
writeLongData stid position chunks writePacket = do
    first <- Stream.read chunks
    case first of
        -- An empty value still needs a command, or the server would look for
        -- this parameter's value in the execute command.
        Nothing -> writeLongDataPiece stid position B.empty writePacket
        Just chunk -> writeChunks stid position chunk chunks writePacket

writeChunks :: StmtID -> Word16 -> ByteString -> InputStream ByteString -> (Packet -> IO ()) -> IO ()
writeChunks stid position chunk chunks writePacket = do
    writePieces stid position chunk writePacket
    next <- Stream.read chunks
    case next of
        Nothing -> pure ()
        Just chunk' -> writeChunks stid position chunk' chunks writePacket

writePieces :: StmtID -> Word16 -> ByteString -> (Packet -> IO ()) -> IO ()
writePieces stid position chunk writePacket =
    if B.length chunk <= longDataPieceSize
    then writeLongDataPiece stid position chunk writePacket
    else do
        let (piece, rest) = B.splitAt longDataPieceSize chunk
        writeLongDataPiece stid position piece writePacket
        writePieces stid position rest writePacket

-- | The most data one long data command carries.
--
-- Decision: a chunk is split into commands of at most 1 MiB, below every
-- default max_allowed_packet since MySQL 5.6 (4 MiB; 16 MiB on MariaDB,
-- 64 MiB on MySQL 8.0). The server refuses a command longer than
-- max_allowed_packet by closing the connection, while long data that
-- outgrows it in many commands only fails the execute. Sending each chunk
-- whole, in as many packets as it needs, would make the caller's chunk size
-- decide which of the two happens.
longDataPieceSize :: Int
longDataPieceSize = 1048576

writeLongDataPiece :: StmtID -> Word16 -> ByteString -> (Packet -> IO ()) -> IO ()
writeLongDataPiece stid position piece = writePayload $ runPut $ do
    putWord8 0x18
    putWord32le stid
    putWord16le position
    putByteString piece

-- | COM_STMT_EXECUTE for these parameters, written as for
-- 'Database.MySQL.Protocol.Command.COM_STMT_EXECUTE' but without the values
-- of the streamed ones.
putStreamedExecute :: StmtID -> [StreamedParam] -> Put
putStreamedExecute stid params = do
    putWord8 0x17
    putWord32le stid
    putWord8 0x00 -- CURSOR_TYPE_NO_CURSOR
    putWord32le 1 -- iteration count
    unless (null params) $ do
        let declared = map declaredValue params
        putByteString (fromBitMap (makeNullMap declared))
        putWord8 0x01 -- new-params-bound-flag
        mapM_ putParamMySQLType declared
        mapM_ putInlineValue params

-- | The value whose type and NULL bit the execute command gives this
-- parameter: a streamed one is declared as an empty value of its kind.
declaredValue :: StreamedParam -> MySQLValue
declaredValue param = case param of
    InlineParam value -> value
    StreamedBytes _ -> MySQLBytes B.empty
    StreamedText _ -> MySQLText T.empty

putInlineValue :: StreamedParam -> Put
putInlineValue param = case param of
    InlineParam value -> putBinaryField value
    StreamedBytes _ -> pure ()
    StreamedText _ -> pure ()
