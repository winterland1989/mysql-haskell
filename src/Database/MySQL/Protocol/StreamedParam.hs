{-|
Module      : Database.MySQL.Protocol.StreamedParam
Description : Prepared statement parameters sent as long data
License     : BSD
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

The commands that execute a prepared statement with some of its parameters
read chunk by chunk, see Note [Streamed parameters].
-}
module Database.MySQL.Protocol.StreamedParam
    ( StreamedParam (..)
    , NumberedParams
    , numberParams
    , writeStreamedExecute
    , ChunkAfterProducerReturned (..)
    ) where

import           Control.Concurrent.MVar            (MVar, modifyMVar, modifyMVar_, newMVar)
import           Control.Exception                  (Exception (..), onException, throwIO)
import           Control.Monad                      (unless)
import           Data.Binary.Put
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Word                          (Word16)
import           Database.MySQL.Connection          (writePayload)
import           Database.MySQL.Protocol.Command    (StmtID)
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet     (Packet)

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
-- as a value or a producer, instead of a separate sendLongData beside an execute
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
-- 'Database.MySQL.Base.queryStmtStreamed': a value, or a producer of one in
-- chunks, which go to the server as they come, so the client need not hold
-- the whole value. The server does, and refuses a streamed value longer than
-- its @max_allowed_packet@.
--
-- A producer is handed a send function and calls it with each chunk in turn;
-- the value ends when the producer returns, and an empty chunk is nothing to
-- send. The send function is meant for the producer's thread while the
-- producer runs: called after the producer returned, also from another thread
-- racing its return, it throws 'ChunkAfterProducerReturned' and sends nothing.
-- An exception from the producer propagates and leaves the connection
-- refusing commands with 'Database.MySQL.Base.AbandonedReply'.
--
-- A file, 64 KiB at a time, with @import qualified Data.ByteString as B@,
-- @import Control.Monad (unless)@ and @import System.IO@:
--
-- > fileChunks :: Handle -> (B.ByteString -> IO ()) -> IO ()
-- > fileChunks handle send = do
-- >     chunk <- B.hGetSome handle 65536
-- >     unless (B.null chunk) (send chunk >> fileChunks handle send)
-- >
-- > withFile "backup.tar" ReadMode $ \handle ->
-- >     executeStmtStreamed conn stmt [StreamedBytes (fileChunks handle)]
--
-- A lazy 'Data.ByteString.Lazy.ByteString', also one read lazily with
-- 'Data.ByteString.Lazy.readFile', with @import qualified Data.ByteString.Lazy
-- as L@:
--
-- > StreamedBytes (\send -> mapM_ send (L.toChunks value))
--
-- A conduit source, here with @import qualified Conduit as C@ (conduit 1.3):
--
-- > StreamedBytes $ \send ->
-- >     C.runConduitRes (C.sourceFile path C..| C.mapM_C (C.liftIO . send))
--
-- A streamly @Stream IO ByteString@, with @import qualified
-- Streamly.Data.Stream as Stream@ and @import qualified Streamly.Data.Fold as
-- Fold@ (streamly-core 0.2):
--
-- > StreamedBytes (\send -> Stream.fold (Fold.drainMapM send) chunks)
--
-- A pipes @Producer ByteString IO ()@, with @import qualified Pipes as P@
-- (pipes 4.3):
--
-- > StreamedBytes (\send -> P.runEffect (P.for chunks (P.lift . send)))
--
-- An io-streams @InputStream ByteString@, with @import qualified
-- System.IO.Streams as Streams@ (io-streams 1.5):
--
-- > StreamedBytes $ \send ->
-- >     Streams.makeOutputStream (mapM_ send) >>= Streams.connect input
--
-- @since 1.6.0
data StreamedParam
    = InlineParam !MySQLValue
      -- ^ A value sent with the execute command, as 'Database.MySQL.Base.executeStmt' sends it.
    | StreamedBytes !((ByteString -> IO ()) -> IO ())
      -- ^ A binary string, as 'MySQLBytes' is sent.
    | StreamedText !((Text -> IO ()) -> IO ())
      -- ^ A string, encoded as UTF-8 as 'MySQLText' is.

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
    StreamedBytes producer -> writeLongData stid position producer writePacket
    StreamedText producer -> writeLongData stid position (encodingChunks producer) writePacket

encodingChunks :: ((Text -> IO ()) -> IO ()) -> (ByteString -> IO ()) -> IO ()
encodingChunks producer send = producer (send . encodeUtf8)

-- | The chunks a producer sent that are not yet in a command, @pendingLength@
-- bytes with the last chunk first.
data Sink = Sink !ProducerStatus !Int [ByteString]

data ProducerStatus = ProducerRunning | ProducerReturned

writeLongData :: StmtID -> Word16 -> ((ByteString -> IO ()) -> IO ()) -> (Packet -> IO ()) -> IO ()
writeLongData stid position producer writePacket = do
    sink <- newMVar (Sink ProducerRunning 0 [])
    producer (sendChunk stid position writePacket sink) `onException` closeSink sink
    pending <- closeSink sink
    -- Sent also when empty: an empty value needs a command too, or the server
    -- would look for this parameter's value in the execute command.
    writeLongDataPiece stid position (reverse pending) writePacket

-- | Refuse further sends, returning the chunks not yet sent.
--
-- Decision: a send holds the sink's MVar while it gathers and writes, and
-- closing takes it too, so a send from another thread either completes before
-- the last command and the execute command are written or throws. With an
-- IORef read at a send's start and written at its end, a close in between was
-- overwritten: about 270 of 300 EscapedSend trials wrote long data after the
-- execute.
closeSink :: MVar Sink -> IO [ByteString]
closeSink sink = modifyMVar sink takePending

takePending :: Sink -> IO (Sink, [ByteString])
takePending (Sink _ _ pending) = pure (Sink ProducerReturned 0 [], pending)

-- | The send function a producer gets.
sendChunk :: StmtID -> Word16 -> (Packet -> IO ()) -> MVar Sink -> ByteString -> IO ()
sendChunk stid position writePacket sink chunk =
    modifyMVar_ sink (gatherChunk stid position writePacket chunk)

-- | Gather the chunk into the pending command, writing the commands it completes.
gatherChunk :: StmtID -> Word16 -> (Packet -> IO ()) -> ByteString -> Sink -> IO Sink
gatherChunk stid position writePacket chunk (Sink status pendingLength pending) =
    case status of
        ProducerReturned -> throwIO (ChunkAfterProducerReturned position)
        ProducerRunning ->
            if  | B.null chunk -> pure (Sink ProducerRunning pendingLength pending)
                | pendingLength + B.length chunk < longDataPieceSize ->
                    pure (Sink ProducerRunning (pendingLength + B.length chunk) (chunk : pending))
                | otherwise -> do
                    let (completing, rest) = B.splitAt (longDataPieceSize - pendingLength) chunk
                    writeLongDataPiece stid position (reverse (completing : pending)) writePacket
                    leftover <- writeWholePieces stid position rest writePacket
                    pure (Sink ProducerRunning (B.length leftover) [leftover])

-- | A send function given to a 'StreamedBytes' or 'StreamedText' producer was
-- called after the producer returned, for the parameter at this position
-- (counting from 0). Its value had been sent by then, so the chunk was not.
--
-- @since 1.6.0
newtype ChunkAfterProducerReturned = ChunkAfterProducerReturned Word16
    deriving (Show, Eq)

instance Exception ChunkAfterProducerReturned where
    displayException (ChunkAfterProducerReturned position) =
        "mysql-haskell: a chunk was sent for streamed parameter " ++ show position
        ++ " (counting from 0) after its producer returned, when the parameter's value "
        ++ "had already gone to the server, so the chunk was not sent. The send function "
        ++ "a StreamedBytes or StreamedText producer gets is only valid while the producer runs."

-- | Send whole commands' worth of these bytes, returning the shorter rest.
writeWholePieces :: StmtID -> Word16 -> ByteString -> (Packet -> IO ()) -> IO ByteString
writeWholePieces stid position bytes writePacket =
    if B.length bytes < longDataPieceSize
    then pure bytes
    else do
        let (piece, rest) = B.splitAt longDataPieceSize bytes
        writeLongDataPiece stid position [piece] writePacket
        writeWholePieces stid position rest writePacket

-- | The data one long data command carries; only a parameter's last one
-- carries less.
--
-- Decision: a streamed value goes in commands of 1 MiB whatever the
-- producer's chunk size. Short chunks are gathered: a command per 4 KiB chunk
-- (network's suggested @recv@ size) cost ten times the client's instructions
-- and, on MariaDB 11.4.9, 1.2 to 2 times the wall time, while 64 KiB commands
-- were no faster than 1 MiB ones. Long ones are split: the server closes the
-- connection on a command longer than max_allowed_packet, and 1 MiB is below
-- its default since MySQL 5.6 (4 MiB; 16 MiB on MariaDB, 64 MiB on MySQL 8.0).
longDataPieceSize :: Int
longDataPieceSize = 1048576

writeLongDataPiece :: StmtID -> Word16 -> [ByteString] -> (Packet -> IO ()) -> IO ()
writeLongDataPiece stid position piece = writePayload $ runPut $ do
    putWord8 0x18
    putWord32le stid
    putWord16le position
    mapM_ putByteString piece

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
