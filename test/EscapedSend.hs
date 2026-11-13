-- | A send function that escaped its 'StreamedBytes' producer, called from
-- another thread while the parameter is being finished, has to either get its
-- chunk into the parameter's value, ahead of the execute command, or throw.
-- Written after the execute command, the server takes the chunk for the
-- value of the statement's next execute; kept and never written, it is lost.
module EscapedSend (tests) where

import           Control.Concurrent        (forkIO, getNumCapabilities, setNumCapabilities)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception         (bracket, try)
import           Control.Monad             (replicateM, void)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import           Data.Int                  (Int64)
import           Data.IORef                (IORef, atomicModifyIORef', atomicWriteIORef, newIORef,
                                            readIORef)
import           Data.Word                 (Word8)
import           Database.MySQL.Base
import           Database.MySQL.Connection (MySQLConn (..), ReplyState (..))
import qualified System.IO.Streams         as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

-- | What one execute with a racing sender left on the connection.
data Trial = Trial
    { acceptedBytes        :: !Integer  -- ^ the chunks of sends that returned normally
    , bytesBeforeExecute   :: !Integer  -- ^ long data written ahead of the execute command
    , commandsAfterExecute :: !Int      -- ^ long data commands written after it
    }

data ExecuteProgress = ExecuteRunning | ExecuteReturned

-- | A written packet's command byte and length. Its data is dropped: kept, the
-- long data commands of 200 trials of 2 MB chunks held gigabytes.
data WrittenPacket = WrittenPacket !(Maybe Word8) !Int64

tests :: TestTree
tests = testGroup "a send racing its producer's return"
    [ raceCase 1 1000 100, raceCase 1 200 2000000, raceCase 4 1000 100, raceCase 4 200 2000000 ]

-- | Trials at a fixed number of capabilities, whatever the binary's @-N@, with
-- chunks shorter and longer than a long data command.
raceCase :: Int -> Int -> Int -> TestTree
raceCase capabilities trialCount chunkSize =
    testCase (show trialCount ++ " trials, " ++ show chunkSize ++ "-byte chunks, "
              ++ show capabilities ++ " capabilities") $
        withCapabilities capabilities $ do
            trials <- replicateM trialCount (raceOnce (B.replicate chunkSize 0x61))
            assertEqual "trials that (lost a chunk a send had taken, wrote long data after the execute)"
                (0, 0)
                (length (filter lostChunks trials), length (filter ((> 0) . commandsAfterExecute) trials))

lostChunks :: Trial -> Bool
lostChunks trial = acceptedBytes trial /= bytesBeforeExecute trial

-- | One execute whose producer forks a sender and returns while it sends.
raceOnce :: B.ByteString -> IO Trial
raceOnce chunk = do
    written <- newIORef []
    reply <- Stream.fromList [encodeToPacket 1 (OK 0 0 2 0)]
    state <- newIORef RepliesRead
    accepted <- newIORef 0
    progress <- newIORef ExecuteRunning
    finished <- newEmptyMVar
    let conn = MySQLConn reply (recordPacket written) (pure ()) state
    _ <- executeStmtStreamed conn 1 [StreamedBytes (racingProducer chunk accepted progress finished)]
    atomicWriteIORef progress ExecuteReturned
    takeMVar finished
    (beforeExecute, fromExecute) <- break (startsWith 0x17) . reverse <$> readIORef written
    total <- readIORef accepted
    -- Forced here, or replicateM keeps every trial's packets until the assertion.
    pure $! Trial
        { acceptedBytes = total
        , bytesBeforeExecute = sum (map longDataBytes (filter (startsWith 0x18) beforeExecute))
        , commandsAfterExecute = length (filter (startsWith 0x18) fromExecute)
        }

recordPacket :: IORef [WrittenPacket] -> Packet -> IO ()
recordPacket written packet =
    let summary = WrittenPacket (fmap fst (L.uncons (pBody packet))) (pLen packet)
    in summary `seq` atomicModifyIORef' written (\packets -> (summary : packets, ()))

-- | Fork a sender and return once it has sent its first chunk, so that it is
-- still sending when the parameter is finished.
racingProducer :: B.ByteString -> IORef Integer -> IORef ExecuteProgress -> MVar () -> (B.ByteString -> IO ()) -> IO ()
racingProducer chunk accepted progress finished send = do
    started <- newEmptyMVar
    void $ forkIO $ do
        send chunk
        addAccepted accepted chunk
        putMVar started ()
        sendUntilRefused chunk accepted progress send
        putMVar finished ()
    takeMVar started

-- | Send until refused, or until a send begun after the execute returned went
-- through, which the byte counts then show.
sendUntilRefused :: B.ByteString -> IORef Integer -> IORef ExecuteProgress -> (B.ByteString -> IO ()) -> IO ()
sendUntilRefused chunk accepted progress send = do
    before <- readIORef progress
    sent <- try (send chunk)
    case sent of
        Left (ChunkAfterProducerReturned _) -> pure ()
        Right () -> do
            addAccepted accepted chunk
            case before of
                ExecuteReturned -> pure ()
                ExecuteRunning -> sendUntilRefused chunk accepted progress send

addAccepted :: IORef Integer -> B.ByteString -> IO ()
addAccepted accepted chunk = atomicModifyIORef' accepted (\total -> (total + toInteger (B.length chunk), ()))

-- | The data of a long data command, after its 7 header bytes.
longDataBytes :: WrittenPacket -> Integer
longDataBytes (WrittenPacket _ len) = toInteger len - 7

-- | The command byte of a packet's payload; every command here fits one packet.
startsWith :: Word8 -> WrittenPacket -> Bool
startsWith commandByte (WrittenPacket firstByte _) = firstByte == Just commandByte

withCapabilities :: Int -> IO a -> IO a
withCapabilities count action =
    bracket (getNumCapabilities <* setNumCapabilities count) setNumCapabilities (const action)
