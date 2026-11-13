-- | A send function that escaped its 'StreamedBytes' producer, called from
-- another thread while the parameter is being finished, has to throw rather
-- than add long data behind the execute command, where the server would take
-- it for the value of the statement's next execute.
module EscapedSend (tests) where

import           Control.Concurrent        (forkIO, getNumCapabilities, setNumCapabilities)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception         (bracket, try)
import           Control.Monad             (replicateM, void)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import           Data.IORef                (IORef, atomicModifyIORef', atomicWriteIORef, newIORef,
                                            readIORef)
import           Data.Word                 (Word8)
import           Database.MySQL.Base
import           Database.MySQL.Connection (MySQLConn (..), ReplyState (..))
import qualified System.IO.Streams         as Stream
import           Test.Tasty
import           Test.Tasty.HUnit

data SenderOutcome
    = SenderRefused
    | SentAfterExecute  -- ^ a send begun after executeStmtStreamed returned went through
    deriving (Show, Eq)

data ExecuteProgress = ExecuteRunning | ExecuteReturned

tests :: TestTree
tests = testCase "a send racing its producer's return throws and never writes after the execute" $
    withCapabilities 4 $ do
        trials <- replicateM 300 raceOnce
        assertEqual "trials with long data written after the execute command"
            0 (length (filter ((> 0) . fst) trials))
        assertEqual "trials with a send after the execute that was not refused"
            0 (length (filter ((== SentAfterExecute) . snd) trials))

-- | One execute whose producer forks a sender and returns at once: the long
-- data commands written after the execute command, and how the sender ended.
raceOnce :: IO (Int, SenderOutcome)
raceOnce = do
    written <- newIORef []
    reply <- Stream.fromList [encodeToPacket 1 (OK 0 0 2 0)]
    state <- newIORef RepliesRead
    let conn = MySQLConn reply (recordPacket written) (pure ()) state
    finished <- newEmptyMVar
    progress <- newIORef ExecuteRunning
    _ <- executeStmtStreamed conn 1 [StreamedBytes (racingProducer progress finished)]
    atomicWriteIORef progress ExecuteReturned
    outcome <- takeMVar finished
    packets <- reverse <$> readIORef written
    pure (length (filter (startsWith 0x18) (dropWhile (not . startsWith 0x17) packets)), outcome)

recordPacket :: IORef [Packet] -> Packet -> IO ()
recordPacket written packet = atomicModifyIORef' written (\packets -> (packet : packets, ()))

-- | Fork a sender and return once it has sent its first chunk, so that it is
-- still sending when the parameter is finished.
racingProducer :: IORef ExecuteProgress -> MVar SenderOutcome -> (B.ByteString -> IO ()) -> IO ()
racingProducer progress finished send = do
    started <- newEmptyMVar
    void (forkIO (send chunk >> putMVar started () >> sendUntilRefused progress send >>= putMVar finished))
    takeMVar started

-- | Send until refused, or until a send begun after the execute returned goes
-- through.
sendUntilRefused :: IORef ExecuteProgress -> (B.ByteString -> IO ()) -> IO SenderOutcome
sendUntilRefused progress send = do
    before <- readIORef progress
    sent <- try (send chunk)
    case sent of
        Left (ChunkAfterProducerReturned _) -> pure SenderRefused
        Right () -> case before of
            ExecuteReturned -> pure SentAfterExecute
            ExecuteRunning -> sendUntilRefused progress send

chunk :: B.ByteString
chunk = B.replicate 1024 0x61

-- | The command byte of a packet's payload; every command here fits one packet.
startsWith :: Word8 -> Packet -> Bool
startsWith commandByte packet = fmap fst (L.uncons (pBody packet)) == Just commandByte

withCapabilities :: Int -> IO a -> IO a
withCapabilities count action =
    bracket (getNumCapabilities <* setNumCapabilities count) setNumCapabilities (const action)
