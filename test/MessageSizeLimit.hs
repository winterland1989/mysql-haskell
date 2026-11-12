-- | A connection with a small 'messageSizeLimit' refuses a large row, over
-- every transport, and the default limit reads it.
module MessageSizeLimit (tests) where

import           Control.Exception          (bracket, try)
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Int                   (Int64)
import qualified Data.Text                  as T
import           Database.MySQL.Base
import qualified Database.MySQL.TLS         as TLS
import qualified System.IO.Streams          as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUser                   (testConnectInfo)

tests :: Maybe FilePath -> Maybe FilePath -> TestTree
tests socketPath caPath = testGroup "message size limit" $
    [ testCase "a row larger than the limit raises MessageTooLarge" $
        refusedRow (tcp (limitOf 1048576)) 2097152 (MessageTooLarge 1048576 2097156)
    , testCase "a row continued past a limit above 16 MiB raises MessageTooLarge" $
        refusedRow (tcp (limitOf 20971520)) 25165824 (MessageTooLarge 20971520 25165833)
    , testCase "the default limit reads that row" $
        bracket (tcp defaultConnectionLimits) close $ \conn -> do
            value <- selectRepeat conn 25165824
            case value of
                Left refusal -> assertFailure ("refused: " ++ show refusal)
                Right text -> assertEqual "length of the value" 25165824 (T.length text)
    ]
    ++ maybe [] (pure . unixRefusal) socketPath
    ++ maybe [] (pure . tlsRefusal) caPath

unixRefusal :: FilePath -> TestTree
unixRefusal socketPath = testCase "over a Unix socket" $
    refusedRow (unix socketPath (limitOf 1048576)) 2097152 (MessageTooLarge 1048576 2097156)

tlsRefusal :: FilePath -> TestTree
tlsRefusal caPath = testCase "over TLS" $
    refusedRow (tls caPath (limitOf 1048576)) 2097152 (MessageTooLarge 1048576 2097156)

limitOf :: Int64 -> ConnectionLimits
limitOf limit = defaultConnectionLimits { messageSizeLimit = limit }

tcp :: ConnectionLimits -> IO MySQLConn
tcp limits = connectWithLimits limits testConnectInfo

unix :: FilePath -> ConnectionLimits -> IO MySQLConn
unix socketPath limits = connectUnixSocketWithLimits limits socketPath testConnectInfo

tls :: FilePath -> ConnectionLimits -> IO MySQLConn
tls caPath limits = do
    params <- TLS.makeClientParams (TLS.CustomCAStore caPath)
    TLS.connectWithLimits limits testConnectInfo (params, "Winter")

-- | Selecting a value of this many bytes raises this refusal, after which the
-- connection refuses commands.
refusedRow :: IO MySQLConn -> Int -> MessageTooLarge -> Assertion
refusedRow open size expected = bracket open close $ \conn -> do
    value <- selectRepeat conn size
    case value of
        Left refusal -> assertEqual "refusal" expected refusal
        Right text -> assertFailure ("read a value of " ++ show (T.length text) ++ " characters")
    next <- try (execute_ conn "DO 1")
    case next of
        Left AbandonedReply -> pure ()
        Right ok -> assertFailure ("the connection took another command: " ++ show ok)

selectRepeat :: MySQLConn -> Int -> IO (Either MessageTooLarge T.Text)
selectRepeat conn size = try $ do
    (_, rows) <- query_ conn (Query (LC.pack ("SELECT REPEAT('a', " ++ show size ++ ")")))
    values <- Stream.toList rows
    case values of
        [[MySQLText value]] -> pure value
        other -> assertFailure ("rows: " ++ take 200 (show other))
