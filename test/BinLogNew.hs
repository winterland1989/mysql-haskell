{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BinLogNew where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Database.MySQL.Base
import           Database.MySQL.BinLog
import qualified System.IO.Streams     as Stream
import           Test.Tasty.HUnit

eventProducer :: IO ()
eventProducer = do
    c <- connect defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    execute_ c q1
    execute_ c q2
    execute_ c q3
    return ()

tests :: MySQLConn -> Assertion
tests c = do
    Just blt <- getLastBinLogTracker c
    x@(fd, _, _) <- dumpBinLog c 1002 blt False
    rowEventStream <- decodeRowBinLogEvent x

    let Just t = parseTimeM True defaultTimeLocale  "%F %T%Q" "2016-08-08 17:25:59.1234" :: Maybe LocalTime
    z <- getCurrentTimeZone
    let timestamp = round $ utcTimeToPOSIXSeconds (localTimeToUTC z t)

    Just (RowUpdateEvent _ _ tme ue) <- Stream.read rowEventStream
    assertEqual "decode update event cloumn" (updateColumnCnt ue) 4
    assertEqual "decode update event rows" (updateRowData ue)
        [
            (
                [ BinLogLong 0
                , BinLogNull
                , BinLogNull
                , BinLogNull
                ], [ BinLogLong 0
                   , BinLogDateTime2 2016 8 8 17 25 59 120000
                   , BinLogTimeStamp2 timestamp 123400
                   , BinLogTime2 0 199 59 59 123456
                   ]
            )
        ]

    Just (RowUpdateEvent _ _ tme ue) <- Stream.read rowEventStream
    assertEqual "decode update event rows" (updateRowData ue)
        [
            (
                [ BinLogLong 0
                  , BinLogDateTime2 2016 8 8 17 25 59 120000
                  , BinLogTimeStamp2 timestamp 123400
                  , BinLogTime2 0 199 59 59 123456
                ], [ BinLogLong 0
                  , BinLogDateTime2 2016 8 8 17 25 59 100000
                  , BinLogTimeStamp2 timestamp 123000
                  , BinLogTime2 1 199 59 59 123450
                ]
            )
        ]

    Just (RowUpdateEvent _ _ tme ue) <- Stream.read rowEventStream
    assertEqual "decode update event rows" (updateRowData ue)
        [
            (
                [ BinLogLong 0
                  , BinLogDateTime2 2016 8 8 17 25 59 100000
                  , BinLogTimeStamp2 timestamp 123000
                  , BinLogTime2 1 199 59 59 123450
                ], [ BinLogLong 0
                  , BinLogNull
                  , BinLogNull
                  , BinLogTime2 0 0 59 59 123450
                ]
            )
        ]

q1 :: Query
q1 = "UPDATE test_new SET \
     \__datetime   = '2016-08-08 17:25:59.12'                  ,\
     \__timestamp  = '2016-08-08 17:25:59.1234'                ,\
     \__time       = '-199:59:59.123456' WHERE __id=0;"

q2 :: Query
q2 = "UPDATE test_new SET \
     \__datetime   = '2016-08-08 17:25:59.1'                  ,\
     \__timestamp  = '2016-08-08 17:25:59.123'                ,\
     \__time       = '199:59:59.12345' WHERE __id=0;"


q3 :: Query
q3 = "UPDATE test_new SET \
     \__datetime   = null                  ,\
     \__timestamp  = null                  ,\
     \__time       = '-00:59:59.12345' WHERE __id=0;"
