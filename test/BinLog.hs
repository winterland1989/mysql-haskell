{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}

module BinLog where

import Control.Applicative
import Control.Monad
import Control.Exception
import Database.MySQL.Base
import Database.MySQL.Protocol
import Database.MySQL.BinLog
import qualified System.IO.Streams as Stream
import Test.Tasty.HUnit
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Text.Encoding (encodeUtf8)

eventProducer :: IO ()
eventProducer = do
    c <- connect defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    execute_ c q1
    return ()

tests :: MySQLConn -> Assertion
tests c = do
    Just blt <- getLastBinLogTracker c
    x@(fd, _, _) <- dumpBinLog c 1002 blt False
    rowEventStream <- decodeRowBinLogEvent x

    Just (RowQueryEvent _ qe) <- Stream.read rowEventStream
    assertEqual "decode query event" (qQuery' qe) q1

    Just (RowUpdateEvent _ tme ue) <- Stream.read rowEventStream
    assertEqual "decode update event cloumn" (updateColumnCnt ue) 30
    assertEqual "decode update event rows" (updateRowData ue)
        [
            (
                [ BinLogLong 0
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                , BinLogNull
                ], [ BinLogLong 0
                   , BinLogBit 224
                   , BinLogTiny (-128)
                   , BinLogTiny (-1)
                   , BinLogShort (-32768)
                   , BinLogShort (-1)
                   , BinLogInt24 (-8388608)
                   , BinLogInt24 (-1)
                   , BinLogLong (-2147483648)
                   , BinLogLong (-1)
                   , BinLogLongLong (-9223372036854775808)
                   , BinLogLongLong (-1)
                   , BinLogNewDecimal 1.2345678900123456789e9
                   , BinLogFloat 3.14159
                   , BinLogDouble 3.1415926535
                   , BinLogDate 103 24 56
                   , BinLogDateTime2 47 5 8 25 61 59 0
                   , BinLogTimeStamp2 1470648359 0
                   , BinLogTime2 0 824 4 5 0
                   , BinLogYear 1999
                   , BinLogBytes "12345678"
                   , BinLogBytes (encodeUtf8 "韩冬真赞")
                   , BinLogBytes "12345678"
                   , BinLogBytes "12345678"
                   , BinLogBytes "12345678"
                   , BinLogBytes (encodeUtf8 "韩冬真赞")
                   , BinLogBytes "12345678"
                   , BinLogBytes (encodeUtf8 "韩冬真赞")
                   , BinLogEnum 1
                   , BinLogSet 3
                   ]
            )
        ]

q1 :: Query
q1 =  "UPDATE test SET \
            \__bit        = b'11100000'                            ,\
            \__tinyInt    = -128                                   ,\
            \__tinyIntU   = 255                                    ,\
            \__smallInt   = -32768                                 ,\
            \__smallIntU  = 65535                                  ,\
            \__mediumInt  = -8388608                               ,\
            \__mediumIntU = 16777215                               ,\
            \__int        = -2147483648                            ,\
            \__intU       = 4294967295                             ,\
            \__bigInt     = -9223372036854775808                   ,\
            \__bigIntU    = 18446744073709551615                   ,\
            \__decimal    = 1234567890.0123456789                  ,\
            \__float      = 3.14159                                ,\
            \__dobule     = 3.1415926535                           ,\
            \__date       = '2016-08-08'                           ,\
            \__datetime   = '2016-08-08 17:25:59'                  ,\
            \__timestamp  = '2016-08-08 17:25:59'                  ,\
            \__time       = '-199:59:59'                           ,\
            \__year       = 1999                                   ,\
            \__char       = '12345678'                             ,\
            \__varchar    = '韩冬真赞'                             ,\
            \__binary     = '12345678'                             ,\
            \__varbinary  = '12345678'                             ,\
            \__tinyblob   = '12345678'                             ,\
            \__tinytext   = '韩冬真赞'                             ,\
            \__blob       = '12345678'                             ,\
            \__text       = '韩冬真赞'                             ,\
            \__enum       = 'foo'                                  ,\
            \__set        = 'foo,bar' WHERE __id=0"
