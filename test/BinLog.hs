{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BinLog where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Text.Encoding    (encodeUtf8)
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
    return ()

tests :: MySQLConn -> Assertion
tests c = do
    Just blt <- getLastBinLogTracker c
    x@(fd, _, _) <- dumpBinLog c 1002 blt False
    rowEventStream <- decodeRowBinLogEvent x

    let Just t = parseTimeM True defaultTimeLocale  "%F %T" "2016-08-08 17:25:59" :: Maybe LocalTime
    z <- getCurrentTimeZone
    let timestamp = round $ utcTimeToPOSIXSeconds (localTimeToUTC z t)

    Just (RowUpdateEvent _ _ tme ue) <- Stream.read rowEventStream
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
                , BinLogDate 2016 8 8
                , BinLogDateTime 2016 8 8 17 25 59
                , BinLogTimeStamp timestamp
                , BinLogTime 0 199 59 59
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

    Just (RowUpdateEvent _ _ tme ue) <- Stream.read rowEventStream
    assertEqual "decode update event rows" (updateRowData ue)
        [
            (
                [ BinLogLong 0
                , BinLogBit 224   -- 0b11100000
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
                , BinLogDate 2016 8 8
                , BinLogDateTime 2016 8 8 17 25 59
                , BinLogTimeStamp timestamp
                , BinLogTime 0 199 59 59
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
                ],  [ BinLogLong 0
                , BinLogBit 57514   -- 0b1110000010101010
                , BinLogTiny (-1)
                , BinLogTiny (-1)
                , BinLogShort (-32768)
                , BinLogShort (-1)
                , BinLogInt24 (-1)
                , BinLogInt24 (-1)
                , BinLogLong (-1)
                , BinLogLong (-1)
                , BinLogLongLong (-9223372036854775808)
                , BinLogLongLong (-1)
                , BinLogNewDecimal -1.2345678900123456789e9
                , BinLogFloat -3.14159
                , BinLogDouble -3.1415926535
                , BinLogDate 1800 8 8
                , BinLogDateTime 1800 8 8 17 25 59
                , BinLogTimeStamp timestamp
                , BinLogTime 1 199 59 59
                , BinLogYear 1901
                , BinLogBytes "12345678"
                , BinLogBytes (encodeUtf8 "韩冬真赞")
                , BinLogBytes "12345678"
                , BinLogBytes "12345678"
                , BinLogBytes "12345678"
                , BinLogBytes (encodeUtf8 "韩冬真赞")
                , BinLogBytes "12345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123"
                , BinLogBytes (encodeUtf8 "韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞")
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
            \__double     = 3.1415926535                           ,\
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


q2 :: Query
q2 =  "UPDATE test SET \
            \__bit        = b'1110000010101010'                    ,\
            \__tinyInt    = -1                                     ,\
            \__tinyIntU   = 255                                    ,\
            \__smallInt   = -32768                                 ,\
            \__smallIntU  = 65535                                  ,\
            \__mediumInt  = -1                                     ,\
            \__mediumIntU = 16777215                               ,\
            \__int        = -1                                     ,\
            \__intU       = 4294967295                             ,\
            \__bigInt     = -9223372036854775808                   ,\
            \__bigIntU    = 18446744073709551615                   ,\
            \__decimal    = -1234567890.0123456789                 ,\
            \__float      = -3.14159                                ,\
            \__double     = -3.1415926535                           ,\
            \__date       = '1800-08-08'                           ,\
            \__datetime   = '1800-08-08 17:25:59'                  ,\
            \__timestamp  = '2016-08-08 17:25:59'                  ,\
            \__time       = '199:59:59'                            ,\
            \__year       = 1901                                   ,\
            \__char       = '12345678'                             ,\
            \__varchar    = '韩冬真赞'                             ,\
            \__binary     = '12345678'                             ,\
            \__varbinary  = '12345678'                             ,\
            \__tinyblob   = '12345678'                             ,\
            \__tinytext   = '韩冬真赞'                             ,\
            \__blob       = '12345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123456781234567812345678123'                             ,\
            \__text       = '韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞韩冬真赞'                              ,\
            \__enum       = 'foo'                                  ,\
            \__set        = 'foo,bar' WHERE __id=0"
