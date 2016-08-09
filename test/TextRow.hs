{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}

module TextRow where

import Control.Applicative
import Database.MySQL.Base
import Database.MySQL.Protocol
import qualified System.IO.Streams as Stream
import Test.Tasty.HUnit
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

tests :: MySQLConn -> Assertion
tests c = do
    (f, is) <- query_ c "SELECT * FROM test;"

    assertEqual "decode Field types" (columnType <$> f)
        [ MYSQL_TYPE_LONG
        , MYSQL_TYPE_BIT
        , MYSQL_TYPE_TINY
        , MYSQL_TYPE_TINY
        , MYSQL_TYPE_SHORT
        , MYSQL_TYPE_SHORT
        , MYSQL_TYPE_INT24
        , MYSQL_TYPE_INT24
        , MYSQL_TYPE_LONG
        , MYSQL_TYPE_LONG
        , MYSQL_TYPE_LONGLONG
        , MYSQL_TYPE_LONGLONG
        , MYSQL_TYPE_NEWDECIMAL
        , MYSQL_TYPE_FLOAT
        , MYSQL_TYPE_DOUBLE
        , MYSQL_TYPE_DATE
        , MYSQL_TYPE_DATETIME
        , MYSQL_TYPE_TIMESTAMP
        , MYSQL_TYPE_TIME
        , MYSQL_TYPE_YEAR
        , MYSQL_TYPE_STRING
        , MYSQL_TYPE_VAR_STRING
        , MYSQL_TYPE_STRING
        , MYSQL_TYPE_VAR_STRING
        , MYSQL_TYPE_BLOB
        , MYSQL_TYPE_BLOB
        , MYSQL_TYPE_BLOB
        , MYSQL_TYPE_BLOB
        , MYSQL_TYPE_STRING
        , MYSQL_TYPE_STRING
        ]

    Just v <- Stream.read is
    assertEqual "decode NULL values" v
        [ MySQLInt32 0
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        , MySQLNull
        ]

    Stream.skipToEof is

    execute_ c "UPDATE test SET \
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
                \__set        = 'foo,bar' WHERE __id=0;"

    (_, is) <- query_ c "SELECT * FROM test;"
    Just v <- Stream.read is

    assertEqual "decode text protocol" v
        [ MySQLInt32 0
        , MySQLBit 224
        , MySQLInt8 (-128)
        , MySQLInt8U 255
        , MySQLInt16 (-32768)
        , MySQLInt16U 65535
        , MySQLInt32 (-8388608)
        , MySQLInt32U 16777215
        , MySQLInt32 (-2147483648)
        , MySQLInt32U 4294967295
        , MySQLInt64 (-9223372036854775808)
        , MySQLInt64U 18446744073709551615
        , MySQLDecimal 1234567890.0123456789
        , MySQLFloat 3.14159
        , MySQLDouble 3.1415926535
        , MySQLDate (fromGregorian 2016 08 08)
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
        , MySQLTime 1 (TimeOfDay 199 59 59)
        , MySQLYear 1999
        , MySQLText "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLText "韩冬真赞"
        , MySQLText "foo"
        , MySQLText "foo,bar"
        ]

    Stream.skipToEof is

    execute c "UPDATE test SET \
            \__bit        = ?     ,\
            \__tinyInt    = ?     ,\
            \__tinyIntU   = ?     ,\
            \__smallInt   = ?     ,\
            \__smallIntU  = ?     ,\
            \__mediumInt  = ?     ,\
            \__mediumIntU = ?     ,\
            \__int        = ?     ,\
            \__intU       = ?     ,\
            \__bigInt     = ?     ,\
            \__bigIntU    = ?     ,\
            \__decimal    = ?     ,\
            \__float      = ?     ,\
            \__dobule     = ?     ,\
            \__date       = ?     ,\
            \__datetime   = ?     ,\
            \__timestamp  = ?     ,\
            \__time       = ?     ,\
            \__year       = ?     ,\
            \__char       = ?     ,\
            \__varchar    = ?     ,\
            \__binary     = ?     ,\
            \__varbinary  = ?     ,\
            \__tinyblob   = ?     ,\
            \__tinytext   = ?     ,\
            \__blob       = ?     ,\
            \__text       = ?     ,\
            \__enum       = ?     ,\
            \__set        = ? WHERE __id=0;"
                [ MySQLBit 224
                , MySQLInt8 (-128)
                , MySQLInt8U 255
                , MySQLInt16 (-32768)
                , MySQLInt16U 65535
                , MySQLInt32 (-8388608)
                , MySQLInt32U 16777215
                , MySQLInt32 (-2147483648)
                , MySQLInt32U 4294967295
                , MySQLInt64 (-9223372036854775808)
                , MySQLInt64U 18446744073709551615
                , MySQLDecimal 1234567890.0123456789
                , MySQLFloat 3.14159
                , MySQLDouble 3.1415926535
                , MySQLDate (fromGregorian 2016 08 08)
                , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
                , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
                , MySQLTime 1 (TimeOfDay 199 59 59)
                , MySQLYear 1999
                , MySQLText "12345678"
                , MySQLText "韩冬真赞"
                , MySQLBytes "12345678"
                , MySQLBytes "12345678"
                , MySQLBytes "12345678"
                , MySQLText "韩冬真赞"
                , MySQLBytes "12345678"
                , MySQLText "韩冬真赞"
                , MySQLText "foo"
                , MySQLText "foo,bar"
                ]


    (_, is) <- query_ c "SELECT * FROM test;"
    Just v <- Stream.read is

    assertEqual "roundtrip text protocol" v
        [ MySQLInt32 0
        , MySQLBit 224
        , MySQLInt8 (-128)
        , MySQLInt8U 255
        , MySQLInt16 (-32768)
        , MySQLInt16U 65535
        , MySQLInt32 (-8388608)
        , MySQLInt32U 16777215
        , MySQLInt32 (-2147483648)
        , MySQLInt32U 4294967295
        , MySQLInt64 (-9223372036854775808)
        , MySQLInt64U 18446744073709551615
        , MySQLDecimal 1234567890.0123456789
        , MySQLFloat 3.14159
        , MySQLDouble 3.1415926535
        , MySQLDate (fromGregorian 2016 08 08)
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
        , MySQLTime 1 (TimeOfDay 199 59 59)
        , MySQLYear 1999
        , MySQLText "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLText "韩冬真赞"
        , MySQLText "foo"
        , MySQLText "foo,bar"
        ]

    Stream.skipToEof is
