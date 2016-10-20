{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BinaryRow where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Vector as V

tests :: MySQLConn -> Assertion
tests c = do
    selStmt <- prepareStmt c "SELECT * FROM test"

    (f, is) <- queryStmt c selStmt []
    assertEqual "decode Field types" (columnType <$> f)
        [ mySQLTypeLong
        , mySQLTypeBit
        , mySQLTypeTiny
        , mySQLTypeTiny
        , mySQLTypeShort
        , mySQLTypeShort
        , mySQLTypeInt24
        , mySQLTypeInt24
        , mySQLTypeLong
        , mySQLTypeLong
        , mySQLTypeLongLong
        , mySQLTypeLongLong
        , mySQLTypeNewDecimal
        , mySQLTypeFloat
        , mySQLTypeDouble
        , mySQLTypeDate
        , mySQLTypeDateTime
        , mySQLTypeTimestamp
        , mySQLTypeTime
        , mySQLTypeYear
        , mySQLTypeString
        , mySQLTypeVarString
        , mySQLTypeString
        , mySQLTypeVarString
        , mySQLTypeBlob
        , mySQLTypeBlob
        , mySQLTypeBlob
        , mySQLTypeBlob
        , mySQLTypeString
        , mySQLTypeString
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
--------------------------------------------------------------------------------
    let bitV = 43744 -- 0b1010101011100000

    execute_ c "UPDATE test SET \
                \__bit        = b'1010101011100000'                    ,\
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

    (_, is) <- queryStmt c selStmt []
    Just v <- Stream.read is

    assertEqual "decode binary protocol" v
        [ MySQLInt32 0
        , MySQLBit bitV
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
        , MySQLText "foo,bar"]
    Stream.skipToEof is

    (_, is') <- queryStmtVector c selStmt []
    Just v' <- Stream.read is'
    Stream.skipToEof is'

    assertEqual "decode binary protocol(queryStmtVector)" v (V.toList v')

--------------------------------------------------------------------------------
    updStmt <- prepareStmt c
            "UPDATE test SET \
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
            \__double     = ?     ,\
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
            \__set        = ? WHERE __id=0"

    executeStmt c updStmt
                [ MySQLBit bitV
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



    (_, is) <- queryStmt c selStmt []
    Just v <- Stream.read is

    assertEqual "roundtrip binary protocol" v
        [ MySQLInt32 0
        , MySQLBit bitV
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
--------------------------------------------------------------------------------
    execute_ c "UPDATE test SET \
        \__mediumInt  = null         ,\
        \__double     = null         ,\
        \__text = null WHERE __id=0"

    (_, is) <- queryStmt c selStmt []
    Just v <- Stream.read is

    assertEqual "decode binary protocol with null" v
        [ MySQLInt32 0
        , MySQLBit bitV
        , MySQLInt8 (-128)
        , MySQLInt8U 255
        , MySQLInt16 (-32768)
        , MySQLInt16U 65535
        , MySQLNull
        , MySQLInt32U 16777215
        , MySQLInt32 (-2147483648)
        , MySQLInt32U 4294967295
        , MySQLInt64 (-9223372036854775808)
        , MySQLInt64U 18446744073709551615
        , MySQLDecimal 1234567890.0123456789
        , MySQLFloat 3.14159
        , MySQLNull
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
        , MySQLNull
        , MySQLText "foo"
        , MySQLText "foo,bar"
        ]

    Stream.skipToEof is
--------------------------------------------------------------------------------
    updStmt1 <- prepareStmt c "UPDATE test SET \
        \__decimal  = ?         ,\
        \__date     = ?         ,\
        \__timestamp = ? WHERE __id=0"
    executeStmt c updStmt1 [MySQLNull, MySQLNull, MySQLNull]

    (_, is) <- queryStmt c selStmt []
    Just v <- Stream.read is

    assertEqual "roundtrip binary protocol with null" v
        [ MySQLInt32 0
        , MySQLBit bitV
        , MySQLInt8 (-128)
        , MySQLInt8U 255
        , MySQLInt16 (-32768)
        , MySQLInt16U 65535
        , MySQLNull
        , MySQLInt32U 16777215
        , MySQLInt32 (-2147483648)
        , MySQLInt32U 4294967295
        , MySQLInt64 (-9223372036854775808)
        , MySQLInt64U 18446744073709551615
        , MySQLNull
        , MySQLFloat 3.14159
        , MySQLNull
        , MySQLNull
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59))
        , MySQLNull
        , MySQLTime 1 (TimeOfDay 199 59 59)
        , MySQLYear 1999
        , MySQLText "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLBytes "12345678"
        , MySQLText "韩冬真赞"
        , MySQLBytes "12345678"
        , MySQLNull
        , MySQLText "foo"
        , MySQLText "foo,bar"
        ]
--------------------------------------------------------------------------------
    Stream.skipToEof is
    execute_ c "UPDATE test SET \
        \__time       = '199:59:59'     ,\
        \__year       = 0  WHERE __id=0"

    selStmt2 <- prepareStmt c "SELECT __time, __year FROM test"
    (_, is) <- queryStmt c selStmt2 []
    Just v <- Stream.read is

    assertEqual "decode binary protocol 2" v
            [ MySQLTime 0 (TimeOfDay 199 59 59)
            , MySQLYear 0
            ]

    Stream.skipToEof is
--------------------------------------------------------------------------------
    updStmt2 <- prepareStmt c "UPDATE test SET \
                \__time       = ?     ,\
                \__year       = ?  WHERE __id=0"

    executeStmt c updStmt2 [ MySQLTime 0 (TimeOfDay 00 00 00), MySQLYear 2055]

    (_, is) <- queryStmt c selStmt2 []
    Just v <- Stream.read is
    assertEqual "roundtrip binary protocol 2" v
            [ MySQLTime 0 (TimeOfDay 00 00 00)
            , MySQLYear 2055
            ]

    Stream.skipToEof is
--------------------------------------------------------------------------------
    execute_ c "UPDATE test SET \
        \__text       = ''     ,\
        \__blob       = ''  WHERE __id=0"

    selStmt3 <- prepareStmt c "SELECT __text, __blob FROM test"
    (_, is) <- queryStmt c selStmt3 []
    Just v <- Stream.read is

    assertEqual "decode binary protocol 3" v
            [ MySQLText ""
            , MySQLBytes ""
            ]

    Stream.skipToEof is
--------------------------------------------------------------------------------
    updStmt3 <- prepareStmt c "UPDATE test SET \
                \__text       = ?     ,\
                \__blob       = ?  WHERE __id=0"

    executeStmt c updStmt3
        [ MySQLText (T.replicate 100000 "xyz")
        , MySQLBytes (B.replicate 1000000 64)
        ]

    (_, is) <- queryStmt c selStmt3 []
    Just v <- Stream.read is
    assertEqual "roundtrip binary protocol 3" v
        [ MySQLText (T.replicate 100000 "xyz")
        , MySQLBytes (B.replicate 1000000 64)
        ]

    Stream.skipToEof is
