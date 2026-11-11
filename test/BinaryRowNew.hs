{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BinaryRowNew where

import           Control.Applicative
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import           Database.MySQL.Base
import           QueryApi
import qualified System.IO.Streams   as Stream
import           Test.Tasty.HUnit

-- | The columns of the @test_new@ table.
testNewColumns :: ColumnCount
testNewColumns = ColumnCount 4

tests :: QueryApi -> MySQLConn -> Assertion
tests api c = do
    selStmt <- prepareStmt c "SELECT * FROM test_new"

    (f, is) <- apiQueryStmt api testNewColumns c selStmt []
    assertEqual "decode Field types" (columnType <$> f)
        [ mySQLTypeLong
        , mySQLTypeDateTime
        , mySQLTypeTimestamp
        , mySQLTypeTime
        ]

    Just v <- Stream.read is
    assertEqual "decode NULL values" v
        [ MySQLInt32 0
        , MySQLNull
        , MySQLNull
        , MySQLNull
        ]

    Stream.skipToEof is

    execute_ c "UPDATE test_new SET \
                \__datetime   = '2016-08-08 17:25:59.12'                  ,\
                \__timestamp  = '2016-08-08 17:25:59.1234'                ,\
                \__time       = '-199:59:59.123456' WHERE __id=0"

    (_, is) <- apiQueryStmt api testNewColumns c selStmt []
    Just v <- Stream.read is

    assertEqual "decode binary protocol" v
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
        , MySQLTime 1 (TimeOfDay 199 59 59.123456)
        ]

    Stream.skipToEof is

    execute_ c "UPDATE test_new SET \
                \__datetime   = '2016-08-08 17:25:59.1'                  ,\
                \__timestamp  = '2016-08-08 17:25:59.12'                ,\
                \__time       = '199:59:59.123' WHERE __id=0"

    (_, is) <- apiQueryStmt api testNewColumns c selStmt []
    Just v <- Stream.read is

    assertEqual "decode binary protocol 2" v
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTime 0 (TimeOfDay 199 59 59.123)
        ]

    Stream.skipToEof is

    updStmt <- prepareStmt c
            "UPDATE test_new SET \
            \__datetime   = ?     ,\
            \__timestamp  = ?     ,\
            \__time       = ? WHERE __id=0"

    executeStmt c updStmt
                [ MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
                , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
                , MySQLTime 1 (TimeOfDay 199 59 59.123456)
                ]


    (_, is) <- apiQueryStmt api testNewColumns c selStmt []
    Just v <- Stream.read is

    assertEqual "roundtrip binary protocol" v
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
        , MySQLTime 1 (TimeOfDay 199 59 59.123456)
        ]

    Stream.skipToEof is

    executeStmt c updStmt
                [ MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
                , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
                , MySQLTime 0 (TimeOfDay 199 59 59.1234)
                ]


    (_, is) <- apiQueryStmt api testNewColumns c selStmt []
    Just v <- Stream.read is

    assertEqual "roundtrip binary protocol 2" v
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTime 0 (TimeOfDay 199 59 59.1234)
        ]

    Stream.skipToEof is
