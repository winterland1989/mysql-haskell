{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TextRowNew where

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
    (f, is5) <- apiQuery_ api testNewColumns c "SELECT * FROM test_new"

    assertEqual "decode Field types" (columnType <$> f)
        [ mySQLTypeLong
        , mySQLTypeDateTime
        , mySQLTypeTimestamp
        , mySQLTypeTime
        ]

    Just v1 <- Stream.read is5
    assertEqual "decode NULL values" v1
        [ MySQLInt32 0
        , MySQLNull
        , MySQLNull
        , MySQLNull
        ]

    Stream.skipToEof is5

    _ <- execute_ c "UPDATE test_new SET \
                \__datetime   = '2016-08-08 17:25:59.12'                  ,\
                \__timestamp  = '2016-08-08 17:25:59.1234'                ,\
                \__time       = '-199:59:59.123456' WHERE __id=0"

    (_, is4) <- apiQuery_ api testNewColumns c "SELECT * FROM test_new"
    Just v2 <- Stream.read is4

    assertEqual "decode text protocol" v2
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
        , MySQLTime 1 (TimeOfDay 199 59 59.123456)
        ]

    Stream.skipToEof is4

    _ <- execute_ c "UPDATE test_new SET \
                \__datetime   = '2016-08-08 17:25:59.1'                  ,\
                \__timestamp  = '2016-08-08 17:25:59.12'                ,\
                \__time       = '199:59:59.1234' WHERE __id=0"

    (_, is0) <- apiQuery_ api testNewColumns c "SELECT * FROM test_new"
    Just v3 <- Stream.read is0

    assertEqual "decode text protocol 2" v3
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTime 0 (TimeOfDay 199 59 59.1234)
        ]

    Stream.skipToEof is0

    _ <- execute c "UPDATE test_new SET \
            \__datetime   = ?     ,\
            \__timestamp  = ?     ,\
            \__time       = ?  WHERE __id=0"
                [ MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
                , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
                , MySQLTime 1 (TimeOfDay 199 59 59.123456)
                ]


    (_, is1) <- apiQuery_ api testNewColumns c "SELECT * FROM test_new"
    Just v4 <- Stream.read is1

    assertEqual "roundtrip text protocol" v4
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1234))
        , MySQLTime 1 (TimeOfDay 199 59 59.123456)
        ]

    Stream.skipToEof is1

    _ <- execute c "UPDATE test_new SET \
            \__datetime   = ?     ,\
            \__timestamp  = ?     ,\
            \__time       = ?  WHERE __id=0"
                [ MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
                , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
                , MySQLTime 0 (TimeOfDay 199 59 59.1234)
                ]

    (_, is2) <- apiQuery_ api testNewColumns c "SELECT * FROM test_new"
    Just v5 <- Stream.read is2

    assertEqual "roundtrip text protocol 2" v5
        [ MySQLInt32 0
        , MySQLDateTime (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.1))
        , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
        , MySQLTime 0 (TimeOfDay 199 59 59.1234)
        ]

    Stream.skipToEof is2

    let row0 =
            [ MySQLInt32 0
            , MySQLDateTime  (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.10))
            , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 08) (TimeOfDay 17 25 59.12))
            , MySQLTime 0 (TimeOfDay 199 59 59.1234)
            ]
    let row1 =
            [ MySQLInt32 1
            , MySQLDateTime  (LocalTime (fromGregorian 2016 08 09) (TimeOfDay 18 25 59.10))
            , MySQLTimeStamp (LocalTime (fromGregorian 2016 08 09) (TimeOfDay 18 25 59.12))
            , MySQLTime 0 (TimeOfDay 299 59 59.1234)
            ]
    _ <- execute c "UPDATE test_new SET \
            \__id         = ?     ,\
            \__datetime   = ?     ,\
            \__timestamp  = ?     ,\
            \__time       = ?  WHERE __id=0"
            row0
    _ <- execute c "INSERT INTO test_new VALUES(\
            \?,\
            \?,\
            \?,\
            \? \
            \)"
            row1

    (_, is3) <- apiQuery api testNewColumns c "SELECT * FROM test_new WHERE __id IN (?) ORDER BY __id" [Many [MySQLInt32 0, MySQLInt32 1]]
    Just v6 <- Stream.read is3
    Just v7 <- Stream.read is3

    assertEqual "select list of ids" [v6, v7] [row0, row1]

    Stream.skipToEof is3
    _ <- execute_ c "DELETE FROM test_new where __id=1"

    return ()
