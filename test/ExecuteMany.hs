{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteMany where

import           Control.Applicative
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import           Database.MySQL.Base
import qualified System.IO.Streams   as Stream
import           Test.Tasty.HUnit

tests :: MySQLConn -> Assertion
tests c = do

    oks <- withTransaction c $ executeMany c "INSERT INTO test VALUES(\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?     ,\
            \?)"
            (replicate 50000
                [ MySQLInt32 0
                , MySQLBit 255
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
            )
    assertEqual "executeMany affected rows" (sum $ map okAffectedRows oks) 50000
