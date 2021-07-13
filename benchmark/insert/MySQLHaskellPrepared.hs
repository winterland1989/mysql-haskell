{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NegativeLiterals #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.QSemN
import           Control.Monad
import           Database.MySQL.Base
import qualified Z.Data.Builder as B
import qualified Z.Data.Vector as V
import           Z.Data.CBytes  as CB
import           Z.Data.PrimRef
import qualified Z.IO.BIO       as BIO
import           Z.IO
import           Data.Time.Calendar  (fromGregorian)
import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))

main :: IO ()
main = do
    args <- getArgs
    case args of (_:threadNum:_) -> go (read (CB.unpack threadNum))
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = void . flip mapConcurrently [1..n] $ \ _ -> do
    withResource (connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    , ciPassword =  "testMySQLHaskell123456!"
                                    }) $ \ c -> do

        stmt <- prepareStmt c "INSERT INTO insert_test values (\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?   ,\
                       \ ?)"

        let bitV = 43744 -- 0b1010101011100000
        execute_ c "BEGIN"
        replicateM_ 1000 $ executeStmt c stmt
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

        execute_ c "COMMIT"





