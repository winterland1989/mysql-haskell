{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

        execute_ c "BEGIN"
        executeMany @MySQLValue c "INSERT INTO insert_test values (\
                       \ 0                                      ,\
                       \ b'1110000010101010'                    ,\
                       \ -128                                   ,\
                       \ 255                                    ,\
                       \ -32768                                 ,\
                       \ 65535                                  ,\
                       \ -8388608                               ,\
                       \ 16777215                               ,\
                       \ -2147483648                            ,\
                       \ 4294967295                             ,\
                       \ -9223372036854775808                   ,\
                       \ 18446744073709551615                   ,\
                       \ 1234567890.0123456789                  ,\
                       \ 3.14159                                ,\
                       \ 3.1415926535                           ,\
                       \ '2016-08-08'                           ,\
                       \ '2016-08-08 17:25:59'                  ,\
                       \ '2016-08-08 17:25:59'                  ,\
                       \ '-199:59:59'                           ,\
                       \ 1999                                   ,\
                       \ '12345678'                             ,\
                       \ '韩冬真赞'                             ,\
                       \ '12345678'                             ,\
                       \ '12345678'                             ,\
                       \ '12345678'                             ,\
                       \ '韩冬真赞'                             ,\
                       \ '12345678'                             ,\
                       \ '韩冬真赞'                             ,\
                       \ 'foo'                                  ,\
                       \ 'foo,bar')"
                       (replicate 1000 [])

        execute_ c "COMMIT"
        return ()





