{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base  hiding (connect, connectDetail)
import           Database.MySQL.TLS
import           System.Environment
import           System.IO.Streams        (fold)
import  qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum)
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = do
    cparams <- makeClientParams (CustomCAStore "/usr/local/var/mysql/ca.pem")

    void . flip mapConcurrently [1..n] $ \ _ -> do
        c <- connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                        , ciDatabase = "testMySQLHaskell"
                                        }
                     (cparams, "MySQL")

        (fs, is) <- query_ c "SELECT * FROM employees"
        (rowCount :: Int) <- fold (\s _ -> s+1) 0 is
        putStr "field name: "
        forM_ fs $ \ f -> B.putStr (columnName f) >> B.putStr ", "
        putStr "\n"
        putStr "numbers of rows: "
        print rowCount






