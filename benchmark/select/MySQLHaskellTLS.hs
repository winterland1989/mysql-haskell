{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base
import           System.Environment
import           System.IO.Streams        (fold)
import           Data.OpenSSLSetting
import           System.IO.Streams.OpenSSL (withOpenSSL)
import  qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum)
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = do
    ctx <- withOpenSSL $ makeClientSSLContext (CustomCAStore "/usr/local/var/mysql/ca.pem")
    void . flip mapConcurrently [1..n] $ \ _ -> do
        c <- connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                        , ciDatabase = "testMySQLHaskell"
                                        , ciTLSInfo = Just ctx
                                        }

        (fs, is) <- query_ c "SELECT * FROM employees"
        (rowCount :: Int) <- fold (\s _ -> s+1) 0 is
        putStr "field name: "
        forM_ fs $ \ f -> B.putStr (columnName f) >> B.putStr ", "
        putStr "\n"
        putStr "numbers of rows: "
        print rowCount






