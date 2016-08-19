{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base
import           System.Environment
import           System.IO.Streams        (fold)
import  qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum)
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = void . flip mapConcurrently [1..n] $ \ _ -> do
    c <- connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    }


    stmt <- prepareStmt c "SELECT * FROM employees"
    (fs, is) <- queryStmt c stmt []
    (rowCount :: Int) <- fold (\s _ -> s+1) 0 is
    putStr "field name: "
    forM_ fs $ \ f -> B.putStr (columnName f) >> B.putStr ", "
    putStr "\n"
    putStr "numbers of rows: "
    print rowCount






