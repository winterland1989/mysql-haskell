{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base
import qualified Database.MySQL.UnixSocket as MySQLUS
import           System.Environment
import           System.IO.Streams        (fold)
import qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum)
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = void . flip mapConcurrently [1..n] $ \ _ -> do
    c <- MySQLUS.connect MySQLUS.ConnectInfo
        { MySQLUS.ciUnixSocket = "/tmp/mysql.sock"
        , MySQLUS.ciUser = "testMySQLHaskell"
        , MySQLUS.ciPassword = ""
        , MySQLUS.ciDatabase = "testMySQLHaskell"
        }

    (fs, is) <- query_ c "SELECT * FROM employees"
    (rowCount :: Int) <- fold (\s _ -> s+1) 0 is
    putStr "field name: "
    forM_ fs $ \ f -> B.putStr (columnName f) >> B.putStr ", "
    putStr "\n"
    putStr "numbers of rows: "
    print rowCount






