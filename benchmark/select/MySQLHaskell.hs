{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base
import           System.Environment
import           System.IO.Streams        (fold)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum) "employees"
                 [threadNum, table] -> go (read threadNum) table
                 _ -> putStrLn "Usage: THREADS [TABLE], TABLE defaults to employees."

go :: Int -> String -> IO ()
go n table = void . flip mapConcurrently [1..n] $ \ _ -> do
    c <- connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    }

    (fs, is) <- query_ c (Query ("SELECT * FROM " <> BL.pack table))
    (rowCount :: Int) <- fold (\s _ -> s+1) 0 is
    putStr "field name: "
    forM_ fs $ \ f -> B.putStr (columnName f) >> B.putStr ", "
    putStr "\n"
    putStr "numbers of rows: "
    print rowCount






