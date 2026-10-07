{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Simple
import           System.Environment
import           Data.Time.Calendar
import           Data.Text (Text)

main :: IO ()
main = do
    args <- getArgs
    case args of [threadNum] -> go (read threadNum)
                 _ -> putStrLn "No thread number provided."

go :: Int -> IO ()
go n = void . flip mapConcurrently [1..n] $ \ _ -> do
    c <- connect defaultConnectInfo { connectUser = "testMySQLHaskell"
                                    , connectDatabase = "testMySQLHaskell"
                                    -- TCP like mysql-haskell; "localhost" means the socket
                                    , connectHost = "127.0.0.1"
                                    }

    putStr "total rows: "
    (rs :: [(Int, Day, Text, Text, Text, Day)]) <- query_ c "SELECT * FROM employees"
    print (length rs)






