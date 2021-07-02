{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Database.MySQL.Base
import           Z.Data.PrimRef
import           Z.Data.CBytes  as CB
import           Z.IO.BIO
import           Z.IO
import qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    case args of (_:threadNum:_) -> go (read (CB.unpack threadNum))
                 _ -> putStdLn "No thread number provided."

go :: Int -> IO ()
go n = void . flip mapConcurrently [1..n] $ \ _ -> do
    withResource (connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    , ciPassword =  "testMySQLHaskell123456!"
                                    }) $ \ c -> do

        (fs, is) <- query_ c "SELECT * FROM employees"
        c <- newCounter 0
        runBIO $ is . counterNode c
        (rowCount :: Int) <- readPrimIORef c
        putStd "field name: "
        forM_ fs $ \ f -> printStd (columnName f) >> putStdLn ", "
        putStd "numbers of rows: "
        printStd rowCount





