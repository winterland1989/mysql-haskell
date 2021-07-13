{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent.QSemN
import           Control.Monad
import           Database.MySQL.Base
import qualified Z.Data.Builder as B
import           Z.Data.PrimRef
import           Z.Data.CBytes  as CB
import qualified Z.IO.BIO       as BIO
import           Z.IO

main :: IO ()
main = do
    args <- getArgs
    case args of (_:threadNum:_) -> go (read (CB.unpack threadNum))
                 _ -> putStdLn "No thread number provided."

go :: Int -> IO ()
go n = do
    q <- newQSemN 0
    replicateM_ n . forkBa $ withResource (connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    , ciPassword =  "testMySQLHaskell123456!"
                                    }) $ \ c -> do

        (fs, is) <- query_ c "SELECT * FROM employees"
        c <- newCounter 0
        BIO.run_ $ is . BIO.counter c
        (rowCount :: Int) <- readCounter c
        putStdLn $ do
            "field name: "
            forM_ fs $ \ f -> B.text (columnName f)
            ", numbers of rows: "
            B.int rowCount
        signalQSemN q 1
    waitQSemN q n




