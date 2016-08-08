{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import Test.HUnit
import qualified TextProtocol

main :: IO ()
main = do
    c <- connect defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    execute_ c  "CREATE TEMPORARY TABLE test(\
                \_id INT,\
                \_bit BIT(16),\
                \_tinyInt TINYINT,\
                \_tinyIntU TINYINT UNSIGNED,\
                \_smallInt SMALLINT,\
                \_smallIntU SMALLINT UNSIGNED,\
                \_mediumInt MEDIUMINT,\
                \_mediumIntU MEDIUMINT UNSIGNED,\
                \_int INT,\
                \_intU INT UNSIGNED,\
                \_bigInt BIGINT,\
                \_bigIntU BIGINT UNSIGNED,\
                \_decimal DECIMAL(20,10),\
                \_float FLOAT,\
                \_dobule DOUBLE\
                \);"

    execute_ c  "INSERT INTO test VALUES(\
                \0,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL,\
                \NULL\
                \);"
    runTestTT $ TestList
        [   TestLabel "text protocol"   $ TextProtocol.tests c
        ]

    close c


