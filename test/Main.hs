module Main where

import Database.MySQL.Base
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import qualified TextProtocol
import System.Environment

main :: IO ()

main = do
    mySQLVer <- lookupEnv "MYSQLVER"

    defaultMain $ testCaseSteps "Multi-step test" $ \step -> do
        step "Preparing..."
        c <- connect defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
        execute_ c  "CREATE TEMPORARY TABLE test(\
                    \__id           INT,\
                    \__bit          BIT(16),\
                    \__tinyInt      TINYINT,\
                    \__tinyIntU     TINYINT UNSIGNED,\
                    \__smallInt     SMALLINT,\
                    \__smallIntU    SMALLINT UNSIGNED,\
                    \__mediumInt    MEDIUMINT,\
                    \__mediumIntU   MEDIUMINT UNSIGNED,\
                    \__int          INT,\
                    \__intU         INT UNSIGNED,\
                    \__bigInt       BIGINT,\
                    \__bigIntU      BIGINT UNSIGNED,\
                    \__decimal      DECIMAL(20,10),\
                    \__float        FLOAT,\
                    \__dobule       DOUBLE,\
                    \__date         DATE,\
                    \__datetime     DATETIME,\
                    \__timestamp    TIMESTAMP NULL,\
                    \__time         TIME,\
                    \__year         YEAR,\
                    \__char         CHAR(8),\
                    \__varchar      VARCHAR(1024),\
                    \__binary       BINARY(8),\
                    \__varbinary    VARBINARY(1024),\
                    \__tinyblob     TINYBLOB,\
                    \__tinytext     TINYTEXT,\
                    \__blob         BLOB,\
                    \__text         TEXT,\
                    \__enum         ENUM('foo', 'bar', 'qux'),\
                    \__set          SET('foo', 'bar', 'qux')\
                    \) CHARACTER SET utf8;"

        when (mySQLVer == Just "5.7") $ do execute_ c "CREATE TEMPORARY TABLE test57(\
                                                 \__datetime     DATETIME(2),\
                                                 \__timestamp    TIMESTAMP(4) NULL,\
                                                 \__time         TIME(6)\
                                                 \) CHARACTER SET utf8;"
                                           return ()

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
                    \NULL,\
                    \NULL,\
                    \NULL\
                    \);"

        step "Testing text protocol"
        TextProtocol.tests c

        close c


