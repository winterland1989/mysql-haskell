{-# LANGUAGE ScopedTypeVariables #-}

module MysqlTests(tests)  where

import qualified BinaryRow
import qualified BinaryRowNew
import qualified BinLog
import qualified BinLogNew
import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Exception     (bracket, catch)
import           Control.Monad
import qualified Data.ByteString       as B
import           Database.MySQL.Base
import           Database.MySQL.BinLog
import           System.Environment
import qualified System.IO.Streams as Stream
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified TextRow
import qualified ExecuteMany
import qualified TextRowNew

tests :: TestTree
tests = testCaseSteps "mysql-haskell test suit" $ \step -> do

    step "preparing table..."
    (greet, c) <- connectDetail defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}

    let ver = greetingVersion greet
        isOld = "5.0" `B.isPrefixOf` ver
                || "5.1" `B.isPrefixOf` ver
                || "5.5" `B.isPrefixOf` ver
        isNew = not isOld  -- MySQL 5.6+ and MariaDB 10+ support fractional seconds
                           -- in TIME, DATETIME, and TIMESTAMP columns


    execute_ c "DROP TABLE IF EXISTS test"
    execute_ c "DROP TABLE IF EXISTS test_new"

    execute_ c  "CREATE TABLE test(\
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
                \__double       DOUBLE,\
                \__date         DATE,\
                \__datetime     DATETIME,\
                \__timestamp    TIMESTAMP NULL,\
                \__time         TIME,\
                \__year         YEAR(4),\
                \__char         CHAR(8),\
                \__varchar      VARCHAR(1024),\
                \__binary       BINARY(8),\
                \__varbinary    VARBINARY(1024),\
                \__tinyblob     TINYBLOB,\
                \__tinytext     TINYTEXT,\
                \__blob         BLOB(1000000),\
                \__text         TEXT(1000000),\
                \__enum         ENUM('foo', 'bar', 'qux'),\
                \__set          SET('foo', 'bar', 'qux')\
                \) CHARACTER SET utf8"

    resetTestTable c

    step "testing executeMany"
    ExecuteMany.tests c

    resetTestTable c

    step "testing text protocol"
    TextRow.tests c

    resetTestTable c

    step "testing binary protocol"
    BinaryRow.tests c

    resetTestTable c


    when isNew $ do
        execute_ c "CREATE TABLE test_new(\
                   \__id           INT,\
                   \__datetime     DATETIME(2),\
                   \__timestamp    TIMESTAMP(4) NULL,\
                   \__time         TIME(6)\
                   \) CHARACTER SET utf8"

        resetTest57Table c

        step "testing MySQL5.7 extra text protocol"
        TextRowNew.tests c

        resetTest57Table c

        step "testing MySQL5.7 extra binary protocol"
        BinaryRowNew.tests c

        void $ resetTest57Table c

    step "testing binlog protocol"

    if isNew
    then do
        forkIO BinLogNew.eventProducer
        BinLogNew.tests c
    else do
        forkIO BinLog.eventProducer
        BinLog.tests c

    close c

    step "testing password change"
    (_, c) <- connectDetail defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell"}
    -- ALTER USER works on both MariaDB and MySQL 8.0 (SET PASSWORD = PASSWORD('...') was removed in MySQL 8.0)
    execute_ c "ALTER USER 'testMySQLHaskell'@'localhost' IDENTIFIED BY '123456abcdefg???'"
    close c

    (_, c) <- connectDetail
        defaultConnectInfo {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell", ciPassword = "123456abcdefg???"}
    execute_ c "ALTER USER 'testMySQLHaskell'@'localhost' IDENTIFIED BY ''"
    close c

    catch
        (void $ connectDetail
                    defaultConnectInfo
                        {ciUser = "testMySQLHaskell", ciDatabase = "testMySQLHaskell", ciPassword = "wrongPassWord"})
        (\ (e :: ERRException) -> do
            let ERRException err = e
            assertEqual "wrong password should fail with error 1045" 1045 (errCode err))

  where
    resetTestTable c = do
            execute_ c  "DELETE FROM test WHERE __id=0"
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
                    \)"

    resetTest57Table c = do
            execute_ c  "DELETE FROM test_new WHERE __id=0"
            execute_ c  "INSERT INTO test_new VALUES(\
                        \0,\
                        \NULL,\
                        \NULL,\
                        \NULL\
                        \)"


