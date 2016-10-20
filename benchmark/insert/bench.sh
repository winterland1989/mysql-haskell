#!/bin/bash

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DROP TABLE IF EXISTS insert_test"
mysql -utestMySQLHaskell -DtestMySQLHaskell -e "CREATE TABLE insert_test(\
                __id           INT,\
                __bit          BIT(16),\
                __tinyInt      TINYINT,\
                __tinyIntU     TINYINT UNSIGNED,\
                __smallInt     SMALLINT,\
                __smallIntU    SMALLINT UNSIGNED,\
                __mediumInt    MEDIUMINT,\
                __mediumIntU   MEDIUMINT UNSIGNED,\
                __int          INT,\
                __intU         INT UNSIGNED,\
                __bigInt       BIGINT,\
                __bigIntU      BIGINT UNSIGNED,\
                __decimal      DECIMAL(20,10),\
                __float        FLOAT,\
                __double       DOUBLE,\
                __date         DATE,\
                __datetime     DATETIME,\
                __timestamp    TIMESTAMP NULL,\
                __time         TIME,\
                __year         YEAR(4),\
                __char         CHAR(8),\
                __varchar      VARCHAR(1024),\
                __binary       BINARY(8),\
                __varbinary    VARBINARY(1024),\
                __tinyblob     TINYBLOB,\
                __tinytext     TINYTEXT,\
                __blob         BLOB,\
                __text         TEXT,\
                __enum         ENUM('foo', 'bar', 'qux'),\
                __set          SET('foo', 'bar', 'qux')\
                ) CHARACTER SET utf8"

g++ ./libmysql.cpp -lmysqlclient -lpthread -lz -lm -lssl -lcrypto -ldl\
 -I/usr/local/opt/mysql/include -L/usr/local/opt/mysql/lib -I/usr/local/opt/openssl/include -L/usr/local/opt/openssl/lib\
 -o libmysql

echo "=============== start benchmark c++ client ================"
time ./libmysql 1
time ./libmysql 2
time ./libmysql 3
time ./libmysql 4
time ./libmysql 10
rm ./libmysql
echo "=============== benchmark c++ client end ================"

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "select count(*) from insert_test" 
mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DELETE FROM insert_test"

cabal build
echo "=============== start benchmark haskell client ============="
time ./dist/build/bench/bench 1          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 2          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 3          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 4          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 10         +RTS -N4 -A128M -RTS
echo "=============== benchmark haskell client end ================"

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "select count(*) from insert_test" 
mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DELETE FROM insert_test"

echo "=============== start benchmark haskell client (executeMany) ============="
time ./dist/build/bench-insert-many/bench-insert-many 1          +RTS -N4 -A128M -RTS
time ./dist/build/bench-insert-many/bench-insert-many 2          +RTS -N4 -A128M -RTS
time ./dist/build/bench-insert-many/bench-insert-many 3          +RTS -N4 -A128M -RTS
time ./dist/build/bench-insert-many/bench-insert-many 4          +RTS -N4 -A128M -RTS
time ./dist/build/bench-insert-many/bench-insert-many 10         +RTS -N4 -A128M -RTS
echo "=============== benchmark haskell client (executeMany) end ================"

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "select count(*) from insert_test" 
mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DELETE FROM insert_test"
echo "=============== start benchmark haskell client prepared ============="
time ./dist/build/benchPrepared/benchPrepared 1          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 2          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 3          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 4          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 10         +RTS -N4 -A128M -RTS
echo "=============== benchmark haskell client prepared end ================"

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "select count(*) from insert_test" 
mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DELETE FROM insert_test"
