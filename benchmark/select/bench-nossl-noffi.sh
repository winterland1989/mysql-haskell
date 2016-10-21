#!/bin/bash

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DROP TABLE IF EXISTS employees"
mysql -utestMySQLHaskell < employees.sql

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

g++ ./libmysql_prepared.cpp -lmysqlclient -lpthread -lz -lm -lssl -lcrypto -ldl\
 -I/usr/local/opt/mysql/include -L/usr/local/opt/mysql/lib -I/usr/local/opt/openssl/include -L/usr/local/opt/openssl/lib\
 -o libmysql_prepared

echo "=============== start benchmark c++ client prepared ================"
time ./libmysql_prepared 1
time ./libmysql_prepared 2
time ./libmysql_prepared 3
time ./libmysql_prepared 4
time ./libmysql_prepared 10
rm ./libmysql_prepared
echo "=============== benchmark c++ client prepared end ================"

cabal build
echo "=============== start benchmark haskell client ============="
time ./dist/build/bench/bench 1          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 2          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 3          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 4          +RTS -N4 -A128M -RTS
time ./dist/build/bench/bench 10         +RTS -N4 -A128M -RTS
echo "=============== benchmark haskell client end ================"

echo "=============== start benchmark haskell client prepared ============="
time ./dist/build/benchPrepared/benchPrepared 1          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 2          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 3          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 4          +RTS -N4 -A128M -RTS
time ./dist/build/benchPrepared/benchPrepared 10         +RTS -N4 -A128M -RTS
echo "=============== benchmark haskell client prepared end ================"

