#!/bin/bash

mysql -utestMySQLHaskell < employees.sql
g++ libmysql.c -lmysqlclient -lpthread -lz -lm -lssl -lcrypto -ldl -I/usr/local/include/mysql -o libmysql
echo "=============== start benchmark c++ client ================"
time ./libmysql 1
time ./libmysql 2
time ./libmysql 3
time ./libmysql 4
echo "=============== benchmark c++ client end ================"
cd ../
cabal build
echo "=============== start benchmark haskell client ============="
time ./dist/build/bench/bench 1
time ./dist/build/bench/bench 2
time ./dist/build/bench/bench 3
time ./dist/build/bench/bench 4
echo "=============== benchmark haskell client end ================"

mysql -utestMySQLHaskell -DtestMySQLHaskell -e "DROP TABLE employees;"
