mysql-haskell
=============

[![Hackage](https://img.shields.io/hackage/v/mysql-haskell.svg?style=flat)](http://hackage.haskell.org/package/mysql-haskell)
[![Build Status](https://travis-ci.org/winterland1989/mysql-haskell.svg)](https://travis-ci.org/winterland1989/mysql-haskell)

`mysql-haskell` is a MySQL driver written entirely in haskell.

<a href="http://www.genshuixue.com/"><img height=42 src='http://cdn.gsxservice.com/asset/img/logo-release2.png'></a>
<a href="http://chordify.net/"><img height=42 src='https://chordify.net/img/about/slide_250_1.jpg'></a>
<a href="http://www.didichuxing.com/"><img height=42 src='http://www.didichuxing.com/images/icon02.png'></a>

Is it fast?
----------

In short, `select`(decode) is about 1.5 times slower than pure c/c++ but 5 times faster than `mysql-simple`, `insert` (encode) is about 1.5 times slower than pure c/c++, there're many factors involved(tls, prepared statment, batch using multiple statement):

<img src="https://github.com/winterland1989/mysql-haskell/blob/master/benchmark/result.png?raw=true" width="100%">

Above figures showed the time to:

* perform a "select * from employees" from a [sample table](https://github.com/datacharmer/test_db)
* insert 1000 rows into a 29-columns table per thread with auto-commit off.

The benchmarks are run by my MacBook Pro 13' 2015.

Motivation
----------

While MySQL may not be the most advanced sql database, it's widely used among China companies, including but not limited to Baidu, Alibaba, Tecent etc., but haskell's MySQL support is not ideal, we only have a very basic MySQL binding written by Bryan O'Sullivan, and some higher level wrapper built on it, which have some problems:

+ lack of prepared statment and binary protocol support.

+ limited concurrency due to FFI.

+ no replication protocol support.

`mysql-haskell` is intended to solve these problems, and provide foundation for higher level libraries such as groundhog and persistent, so that accessing MySQL is both fast and easy in haskell.

Guide
-----

The `Database.MySQL.Base` module provides everything you need to start making queries:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams

main :: IO () 
main = do
    conn <- connect
        defaultConnectInfo {ciUser = "username", ciPassword = "password", ciDatabase = "dbname"}
    (defs, is) <- query_ conn "SELECT * FROM some_table"
    print =<< Streams.toList is
```

`query/query_` will return a column definition list, and an `InputStream` of rows, you should consume this stream completely before start new queries.

It's recommanded to use prepared statement to improve query speed:

```haskell
    ...
    s <- prepareStmt conn "SELECT * FROM some_table where person_age > ?"
    ...
    (defs, is) <- queryStmt conn s [MySQLInt32U 18]
    ...
```

If you want to do batch inserting/deleting/updating, you can use `executeMany` to save considerable time.

The `Database.MySQL.BinLog` module provides binlog listenning functions and row-based event decoder, following program will automatically get last binlog position, and print every row event it receives:

```haskell
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad         (forever)
import qualified Database.MySQL.BinLog as MySQL
import qualified System.IO.Streams     as Streams

main :: IO () 
main = do
    conn <- MySQL.connect 
        MySQL.defaultConnectInfo
          { MySQL.ciUser = "username"
          , MySQL.ciPassword = "password"
          , MySQL.ciDatabase = "dbname"
          }
    MySQL.getLastBinLogTracker conn >>= \ case
        Just tracker -> do
            es <- MySQL.decodeRowBinLogEvent =<< MySQL.dumpBinLog conn 1024 tracker False
            forever $ do
                Streams.read es >>= \ case
                    Just v  -> print v
                    Nothing -> return ()
        Nothing -> error "can't get latest binlog position"
```

Build Test Benchmark
--------------------

Just use the old way:

```bash
git clone https://github.com/winterland1989/mysql-haskell.git
cd mysql-haskell
cabal install --enable-tests --only-dependencies
cabal build
```

Running tests require:

* A local MySQL server, a user `testMySQLHaskell` and a database `testMySQLHaskell`, you can do it use following script:

```bash
mysql -u root -e "CREATE DATABASE IF NOT EXISTS testMySQLHaskell;"
mysql -u root -e "CREATE USER 'testMySQLHaskell'@'localhost' IDENTIFIED BY ''"
mysql -u root -e "GRANT ALL PRIVILEGES ON testMySQLHaskell.* TO 'testMySQLHaskell'@'localhost'"
mysql -u root -e "FLUSH PRIVILEGES"
```

* Enable binlog by adding `log_bin = filename` to `my.cnf` or add `--log-bin=filename` to the server, and grant replication access to `testMySQLHaskell` with:

```bash
mysql -u root -e "GRANT REPLICATION SLAVE, REPLICATION CLIENT ON *.* TO 'testMySQLHaskell'@'localhost';"
```

* Set `binlog_format` to `ROW`.

* Set `max_allowed_packet` to larger than 256M(for test large packet).

New features will be automatically tested by inspecting MySQL server's version, travis is keeping an eye on following combinations:

+ CABALVER=1.18 GHCVER=7.8.4  MYSQLVER=5.5
+ CABALVER=1.22 GHCVER=7.10.2 MYSQLVER=5.5
+ CABALVER=1.24 GHCVER=8.0.1  MYSQLVER=5.5
+ CABALVER=1.24 GHCVER=8.0.1  MYSQLVER=5.6
+ CABALVER=1.24 GHCVER=8.0.1  MYSQLVER=5.7

Please reference `.travis.yml` if you have problems with setting up test environment.

Enter benchmark directory and run `./bench.sh` to benchmark 1) c++ version 2) mysql-haskell 3) FFI version mysql, you may need to:

+ Modify `bench.sh`(change the include path) to get c++ version compiled.
+ Modify `mysql-haskell-bench.cabal`(change the openssl's lib path) to get haskell version compiled.
+ Setup MySQL's TLS support, modify `MySQLHaskellOpenSSL.hs/MySQLHaskellTLS.hs` to change the CA file's path, and certificate's subject name.
+ Adjust rts options `-N` to get best results.

With `-N10` on my company's 24-core machine, binary protocol performs almost identical to c version!

Reference
---------

[MySQL official site](https://dev.mysql.com/doc/internals/en/) provided intensive document, but without following project, `mysql-haskell` may not be written at all:

+ [mysql-binlog-connector-java](https://github.com/shyiko/mysql-binlog-connector-java)

+ [canal](https://github.com/alibaba/canal)

+ [go mysql toolkit](https://github.com/siddontang/go-mysql)

+ [python binlog parser](https://github.com/noplay/python-mysql-replication)

License
-------

Copyright (c) 2016, winterland1989

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of winterland1989 nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
