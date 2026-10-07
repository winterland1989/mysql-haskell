mysql-haskell
=============

[![Hackage](https://img.shields.io/hackage/v/mysql-haskell.svg?style=flat)](http://hackage.haskell.org/package/mysql-haskell)

`mysql-haskell` is a MySQL driver written entirely in haskell.

<a href="http://chordify.net/"><img height=42 src='https://chordify.net/img/about/slide_250_1.jpg'></a>

Is it fast?
----------

In short, reading is as fast as the C client (`libmysqlclient`) on one connection and up to 1.7 times slower with ten connections in parallel, and 5 to 8 times faster than `mysql-simple`. Writing one statement at a time is 1.2 to 1.4 times slower than C, and on par with C through `executeMany`.

Median wall time in milliseconds over 10 runs, for 1 to 10 threads that each use their own connection:

| threads                                   |   1 |   2 |   3 |   4 |   10 |
|-------------------------------------------|----:|----:|----:|----:|-----:|
| libmysqlclient select                     |  76 |  80 |  88 | 108 |  178 |
| mysql-haskell select                      |  80 |  95 | 106 | 130 |  309 |
| mysql-haskell select over TLS (`tls`)     | 120 | 130 | 148 | 172 |  436 |
| libmysqlclient select, prepared           |  82 |  86 |  94 | 120 |  179 |
| mysql-haskell select, prepared            |  77 |  88 | 102 | 124 |  275 |
| mysql-simple select                       | 462 | 504 | 578 | 698 | 2399 |
| libmysqlclient insert                     |  29 |  30 |  36 |  38 |   62 |
| mysql-haskell insert                      |  40 |  40 |  45 |  46 |   76 |
| mysql-haskell insert, `executeMany`       |  27 |  29 |  36 |  37 |   63 |
| mysql-haskell insert, prepared            |  38 |  38 |  45 |  46 |   82 |

Each thread either reads all 300,024 rows of the [sample employees table](https://github.com/datacharmer/test_db) with `select * from employees`, or inserts 1000 rows into a 29-column table with auto-commit off. The programs are the ones in `benchmark/`. They ran on mysql-haskell 1.3.3 with GHC 9.10.3 and `+RTS -N4`, against MySQL 8.0.45 and its own client library (mysql-simple goes through MariaDB Connector/C 3.3.5), with TLS off unless stated, on an AMD Ryzen AI 7 350 (October 2026). The server kept its data in RAM, so the inserts measure the clients rather than the disk.

Leave the allocation area (`-A`) at its default when running with several capabilities: `-A128M` with `-N4` makes the runtime fault in a 128 MB allocation area for each capability it touches, which costs 35 to 280 ms per run in these benchmarks on every GHC version tried. Since GHC 8.2 it touches all four even when one thread does the work, where GHC 8.0, used for the 2016 figures this README showed until 2026, touched one or two. Without the flag, mysql-haskell 0.6.0.0 and 1.3.3 run plain and prepared selects equally fast, and 1.3.3 is 1.4 to 1.6 times faster over TLS.

Motivation
----------

While MySQL may not be the most advanced sql database, it's widely used among China companies, including but not limited to Baidu, Alibaba, Tecent etc., but haskell's MySQL support is not ideal, we only have a very basic MySQL binding written by Bryan O'Sullivan, and some higher level wrapper built on it, which have some problems:

+ lack of prepared statment and binary protocol support.

+ limited concurrency due to FFI.

+ no replication protocol support.

`mysql-pure` is intended to solve these problems, and provide foundation for higher level libraries such as groundhog and persistent, so that accessing MySQL is both fast and easy in haskell.

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
git clone https://github.com/winterland1989/mysql-pure.git
cd mysql-pure
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

Enter benchmark directory and run `./bench.sh` to benchmark 1) c++ version 2) mysql-pure 3) FFI version mysql, you may need to:

+ Modify `bench.sh`(change the include path) to get c++ version compiled.
+ Modify `mysql-pure-bench.cabal`(change the openssl's lib path) to get haskell version compiled.
+ Setup MySQL's TLS support, modify `MySQLHaskellOpenSSL.hs/MySQLHaskellTLS.hs` to change the CA file's path, and certificate's subject name.
+ Adjust rts options `-N` to get best results, and leave `-A` at its default (see "Is it fast?").

With `-N10` on my company's 24-core machine, binary protocol performs almost identical to c version!

The `.cabal` files in `benchmark/` date from 2016 and no longer build; the figures in "Is it fast?" came from the same programs compiled against the current library.

Reference
---------

[MySQL official site](https://dev.mysql.com/doc/internals/en/) provided intensive document, but without following project, `mysql-pure` may not be written at all:

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
