mysql-haskell
=============

`mysql-haskell` is a mysql driver written entirely in haskell by Winterland at infrastructure department of Didi group, it's going to be used in projects aiming at replacing old java based mysql middlewares.

Roadmap:

- [x] username/password authenticate
- [x] text protocol decoding
- [x] binary protocol decoding
- [x] binlog listening
- [x] basic binlog parsing
- [ ] support some new binary types: `TIMESTAMP2/DATETIME2...`
- [ ] full binlog parsing 
- [ ] comprehensive testsuit and benchmark
- [ ] polish and stablize API
- [ ] tls support

This project is still in infancy stage and lack of produciton tests, use on your own risk, and any form of contributions are welcomed!

Motivation
----------

While mysql may not be the most advanced sql database, it's widely used among china companies, including but not limited to Baidu, Alibaba, Tecent etc., but haskell's mysql support is not ideal, we only have a very basic mysql binding written by Bryan O'Sullivan, and some higher level wrapper built on it, which have some problems:

+ lack of prepared statment and binary protocol support.

+ limited concurrency due to FFI.

+ no replication protocol support.

mysql-haskell is intended to solve these problems, and providing foundation for higher level libraries such as groundhog and persistent, so that accessing mysql is both fast and easy in haskell.

Building
--------

```bash
git clone https://github.com/winterland1989/mysql-haskell.git
cd mysql-haskell
cabal new-build
```

or use whatever tools you like, currently i didn't add `stack.yml` though. 

Guide
-----

Currently document is extremely lacking, you may find something you need in `Database.MySQL.Protocol.Base` module.

Reference
---------

[Mysql official site](https://dev.mysql.com/doc/internals/en/) provided intensive document, but without following project, `mysql-haskell` may not be written at all:

+ [go mysql toolkit](https://github.com/siddontang/go-mysql)

+ [python binlog parser](https://github.com/noplay/python-mysql-replication)

+ [ruby binlog parser](https://github.com/jeremycole/mysql_binlog)

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
