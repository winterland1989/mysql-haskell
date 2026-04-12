# Revision history for mysql-haskell

## 1.2.3 -- 2026.04.12 
+ Support caching_sha2_password full auth via RSA on non-TLS connections#81 thanks @ikaro1192

## 1.2.2 -- 2026.03.25
+ Widen tls upper bound to allow tls 2.4.x
+ Fix unsafe ByteString operations (`unsafeDrop`, `unsafeTail`, `unsafeIndex`)
  on untrusted wire protocol data that caused undefined behavior (segfaults,
  garbage reads) on malformed input. The safe alternatives are also O(1).
  Affected parsers: text protocol timestamp/datetime/time/date fields,
  NEWDECIMAL in binlog, and `eventHeaderLen` for unknown event types.

## 1.2.1 -- 2026.03.23
+ Export `connectUnixSocket` function.
+ Tweak cabal file to better handle crypton vs cryptonite.
+ Fix version cut off for tls compatibility.
+ Fix missing `liftA2` import when running tests on GHC 9.4.
+ Add integration test for Unix socket.

## 1.2.0 -- 2026.03.07
+ Add support for  caching_sha2_password authentiation.

## 1.1.9 -- 2026.03.07
+ Fix binary protocol error 1210 on modern MySQL/MariaDB:
  `MYSQL_TYPE_BIT` and `MYSQL_TYPE_YEAR` are not valid parameter types
  for `COM_STMT_EXECUTE`. Send `MySQLBit` as `MYSQL_TYPE_LONGLONG`
  (unsigned, little-endian) and `MySQLYear` as `MYSQL_TYPE_SHORT`
  (unsigned, 2-byte integer) instead.
+ Fix `MySQLYear` binary encoding: was sending a length-encoded string
  which caused data misalignment for all subsequent parameters.
+ Fix version detection for MariaDB 10+/11+ (fractional seconds support).
+ Fix CI: enable binary logging and grant binlog privileges for
  integration tests on MariaDB.
+ Add roundtrip integration tests for `MySQLBit` and `MySQLYear`.

## 1.1.8
+ bump constraints

## 1.1.7 -- 2025.08.23 
+ bump constraints
+ upgrade cabal file
+ add upper bounds to all the warnings
  + it was only the test suite so I made them broad.
+ Disable inline-rule-shadowing for Int24 and Word24.
  those rules fire fine. you can test it our yourself with
  the Main.hs in the main repository.
  There is a stanza to do so in cabal.
+ clean up cabal file a bit, use GHC2021

## 1.1.6 -- 2024.10.23 
+ bump constraints

## 1.1.5 -- 2024.06.25
+ bump constraints

## 1.1.4 -- 2024.02.17 
+ bump constraints

## 1.1.3 -- 2023.08.30
+ bump constraints

## 1.1.2 -- 2023.08.14

+ Fix package name of changelog
+ Drop support for RC4 chipher which is depracated
+ drop dependency on binary-ieee754, which was unused.
+ Fix text 2 support, thanks @RikvanToor

## 1.1.1 -- 2023.08.14

+ cleaned up some warnings
+ Merge back into mysql-haskell after gaining hackage access.
+ Deprecate mysql-pure in favor of old hackage
  since it's only been out for a day this sort off
  stream lines upgrading for most applications.
  Cabal will just figure it out, rather then
  users having to "find" mysql-pure.
  I'll just make a bonus announcement to
  let people not depend on mysql-pure.

## 1.1.0 -- 2023.08.12 
There was a bunch of stuff unrelated to mysql
which I purged.
If you need any on these go depend on the 
respective unmaintained package.

+ Delete module System.IO.Streams.UnixSocket
+ Dleete module Data.Binary.Parser.Char8
+ Delete module System.IO.Streams.Binary

## 1.0.2 -- 2023.08.12 
+ Bump dependencies, go all into crypton
+ merge tcp-streams into the package

## 1.0.1 -- 2023.08.12 
+ add json testfiles as extra source files to make tests pass in nix builds

## 1.0.0 -- 2023.08.12 

+ Fork from mysql-haskell into mysql-pure
+ add flake
+ merge packages:
  + word24
  + binary-parsers
  + wirestreams
  
  This involved copying over all source files,
  furthermore I copied in all tests and benchmarks.
  The tests are now one giant test suite.
  I temporarly disabled the mysql tests as they need a mysql
  database to run which won't work nicely with CI right now.
  However you can run these locally by uncommenting that line.
+ Add CI which relies on native cabal instead of stack
+ Add an action to automatically bump version.
+ Add nightly build cron job.

## 0.8.4.3 -- 2020-11-04

* Fix build with GHC 8.8.

## 0.8.4.2 -- 2019-01-22

* Fix [stackage#4312](https://github.com/commercialhaskell/stackage/issues/4312): Relax `network` bounds.

## 0.8.4.1 -- 2018-10-23

* Relax `tasty` version bound to build with latest stackage. [#26](https://github.com/winterland1989/mysql-haskell/pull/26)

## 0.8.4.0  -- 2018-10-23

* Add `executeMany_` to execute batch SQLs, [#26](https://github.com/winterland1989/mysql-haskell/issues/26).
* Optimize connection closing sequence, [#20](https://github.com/winterland1989/mysql-haskell/pull/20), [#25](https://github.com/winterland1989/mysql-haskell/pull/25).

## 0.8.3.0  -- 2017-10-09

* Remove unnecessary exports from `Database.MySQL.Base`.
* Reuse TCP connection when using TLS.
* Clean up some compiler warnings.

## 0.8.2.0  -- 2017-10-09

Courtesy of naushadh, `mysql-haskell` will be on stackage again.

* Update to use `tcp-streams-1.x`.
* Fix compatibility with new  `tls/memory` version.

## 0.8.1.0  -- 2016-11-09

* Add `Show` instance to `ConnectInfo`.
* Add proper version bound for `binary`.

## 0.8.0.0  -- 2016-11-09

* Add `ciCharset` field to support `utf8mb4` charset.
* Add `BitMap` field to `COM_STMT_EXECUTE`, and [#8](https://github.com/winterland1989/mysql-haskell/pull/8) by [alexbiehl](https://github.com/alexbiehl).

## 0.7.1.0 -- 2016-11-21

* Add `QueryParam` class and `Param` datatype for multi-valued parameter(s) by [naushadh](https://github.com/naushadh).

## 0.7.0.0 -- 2016-11-09

* Split openssl support to [mysql-haskell-openssl](http://hackage.haskell.org/package/mysql-haskell-openssl).
* Expose `Database.MySQL.Connection` module due to this split, it shouldn't be used by user directly.

## 0.6.0.0 -- 2016-10-25

* Use binary-ieee754 for older binary compatibility.
* Clean up `Database.MySQL.Protocol.MySQLValue` 's export.

## 0.5.1.0 -- 2016-10-20

* Add `queryVector`, `queryVector_` and `queryStmtVector`.
* Use binary-parsers to speed up binary parsers.

## 0.5.0.0 -- 2016-8-22

* Export exception types.
* Fix a regression cause password authentication failed, add tests.
* Fix a reading order bug cause 'prepareStmt/prepareStmtDetail' failed.

## 0.4.0.0 -- 2016-8-22

* Enable TLS support via `tls` package, add benchmarks.

## 0.3.0.0  -- 2016-8-22

* Fix tls connection, change TLS implementation to HsOpenSSL, add benchmarks.
* Fix a bug in 'putLenEncInt' which cause sending large field fail.
* Various optimizations.

## 0.2.0.0  -- 2016-8-19

* Fix OK packet decoder.
* Fix sending large packet(>16M).
* Add `executeMany`, `withTransaction` to Base module.
* Add timestamp field to `RowBinLogEvent`.
* Add test, add insert benchmark.

## 0.1.0.0  -- 2016-8-16

* First version. Released on an unsuspecting world.
