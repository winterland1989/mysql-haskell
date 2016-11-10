# Revision history for mysql-haskell

## 0.7.0.0 -- 2016-11-09

* Split openssl support to [mysql-haskell-openssl](hackage.haskell.org/package/mysql-haskell-openssl).
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
