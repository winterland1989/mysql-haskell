# Revision history for mysql-haskell

## 0.3.0.0  -- 2016-8-19

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
