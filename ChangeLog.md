# Revision history for mysql-haskell

## 1.4.0 -- 2026.11.11

Fixes for MySQL and MariaDB alike, found comparing mysql-haskell with the
mariadb-haskell fork, and faster row decoding. A text query no longer loses a
row whose first value is 16 MiB or longer; insert IDs above 2^63 come back
intact; a `?` inside a quoted string or a comment is no longer taken for a
placeholder; dates such as `0000-00-00` round-trip instead of turning into
`0000-01-01`; latin1 results decode; FLOAT and DOUBLE values in exponent form
no longer lose their exponent through `query_` (`1e20` came back as 1); the
binlog row decoder no longer drops a statement's rows events after its first;
and the Mozilla CA bundle is installed with the library. `query_` and
`queryStmt` resolve each column's type once per result set, and the new
`Database.MySQL.Decoder` decodes rows straight into Haskell values with typed
errors through `queryRows_`, `queryRows` and `queryStmtRows`, or by column
number through `queryRawRows_` and `queryStmtRawRows`.

New in `Database.MariaDB`, for MariaDB servers only: connections that ask for
capabilities only MariaDB has. Extended column metadata names JSON, UUID, INET6
and geometry columns through `prepareStmtMetadata`, and `executeBulk` runs a
prepared statement for many parameter rows in one round trip, several times
faster than `executeMany`. Its connect functions refuse a MySQL server with
`NotMariaDB`; `mysqlConnection` gives the underlying connection for everything
in `Database.MySQL.Base`. Connections made with `Database.MySQL.Base` send the
same handshake as before.

Upgrading can break code. `okLastInsertID` is a `Word64`. `MySQLValue` gains
`MySQLDateRaw`, `MySQLDateTimeRaw` and `MySQLTimeStampRaw` for dates a `Day`
cannot hold, `DecodeError` gains `DecodeUnrepresentableDate`, and
`BinLogEventType` gains `BINLOG_UNRECOGNIZED_EVENT` and loses its `Enum`
instance (use `binLogEventTypeCode`). A connection whose reply was interrupted,
for example by a timeout, now refuses further commands with `AbandonedReply`
instead of handing the rest of that reply to the next query (#10); discard it
and reconnect. `resetStmt` with rows still unread raises `UnconsumedResultSet`,
`withTransaction` rethrows the procedure's own exception, and
`decodeRowBinLogEvent` raises `RowEventDecodeException` on an event that may
hold rows it cannot decode, such as MySQL's compressed transactions, where it
used to crash or skip it. `Database.MySQL.Base` now also exports the five new
query functions and re-exports `Database.MySQL.Protocol.RawRow`, which can clash
with same-named definitions under an unqualified import.

It needs GHC 9.6 or newer, for the new `unwitch` dependency, and bytestring
0.11.2; projects on older compilers keep resolving to 1.3.3.

* Fix eight defects for MySQL and MariaDB, each with a reproducer
  [#102](https://github.com/winterland1989/mysql-haskell/pull/102)
* Faster row decoding, decoders straight into Haskell values, and FLOAT and
  DOUBLE in exponent form read correctly
  [#98](https://github.com/winterland1989/mysql-haskell/pull/98)
* A MariaDB module with capability negotiation and extended column metadata
  [#100](https://github.com/winterland1989/mysql-haskell/pull/100)
* MariaDB bulk execution with NULL, DEFAULT and IGNORE parameters
  [#101](https://github.com/winterland1989/mysql-haskell/pull/101)
* `Database.MariaDB` for MariaDB servers only, with `mysqlConnection` for the
  MySQL functions [#104](https://github.com/winterland1989/mysql-haskell/pull/104)
* Allow crypton 2.2 and tls 2.5
  [#105](https://github.com/winterland1989/mysql-haskell/pull/105)
* A comparison with the mariadb-haskell fork and a plan to adopt its
  features, in `docs/`
  [#99](https://github.com/winterland1989/mysql-haskell/pull/99)
* Benchmark figures re-measured on current hardware
  [#95](https://github.com/winterland1989/mysql-haskell/pull/95)
* A profile of where the CPU time goes, in `benchmark/`
  [#96](https://github.com/winterland1989/mysql-haskell/pull/96)

## 1.3.3 -- 2026.10.07

New functions, and fixes to `executeMany` and `executeMany_` for inputs that
left 1.3.2's connection out of step. `queryMulti_` and `queryMulti` return every
result of a CALL or a multi-statement query in order: each result set with its
columns and rows, and each statement's `OK`.

`executeMany` and `executeMany_` now read the whole reply. A statement that
returns rows raises `ExtraResultSets`, where 1.3.2 threw `UnexpectedPacket` or
left the rows for the next command. A query with several statements per
parameter set gives one `OK` per statement, where 1.3.2 returned one per
parameter set and left the rest. `executeMany` with no parameter sets returns
`[]` without contacting the server, where 1.3.2 sent an empty query whose error
reached the next command.

+ Every result set of a CALL or multi-statement query through `queryMulti_`
  [#94](https://github.com/winterland1989/mysql-haskell/pull/94)

## 1.3.2 -- 2026.10.07

Requires crypton 2.0 or newer, the first crypton with the timing side-channel
fixes that TLS connections reach; a project held to crypton 1.x keeps
resolving to 1.3.1. The only API addition is the `ExtraResultSets` exception.
The query functions now return an empty result for an INSERT, UPDATE or DELETE
instead of hanging. `execute_`, `executeStmt` and the query functions read the
whole reply of a CALL or a multi-statement query, so they no longer leave
results behind for the next command; `executeMany` and `executeMany_` are
unchanged (#93).

Errors change only for statements that produce several results, or rows where
`execute_` and `executeStmt` expect none. Those two raise `ExtraResultSets` for
a result set (1.3.1 threw `UnexpectedPacket` when it came first and returned the
`OK` when it came later); the query functions raise it after the first result
set's rows when a second one follows, which 1.3.1 left on the connection. A
later statement that fails raises its `ERRException` from that call or its row
stream, also ahead of `ExtraResultSets`. Stack users who set the removed
`crypton-1-1` flag must delete it from `stack.yaml`, since Stack rejects flags a
package does not define.

+ Query functions no longer hang on statements without a result set,
  multi-result replies are read to the end, and crypton 2 is required
  [#91](https://github.com/winterland1989/mysql-haskell/pull/91)
+ CI: the cabal job builds and runs the test suite
  [#90](https://github.com/winterland1989/mysql-haskell/pull/90)

## 1.3.1 -- 2026.10.06
+ Allow crypton 2.0 and 2.1 (commercialhaskell/stackage#8128). The library
  code needed no change; CI now builds and tests against every crypton
  series the bounds admit (0.34, 1.0, 1.1, 2.0 and 2.1).

## 1.3.0 -- 2026.07.22
+ Add `Database.MySQL.Field` with a `Field` typeclass for converting
  individual Haskell values to and from `MySQLValue` (#86). One combined
  class in the style of persistent's `PersistField`, with a `DecodeError`
  sum type for decode failures and named top-level encode/decode functions
  backing every instance.

## 1.2.5 -- 2026.05.04
+ Replace abandoned `pem` dependency with `crypton-pem` (#84)

## 1.2.4 -- 2026.04.20
+ Fix benchmark compilation failures (#82):
  + Replace deleted `Data.Binary.Parser.Char8` import in benchmark with
    `Word8`-based combinators from `Data.Binary.Parser`
  + Replace `GHC.Prim`/`GHC.Types` imports with `GHC.Exts` from `base`
  + Fix `Word8#` vs `Word#` type mismatch in `mhDigitToInt`
  + Remove stale haddock reference to deleted `Char8` module

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
