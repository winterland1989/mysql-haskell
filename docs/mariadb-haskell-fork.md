# mariadb-haskell compared with mysql-haskell

Written 11 November 2026 after the
[mariadb-haskell 0.1.0.0 announcement](https://discourse.haskell.org/t/ann-mariadb-haskell-0-1-0-0-a-mariadb-focused-driver-now-on-hackage-alpha/15032).
It records what the fork adds, which of its findings are defects in
mysql-haskell, and how mysql-haskell can take the MariaDB work in without
dropping MySQL.

Versions compared:

| | Commit | Notes |
|---|---|---|
| mysql-haskell | `92a9401` (1.3.3 plus #96) | master on 11 Nov 2026 |
| mariadb-haskell | `9f2a1ca` (0.1.0.0) | forked from `92a9401`, 35 commits from 6 to 9 Nov 2026 |

The fork adds about 6,000 lines and removes about 1,500 under `src/`
and `cbits/`. The remaining 2.5 million added lines are validation
results. Every module keeps its `Database.MySQL.*` name and no export of
`Database.MySQL.Base` was removed. The package supports MariaDB only.

## What the fork adds

### Features only MariaDB has

| Feature | Protocol element |
|---|---|
| Bulk execution of a prepared statement over many parameter rows, with NULL/DEFAULT/IGNORE indicators and a result per row | `COM_STMT_BULK_EXECUTE` (250), `MARIADB_CLIENT_STMT_BULK_OPERATIONS` |
| Prepared statement metadata cache: execute replies skip the column definitions | `MARIADB_CLIENT_CACHE_METADATA` |
| Extended column metadata (for example JSON and UUID type names) | `MARIADB_CLIENT_EXTENDED_METADATA` |
| Progress reports for long statements | `MARIADB_CLIENT_PROGRESS` |
| ed25519 and PARSEC authentication | `client_ed25519`, `parsec` plugins |
| PAM dialogs and Kerberos | `dialog`, `auth_gssapi_client` plugins |
| GTID resume, MariaDB binlog events 160 to 171, compressed binlog events | `@slave_connect_state`, `@mariadb_slave_capability` |

The four `MARIADB_CLIENT_*` flags are bits 32 to 36 of the capability set.
`include/mariadb/mariadb_com.h` in mariadb-connector-c 3.3.5 defines them.

### Features MySQL has as well

These are standard protocol features. They belong in the shared modules
and should be tested on both servers.

| Feature | Protocol element |
|---|---|
| Reset a pooled connection | `COM_RESET_CONNECTION` |
| Session state changes in OK packets | `CLIENT_SESSION_TRACK` |
| Server-side cursors | `COM_STMT_FETCH` |
| Streamed long parameters | `COM_STMT_SEND_LONG_DATA` |
| zlib compression | `CLIENT_COMPRESS` |
| OUT parameters from a prepared `CALL` | `CLIENT_PS_MULTI_RESULTS` |
| Semisync acknowledgements sent after the consumer stored the event | semisync reply packet |
| Binlog CRC32 verification | `binlog_checksum` |
| TLS client certificates | |
| `Show ConnectInfo` without the password | |

The fork also changes behaviour beyond the features: utf8mb4 is the
default charset; a timeout or cancellation during protocol I/O retires the
connection; messages are capped at 64 MiB and metadata at 16 MiB; and
`MySQLValue` gains `MySQLDateRaw`, `MySQLDateTimeRaw` and
`MySQLTimeStampRaw`.

### Costs of the fork's approach

* Linux builds need MIT Kerberos (`extra-libraries: gssapi_krb5` with no
  flag), so the driver is no longer pure Haskell.
* GHC 9.4 or newer and MariaDB only. Windows still builds (GSSAPI is
  stubbed out there) but is not a verified platform and left the fork's CI.
* The fork's own report (`validation/RESULTS.md`) measures ordinary
  workloads slower than 1.3.3: `latency.prepared` 23.3%,
  `read.text.narrow` 14.5% and `read.prepared.narrow` 11.7%. Between the
  same two runs its unchanged MariaDB Connector/C control also slowed, by
  5.6% to 12.4% (`read.text.narrow` 8.0%), and the announcement says the
  host had background activity. Only `latency.prepared` is flagged there
  for a recheck on an idle machine.
* The speed-ups in the announcement compare new APIs against other APIs of
  the same build, for example bulk execution against one prepared insert
  per row. None of them compares against mysql-haskell.

## The fork's baseline against 1.3.3

`validation/baselines/macos-arm64-mariadb-12.3.3/SUMMARY.md` runs the
unchanged driver against MariaDB 12.3.3: 246 checks pass, 56 fail, 11 are
unsupported and 2 lack an environment. The 56 failures are not 56 bugs.

| Failures | Status |
|---|---|
| 12 × `cdc.native-event-160` to `171` | one defect, the `toEnum` crash below |
| 6 × `auth.ed25519.*`, `auth.parsec.*` | plugins mysql-haskell never supported |
| `reliability.timeout-before-metadata` | defect: the next query reads the interrupted query's reply (below). The fork changed the expected outcome on 9 Nov from reuse to retirement (`docs/ROADMAP.md`); 1.3.3 fails both |
| `connection.default-full-unicode` | documented default; `defaultConnectInfoMB4` gives utf8mb4 |
| `transaction.nested-preserves-outer` | `withTransaction` never promised nesting |
| `auth.sha2.tcp`, `auth.mutual-tls-helper-client`, `connection.localhost-address-fallback` | not investigated |
| the rest | defects, partly confirmed below |

## Defects confirmed in mysql-haskell

Each of these was checked at `92a9401`, from the source and, for the
interrupted query, by running it against MariaDB 11.4.9 and MySQL 8.0.45.
They affect MySQL users as much as MariaDB users.

| Defect | Location |
|---|---|
| `isEOF` only checks for a leading `0xFE`. A row whose first column is 16 MiB or longer starts with the same byte and is read as the end of the result; the packet length must be checked as well. An empty packet makes `L.index` crash | `Protocol/Packet.hs:71` |
| `okLastInsertID :: Int`. An unsigned 64-bit ID above `maxBound :: Int` comes back negative | `Protocol/Packet.hs:133` |
| `renderParams` splits on every `?`, including those inside string literals and comments, so `SELECT '?'` raises `WrongParamsCount` | `Query.hs:72` |
| `fromGregorian` clamps, so `0000-00-00` comes back as `0000-01-01`, in the text protocol (`dateParser`, used for DATE, DATETIME and TIMESTAMP) and in the binary protocol | `Protocol/MySQLValue.hs:206` (text), `311,328,346` (binary) |
| `T.decodeUtf8` on text columns throws when the connection delivers latin1 bytes | `Protocol/MySQLValue.hs:189,377` |
| `toEnum` on the binlog event type calls `error` for any type it does not know, such as MariaDB's 160 to 171 or MySQL 8.0's 36 to 41 (`TRANSACTION_PAYLOAD_EVENT` is 40) | `BinLogProtocol/BinLogEvent.hs:99` |
| A query interrupted by an asynchronous exception, such as `timeout` while it waits for the reply, leaves that reply on the connection. The next query on the connection returns it: after `timeout 20000 (query_ c "SELECT SLEEP(0.2)")`, `SELECT 937` returns 0. Issue #10 ("Handling timeout") has been open since 2016 | `Connection.hs`, command and reply handling |
| `mozillaCAStore.pem` is listed in `extra-source-files` but read through `getDataFileName`, so `MozillaCAStore` fails in an installed library | `mysql-haskell.cabal:14`, `Data/TLSSetting.hs:46` |

The fork reports these as well; they are not yet checked:

* values of 16 MiB and longer (16777216, 16777217 and 33554430 bytes) fail,
  which may be the `isEOF` defect alone;
* binlog decoding loses rows when a transaction carries several table maps
  or repeats row events;
* binlog CRC32 checksums are not verified;
* text parameters are escaped with backslashes even under
  `NO_BACKSLASH_ESCAPES`;
* an exception inside `withTransaction` is replaced by
  `UnconsumedResultSet` when a result was left unread;
* caching_sha2 over a Unix socket asks for the RSA key instead of sending
  the password over the secure transport.

## Plan for mysql-haskell

### 1. Fix the defects for both servers

One pull request with a reproducer per defect, each shown to fail on
master. The integration suite already runs in two NixOS VMs from
`nix/ci.nix`: `integrated-checks` on MariaDB (11.4.9 in the pin) and
`integrated-checks-mysql80` on MySQL 8.0, so every fix is tested against
both.

### 2. A `Database.MySQL.MariaDB` module

Most MariaDB features can live in their own module, but the handshake
needs a hook in the core:

* A MariaDB server leaves `CLIENT_MYSQL` (bit 0, the old
  `CLIENT_LONG_PASSWORD`) out of its greeting and puts its extended
  capabilities in the last 4 of the 10 reserved greeting bytes.
  `getGreeting` skips those bytes (`Protocol/Auth.hs:100`).
* The client answers with the last 4 of the 23 reserved bytes in the
  handshake response and in the SSL request, and must leave bit 0 out
  to be treated as a MariaDB client. `clientCap` sets bit 0
  (`Protocol/Auth.hs:179`) and `putAuth` writes 23 zero bytes
  (`Protocol/Auth.hs:142`).

The connection must stay byte for byte the same unless the caller asks for
a MariaDB capability and the server offers it. MySQL servers set bit 0,
so they never see the change. `Greeting` and `Auth` are exported records,
so new fields there break pattern matches; the extended capabilities
should travel outside them.

Once negotiated, some capabilities change replies the core parses:
progress reports arrive as ERR packets with error number 65535 in the
middle of a reply, the metadata cache adds a "metadata follows" byte to the
column count packet, and extended metadata adds fields to the column definition. Each
needs a branch in the core reader that only runs when the capability was
negotiated.

`BinLogEventType` and `ColumnDef` are exported too. MariaDB event types
and extended metadata can go in separate types in the MariaDB module, or
into the existing types with a major version bump.

ed25519 needs nothing beyond crypton, which is already a dependency.
GSSAPI needs a C library, so it belongs behind a manual flag that is off
by default, or in a separate package. The pinned MariaDB 11.4.9 ships no
PARSEC plugin, so CI cannot test PARSEC yet.

### 3. A test suite that only runs on MariaDB

`test/Integration.hs` already includes the caching_sha2 tests only on the
MySQL 8.0 VM. A separate `integration-mariadb` test suite, run only in the
`integrated-checks` VM, keeps MariaDB-only tests apart from the shared
suite.

### 4. Bulk execution

Only MariaDB has bulk execution. `enum enum_server_command` in
`include/mysql/my_command.h` of MySQL 8.0.45 and 8.4.8 (both in the pin)
lists no bulk command; its last real entry is
`COM_SUBSCRIBE_GROUP_REPLICATION_STREAM`. On MySQL the closest options
remain `executeMany` and multi-row `INSERT ... VALUES`. Bulk execution
therefore goes in the MariaDB module behind
`MARIADB_CLIENT_STMT_BULK_OPERATIONS`.

## Sources

* Fork repository: <https://github.com/aantich/mariadb-haskell> at `9f2a1ca`,
  in particular `docs/MIGRATION.md`, `ChangeLog.md`, `validation/RESULTS.md`
  and `validation/baselines/macos-arm64-mariadb-12.3.3/SUMMARY.md`.
* mariadb-connector-c 3.3.5, `include/mariadb/mariadb_com.h`, for the
  command number and capability bits.
* MySQL 8.0.45 and 8.4.8, `include/mysql/my_command.h`.
* MySQL 8.0 `libbinlogevents/include/binlog_event.h`, for binlog event
  types 36 to 41.
* MariaDB knowledge base: [result set packets](https://mariadb.com/kb/en/result-set-packets/)
  (the `0xFE` row header and the metadata follows byte) and
  [progress reporting](https://mariadb.com/kb/en/progress-reporting/)
  (error number 65535).
* MariaDB 11.4.9 server plugin directory, for which authentication plugins
  the pinned server ships.
