# mysql-haskell 2.0 plan

A proposal from 13 November 2026, written while 1.6.0 was being prepared;
nothing in it is implemented. No earlier 2.0 plan exists: the branches, the
issues and the pull requests of winterland1989/mysql-haskell mention none.
The closest is docs/mariadb-haskell-fork.md, which names the exported records
(`Greeting`, `Auth`, `ColumnDef`, `BinLogEventType`) that kept MariaDB's
extra fields out of 1.x; this plan takes that up.

Three of the 1.6.0 pull requests were still open when this was written:
#115 (binlog checksums, `decodeBinLogPacketFollowing`), #117 (long data,
`executeStmtStreamed`) and #118 (cursors, `queryStmtCursor`). The parts of
this plan that mention them assume they merge as they are.

## Goals

1. Every public name has one home. A user reads one module's Haddock for
   one thing, and the package's Haddock index shrinks from 28 modules to
   about seven.
2. The records that are likely to grow (`ConnectInfo`, `MySQLConn`,
   `Command`, `ColumnDef`, `OK`, `Greeting` and the binlog event records)
   get abstract constructors, so a later release can add a field without
   another major version. 1.x could not add state to a connection
   without a break, and several fixes waited for that (listed below).
3. The public API no longer exposes io-streams, and monad-loops and
   data-default-class leave the dependency list.

## Module layout

Today 28 modules are exposed, and many names are exported from two of them:
`Database.MySQL.Base` re-exports six whole `Protocol` modules,
`Database.MySQL.Decoder` re-exports `Protocol.RawRow`, `Database.MySQL.BinLog`
re-exports three `BinLogProtocol` modules and `Database.MySQL.TLS` re-exports
`Data.TLSSetting`. In 2.0 a name is exported from one public module, and a
re-export only happens from a hidden module.

| 1.x module | 2.0 |
| --- | --- |
| `Database.MySQL.Base` | renamed `Database.MySQL`: connecting, queries, statements, `MySQLValue`, `ColumnDef`, `OK`, `ConnectInfo`, the limits, the exceptions, escaping |
| `Database.MySQL.Connection` | hidden; its own header already calls it internal. The public names (`ConnectInfo`, `ConnectionLimits`, the exceptions) move to `Database.MySQL` |
| `Database.MySQL.TLS` | stays; also exports what users need from `Data.TLSSetting` (`TrustedCAStore`, `makeClientParams`, `makeClientParams'`) |
| `Database.MariaDB` | stays |
| `Database.MySQL.BinLog` | stays; exports the event and value types of `BinLogProtocol.*`, and `Int24`, which `BinLogInt24` carries |
| `Database.MySQL.Decoder`, `Database.MySQL.Field` | stay; `RawRow` is exported from one of them only |
| `Database.MySQL.Protocol.*` (Auth, ColumnDef, Command, Escape, FloatingPoint, MySQLValue, Packet, RawRow) | hidden, with no `Database.MySQL.Protocol` in their place. The few names downstream code uses (see "Who breaks") are exported from `Database.MySQL` |
| `Database.MySQL.BinLogProtocol.*` | hidden |
| `Data.Binary.Parser`, `.Numeric`, `.Word8` | hidden (vendored parser) |
| `Data.Connection`, `System.IO.Streams.TCP`, `System.IO.Streams.TLS` | hidden, then gone with io-streams |
| `Data.TLSSetting` | hidden; `makeServerParams` and `makeServerParams'` move to the internal library, as only the test suite's TLS servers (test/TCPStreams.hs, test/TLSServerName.hs) use them |
| `Data.Int.Int24`, `Data.Word.Word24` | hidden; `Int24` is re-exported by `Database.MySQL.BinLog`, `Word24` only appears in the hidden packet getters |

Hiding the vendored modules also ends a clash that exists today: binary-parsers
exposes `Data.Binary.Parser*`, tcp-streams exposes `Data.Connection`,
`Data.TLSSetting` and `System.IO.Streams.TCP`/`TLS`, word24 exposes
`Data.Int.Int24` and `Data.Word.Word24`, and the unrelated connections package
exposes `Data.Connection`. A component that depends on mysql-haskell (1.1 or
later) and one of those gets "Ambiguous module name" without
`PackageImports`. `Database.MySQL` is exposed by no other package on Hackage,
where `Database.MySQL.Base` clashes with the C binding `mysql`.

`Database.MySQL.Protocol.MySQLValue` (1081 lines) gets split along the
way, which is easier once it is hidden.

Tests reach the hidden modules through a private internal library
(`library internal` with `visibility: private`, which the test suites
depend on), so the hidden code stays testable without showing in the
package's Haddock.

## Abstract constructors

Export the type and its field selectors, not the constructor. Record update
keeps working (`defaultConnectInfo { ciUser = "app" }`); building the record
with the constructor, positionally or by field name, and pattern matching on
it stop compiling. 1.6.0 already exports `ConnectionLimits` this way.

- `ConnectInfo`: built from `defaultConnectInfo` or `defaultConnectInfoMB4`
  only.
- `MySQLConn`: built by the connect functions only. Issue #27 (2018) shows
  what that costs: a Selda backend wanted rows without `MySQLValue` and the
  affected-rows count from one call that may or may not return rows, and the
  answer was to use the exported constructor with `readPacket` and
  `waitCommandReply`. The direct row decoding of 1.4.0 (`queryRows`,
  `queryRawRows_`) covers the rows, but an OK reply comes back as an empty
  stream and its count is dropped. Before the plumbing is hidden, 2.0 needs a
  public query that returns either the decoded rows or the OK, so a backend
  like that has a supported way.
- `ColumnDef`, `OK` and `Greeting`: field selectors only. MariaDB's extended
  metadata had to go into a wrapper (`ColumnMetadata` in `Database.MariaDB`)
  because `ColumnDef`'s constructor is exported. `MySQLValue` keeps its
  constructors, since matching on them is how a value is read; a new MySQL
  type remains a major change.
- `Command`: hidden with the `Protocol` modules. The cursor (#118) and long
  data (#117) commands are written outside it because a new constructor
  would have broken code matching on it.
- `ReplyState`, `Packet`'s constructor and the plumbing
  `Database.MySQL.Connection` exports for the TLS and MariaDB modules
  (`runCommand`, `writeCommand`, `readPacket`, `completeAuth` and the
  rest): hidden.

## What a field on `MySQLConn` makes possible

These were postponed in 1.x because each needs state on the connection:

- Limits on the rows and result sets `queryMulti` and `queryMulti_` keep in
  memory, as fields of `ConnectionLimits` (#116 lists them as not bounded).
- A cursor generation, so a stream from `queryStmtCursor` refuses to read
  the rows of a later execution of the same statement (#118 documents this
  as a sharp edge).
- The connection's character set, so `escapeText` and `escapeBytes` can
  refuse or correctly escape gbk, big5, sjis, cp932 and gb18030 text, whose
  multibyte characters can swallow the escaping backslash (the 1.5.0
  changelog documents the hole).
- `waitCommandReplys`, exported with only a packet stream and therefore
  unbounded, goes with the hidden plumbing.

## Dependencies

### io-streams

Every query function that streams rows returns an io-streams
`InputStream` (all but `queryMulti` and `queryMulti_`, which return lists),
and the connection itself reads and writes through io-streams (`Data.Connection`,
`System.IO.Streams.TCP`, `System.IO.Streams.TLS`). io-streams is not
abandoned (1.5.2.2 was uploaded in January 2026); the case for replacing it
is that users have to learn and depend on it to read a result, and that a
row stream's state (unread rows block the connection, `Stream.skipToEof`)
lives in a library that knows nothing about it.

The proposal:

- Inside, read packets from the socket with `network`'s `recv` and tls's
  `recvData` into one buffer, with no streaming library.
- Outside, a small row stream of our own, `RowStream a`, with `nextRow ::
  RowStream a -> IO (Maybe a)`, `skipRemainingRows`, and folds
  (`foldRows`, `rowsToList`) for the common cases. It carries the reply
  state the connection needs, so the rule "read or skip the rows before the
  next command" is the type's own.
- Two library-agnostic forms on top of it, so conduit, pipes, streaming or
  io-streams users plug in without the package depending on any of them:
  - A fold in the shape foldl's `impurely` takes,
    `foldRowsM :: (x -> a -> IO x) -> IO x -> (x -> IO b) -> ... -> IO b`.
    foldl documents `purely` and `impurely` as the way to accept its folds
    "without incurring a foldl dependency", and pipes, streaming and
    io-streams folds already have that shape
    ([Control.Foldl](https://hackage.haskell.org/package/foldl/docs/Control-Foldl.html)).
    The fold runs inside the query function, so every row is read before it
    returns and the connection's reply state can never be left mid-result.
  - A polymorphic source, `rows :: (MonadIO m, MonadPlus m) => ... -> m a`,
    which any `ListT` with `MonadPlus` and `MonadIO` instances runs (pipes'
    `ListT`, logict, list-t), as Gabriella Gonzalez describes in
    [How to build library-agnostic streaming sources](https://haskellforall.com/2014/11/how-to-build-library-agnostic-streaming)
    (2014). A consumer can stop early, so this form needs the same
    unread-rows guard `RowStream` has, and transformers' `ListT` does not
    stream (the post says so); both need benchmarks, as the polymorphic code
    has to specialise to stay fast.
- Adapters beyond those stay out of the package: a few lines each in the
  documentation, or small separate packages if anyone asks.

The alternative, exposing conduit or streaming instead, moves the
dependency rather than removing it; conduit is the best maintained of them
(1.3.6.1, February 2025) but heavy for a driver.

This is the largest change and touches the code the 1.3 to 1.4 speed work
tuned (`benchmark/`); it merges only with benchmark numbers no worse than
1.6.0's.

### monad-loops

Three calls of `untilM` in `Database.MySQL.BinLogProtocol.BinLogEvent`, and
nothing else. A local loop replaces them. No API changes, so this can ship
in any 1.6.x.

### data-default-class

Its latest release (0.2.0.0) is a compatibility shim over data-default, and
mysql-haskell imports it only in `Data.TLSSetting`, for tls older than 2.1.4
(released November 2024). Raising the tls lower bound to 2.1.4 removes it,
and with it the `-Wunused-packages` warning the newer tls versions give. A
bounds change, so this too can ship in a 1.6.x.

## Smaller changes

- Booleans become sum types: the checksum and semi-sync flags of
  `decodeBinLogPacket` and `decodeBinLogPacketFollowing`, and the results of
  `isCheckSumEnabled` and `isSemiSyncEnabled`, which callers pass on to
  them.
- The exception types stay separate but are exported from
  `Database.MySQL` in one list.

## Who breaks

Measured on 13 November 2026 from Hackage's reverse dependencies, the
latest tarball of each, the Hackage index and GitHub code search (a lower
bound, as GitHub's index is incomplete).

| Package | Latest | Bound lets 2.0 in | Uses | Breaks on |
| --- | --- | --- | --- | --- |
| hasqlator-mysql | 0.2.2, January 2026 | yes | `Database.MySQL.Base`, `putTextField`, `Streams.toList` on a result (`Hasqlator.hs:338`) | the rename, the io-streams removal |
| hasqly-mysql | 0.0.1, 2020 | yes | `Base` (hiding `Query`, `Command`), `putTextField` | the rename |
| mysql-haskell-nem | 0.1.0.0, 2016, in Stackage nightly | yes | `Base`, `ColumnDef(..)` in imports, `columnName`, `StmtID`, and `InputStream` in its own API (`Nem.hs:24`, `:38`); no use of `ColumnDef`'s constructor | the rename, the io-streams removal |
| users-mysql-haskell | 0.5.2.0, 2016 | yes | `Base`, `System.IO.Streams.List.toList` on results (`Web/Users/MySQL.hs:321`) | the rename, the io-streams removal (its tests already fail: they build a 5-field `ConnectInfo`) |
| persistent-mysql-pure (Jappie) | 1.0.4, March 2026 | no (`< 2.0`) | `Base`, `Protocol.Escape`, `TLS`, a pattern match on `ConnectInfo`'s constructor (`Database/Persist/MySQL.hs:1289`), `putTextField`, `okAffectedRows`, `Streams.read` and `skipToEof` on results (`:253` to `:275`) | the rename, the hidden constructor, the hidden `Escape` module, the io-streams removal |
| persistent-mysql-haskell, strongswan-sql, mysql-haskell-openssl | 2019 to 2020 | no | | nothing new; their bounds already exclude current 1.x, and mysql-haskell-openssl, the only Hackage user of `MySQLConn`'s constructor and the `Connection` plumbing, is already incompatible with it (`MySQLConn` changed arity) |

On GitHub, outside copies of mysql-haskell:

- juspay/euler-hs and its active fork nammayatri/euler-hs build `ConnectInfo`
  with its constructor (`EulerHS/Core/Types/MySQL.hs:154`,
  `EulerHS/SqlDB/MySQL.hs:149`); kowainik/mysql-not-so-simple does in its
  tests. About 25 repositories only record-update `defaultConnectInfo`, which
  keeps working.
- ikaro1192/PureMyHA and an ICFP contest entry import `ERR` from
  `Database.MySQL.Protocol.Packet`; icfpc2024 and mysql-not-so-simple import
  `utf8mb4_unicode_ci` from `Database.MySQL.Connection`.
- Nobody outside mysql-haskell uses `MySQLConn`'s internals
  (`mysqlRead`, `isConsumed`), `Data.TLSSetting`, `System.IO.Streams.*` or
  `Data.Binary.Parser` from this package.

So `Database.MySQL` has to export, besides the obvious: `putTextField`,
`OK(..)` with `okAffectedRows`, `ERR(..)`, `Query(..)`, `StmtID`,
`ColumnDef`'s field selectors and the character set constants
(`utf8_general_ci`, `utf8mb4_unicode_ci`); and `Database.MySQL.TLS` keeps
`TrustedCAStore(..)`, `makeClientParams` and `makeClientParams'`.

GHC 9.10 confirms the cost of the abstract constructors: with only the type
and its fields exported, `import Database.MySQL (ConnectInfo (..))` and
`defaultConnectInfo { ciHost = ... }` compile without warnings, while
building the record or matching on it fails (GHC-01928). A `hiding (Command)`
of a name no longer exported is only a `-Wdodgy-imports` warning.

The module changes and the abstract constructors alone would leave the four
unbounded packages working if `Database.MySQL.Base` survived as a deprecated
module. The io-streams removal does not: hasqlator-mysql, mysql-haskell-nem
and users-mysql-haskell read results as `InputStream`s, and so does
persistent-mysql-pure; only hasqly-mysql does not. Every live user therefore
has to change something for 2.0, unless a compatibility layer keeps the old
types.

The proposal for that layer is a separate package, mysql-haskell-io-streams,
with a deprecated `Database.MySQL.Base` that has the 1.x signatures,
returning `InputStream`s converted from `RowStream`. It keeps io-streams out
of the core package, keeps the core's rule that a name has one public home,
and turns the 2.0 upgrade of the four unbounded packages into one added
dependency. persistent-mysql-pure needs a new release anyway (its `< 2.0`
bound, its constructor match, its `Escape` import), and moves to the new
API directly.

The alternative is two major versions: 2.0 for the modules and the
constructors, 3.0 for io-streams. It breaks users twice for a smaller
first step.

## Order of work

1. In 1.6.x, nothing breaks: drop monad-loops, raise the tls floor and drop
   data-default-class.
2. 2.0, one pull request per item, so each can be reviewed alone:
   1. the module moves and the hidden modules (mechanical, the largest diff);
   2. the abstract constructors;
   3. the io-streams replacement, gated on the benchmarks;
   4. the mysql-haskell-io-streams compatibility package with the
      deprecated `Database.MySQL.Base`, released together with 2.0, and
      persistent-mysql-pure moved to the new API in the same week;
   5. the fields on `MySQLConn` and the fixes they unlock.
3. The 2.0 changelog carries a table from every removed or moved name to
   its replacement. 1.6.x keeps getting fixes for a while after 2.0.
