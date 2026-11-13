# mysql-haskell 2.0 plan

A proposal from 13 November 2026, written while 1.6.0 was being prepared;
nothing in it is implemented. No earlier 2.0 plan exists: the branches, the
issues and the pull requests of winterland1989/mysql-haskell mention none.
The closest is docs/mariadb-haskell-fork.md, which names the exported records
(`Greeting`, `Auth`, `ColumnDef`, `BinLogEventType`) that kept MariaDB's
extra fields out of 1.x.

Three of the 1.6.0 pull requests were still open when this was written:
#115 (binlog checksums, `decodeBinLogPacketFollowing`), #117 (long data,
`executeStmtStreamed`) and #118 (cursors, `queryStmtCursor`). The parts of
this plan that mention them assume they merge as they are.

## Constraints

- One package. No compatibility or adapter packages next to it: each one is
  more to release and keep building.
- Keep maintenance small. The vendored modules (merged in from
  binary-parsers, tcp-streams and word24) are supporting code, not something
  to develop further.
- Break as little as possible. Commercial code bases use this package and
  can import anything it exposes, deeper than the public reverse
  dependencies below show.

## Goals

1. A Haddock index a user can read: the modules that make up the driver,
   not the vendored and wire-level ones.
2. The fixes 1.x postponed because they need state on the connection (listed
   under "What a field on `MySQLConn` makes possible").
3. Fewer dependencies: monad-loops and data-default-class go; io-streams
   leaves the main API.

## Step 1, no break: hide modules from the Haddock, not from users

Haddock's `hide` module attribute, `{-# OPTIONS_HADDOCK hide #-}`, means
"Omit this module from the generated documentation, but nevertheless
propagate definitions and documentation from within this module to modules
that re-export those definitions"
([Haddock, module attributes](https://haskell-haddock.readthedocs.io/latest/markup.html)).
The module stays in `exposed-modules`, so every import keeps compiling. That
gets goal 1 without a major version.

Of the 28 exposed modules, these get the attribute:

| Modules | Why |
| --- | --- |
| `Data.Binary.Parser`, `.Numeric`, `.Word8`, `Data.Connection`, `Data.TLSSetting`, `System.IO.Streams.TCP`, `System.IO.Streams.TLS`, `Data.Int.Int24`, `Data.Word.Word24` | vendored supporting code; its upstream packages keep their own documentation |
| `Database.MySQL.Protocol.*` (eight modules) | wire level; `Database.MySQL.Base` re-exports six of them, whose names then show on its page |
| `Database.MySQL.BinLogProtocol.*` (four modules) | `Database.MySQL.BinLog` re-exports the three users need |

That leaves seven in the index: `Database.MySQL.Base`,
`Database.MySQL.Connection`, `Database.MySQL.TLS`, `Database.MariaDB`,
`Database.MySQL.BinLog`, `Database.MySQL.Decoder` and `Database.MySQL.Field`.

Two things to settle in that pull request:

- A re-exported hidden module's contents "are always expanded in place in
  the re-exporting module" (same page), so `Database.MySQL.Base`'s page
  grows by the six `Protocol` modules. Replacing its `module ...` re-exports
  with the names users need (`MySQLValue(..)`, `ColumnDef` and its fields,
  `OK(..)`, `ERR(..)`, `putTextField`, `StmtID`, the field type constants)
  keeps the page short; the rest stays importable from the hidden modules.
- `Database.MySQL.Connection` mixes public names (`ConnectInfo`,
  the exceptions, the character set constants) with plumbing (`runCommand`,
  `readPacket`, `completeAuth` and so on, exported for the TLS and MariaDB
  modules). Hiding it too, with `Database.MySQL.Base` re-exporting its
  public names, would leave six modules in the index and give each public
  name one documented home. That reads the re-export rule as "one public
  home in the documentation", since the names stay importable from both
  modules.

Grouping the vendored modules under one namespace (`Database.MySQL.Support.*`
or similar) would also tidy the index, but it renames them, which breaks
every import of them, and it keeps them listed. The `hide` attribute gives
the shorter index without the break. Their names do clash with the upstream
packages (binary-parsers exposes `Data.Binary.Parser*`, tcp-streams
`Data.Connection`, `Data.TLSSetting` and `System.IO.Streams.TCP`/`TLS`,
word24 `Data.Int.Int24` and `Data.Word.Word24`): a component depending on
both gets "Ambiguous module name" and needs `PackageImports`. That clash
dates from 1.1 and no one has reported it; renaming to fix it can wait
until someone does.

How Hackage's module list on the package page shows hidden modules is not
checked yet; a local `cabal haddock` shows the effect on the generated
index.

## Step 2, no break: dependencies that can go in 1.6.x

### monad-loops

Three calls of `untilM` in `Database.MySQL.BinLogProtocol.BinLogEvent`, and
nothing else. A local loop replaces them.

### data-default-class

Its latest release (0.2.0.0) is a compatibility shim over data-default, and
mysql-haskell imports it only in `Data.TLSSetting`, for tls older than 2.1.4
(released November 2024). Raising the tls lower bound to 2.1.4 removes it,
and with it the `-Wunused-packages` warning the newer tls versions give.

## Step 3, 2.0: what needs a break

### A new `Database.MySQL`, with `Database.MySQL.Base` kept

`Database.MySQL` becomes the main module, with the API below.
`Database.MySQL.Base` stays in the package, deprecated, with its 1.x types,
built on the same code, so existing code compiles against 2.0 with
deprecation warnings. Nothing on Hackage exposes a `Database.MySQL` module,
while `Database.MySQL.Base` clashes with the C binding `mysql`.

### Abstract constructors

`Database.MySQL` exports `ConnectInfo`, `MySQLConn`, `ColumnDef`, `OK` and
`Greeting` with their field selectors but without their constructors. Record
update keeps working
(`defaultConnectInfo { ciUser = "app" }`); building the record with the
constructor and matching on it stop compiling (GHC 9.10 rejects them with
GHC-01928, while `import ... (ConnectInfo (..))` still compiles).
`MySQLValue` keeps its constructors, since matching on them is how a value
is read.

A field can then be added without a major version, but only for code that
uses `Database.MySQL`: as long as the deprecated `Database.MySQL.Base`
exports the constructors, a new field still breaks its users. So the gain
arrives when `Database.MySQL.Base` is removed in a later major version,
unless `Database.MySQL.Base` exports them abstractly too and its
constructor users (persistent-mysql-pure, euler-hs, see below) change in
2.0.

Issue #27 (2018) shows a gap to close first: a Selda backend wanted rows
without `MySQLValue` and the affected-rows count from one call that may or
may not return rows, and the answer was the exported `MySQLConn`
constructor with `readPacket` and `waitCommandReply`. The direct row
decoding of 1.4.0 (`queryRows`, `queryRawRows_`) covers the rows, but an OK
reply comes back as an empty stream and its count is dropped.
`Database.MySQL` needs a query that returns either the decoded rows or the
OK.

### io-streams out of the main API

Every query function that streams rows returns an io-streams `InputStream`
(all but `queryMulti` and `queryMulti_`, which return lists), and the
connection reads and writes through io-streams (`Data.Connection`,
`System.IO.Streams.TCP`, `System.IO.Streams.TLS`). io-streams is not
abandoned (1.5.2.2 was uploaded in January 2026); the case for moving off it
is that users have to learn and depend on it to read a result, and that a
row stream's state (unread rows block the connection, `Stream.skipToEof`)
lives in a library that knows nothing about it.

- `Database.MySQL` returns a small row stream of our own, `RowStream a`,
  with `nextRow :: RowStream a -> IO (Maybe a)`, `skipRemainingRows`, and
  folds (`foldRows`, `rowsToList`). It carries the reply state, so "read or
  skip the rows before the next command" is the type's own rule.
- Two library-agnostic forms on top of it, so conduit, pipes, streaming or
  io-streams users plug in without the package depending on any of them:
  - A fold in the shape foldl's `impurely` takes,
    `foldRowsM :: (x -> a -> IO x) -> IO x -> (x -> IO b) -> ... -> IO b`.
    foldl documents `purely` and `impurely` as the way to accept its folds
    "without incurring a foldl dependency", and io-streams' `fold_` and
    `foldM_` already have that shape
    ([Control.Foldl](https://hackage.haskell.org/package/foldl/docs/Control-Foldl.html),
    [System.IO.Streams.Combinators](https://hackage.haskell.org/package/io-streams-1.5.2.2/docs/System-IO-Streams-Combinators.html)).
    The fold runs inside the query function, so every row is read before it
    returns.
  - A polymorphic source, `rows :: (MonadIO m, MonadPlus m) => ... -> m a`,
    which any `ListT` with `MonadPlus` and `MonadIO` instances runs (pipes'
    `ListT`, logict, list-t), as Gabriella Gonzalez describes in
    [How to build library-agnostic streaming sources](https://haskellforall.com/2014/11/how-to-build-library-agnostic-streaming)
    (2014). A consumer can stop early, so it needs the same unread-rows
    guard as `RowStream`, and transformers' `ListT` does not stream (the
    post says so).
- `Database.MySQL.Base` keeps returning `InputStream`s, made from
  `RowStream`. io-streams stays a dependency for as long as that module
  exists, and leaves with it.
- Inside, the packet reader moves to `network`'s `recv` and tls's
  `recvData` over one buffer; `Data.Connection` and
  `System.IO.Streams.TCP`/`TLS` stay exposed (hidden from the Haddock) for
  whoever imports them.

This touches the code the 1.3 to 1.4 speed work tuned (`benchmark/`); it
merges only with benchmark numbers no worse than 1.6.0's.

### What a field on `MySQLConn` makes possible

These were postponed in 1.x because each needs state on the connection:

- Limits on what a server can make the client read, if users ask for them.
  #116 (closed) bounded messages inside the packet stream, which cost about
  40 instructions on every row and a new `ConnectInfo` field, against a
  hostile server only; with the limit on `MySQLConn`, `readPacket` can check
  it only when a message continues past one 16 MiB packet, so ordinary rows
  pay nothing.
- A cursor generation, so a stream from `queryStmtCursor` refuses to read
  the rows of a later execution of the same statement (#118 documents this
  as a sharp edge).
- The connection's character set, so `escapeText` and `escapeBytes` can
  refuse or correctly escape gbk, big5, sjis, cp932 and gb18030 text, whose
  multibyte characters can swallow the escaping backslash (the 1.5.0
  changelog documents the hole).

`MySQLConn`'s constructor is exported from `Database.MySQL.Connection`
today, so adding the field breaks code that builds or matches it; on
Hackage only mysql-haskell-openssl does, and it no longer builds against
1.x.

### Smaller changes

- Booleans become sum types in `Database.MySQL.BinLog`: the checksum and
  semi-sync flags of `decodeBinLogPacket` and `decodeBinLogPacketFollowing`,
  and the results of `isCheckSumEnabled` and `isSemiSyncEnabled`, which
  callers pass on to them.
- `Database.MySQL.Protocol.MySQLValue` (1081 lines) gets split.

## Who breaks

Measured on 13 November 2026 from Hackage's reverse dependencies, the
latest tarball of each, the Hackage index and GitHub code search (a lower
bound, as GitHub's index is incomplete). Commercial code is not in it.

| Package | Latest | Bound lets 2.0 in | Uses |
| --- | --- | --- | --- |
| hasqlator-mysql | 0.2.2, January 2026 | yes | `Database.MySQL.Base`, `putTextField`, `Streams.toList` on a result (`Hasqlator.hs:338`) |
| hasqly-mysql | 0.0.1, 2020 | yes | `Base` (hiding `Query`, `Command`), `putTextField` |
| mysql-haskell-nem | 0.1.0.0, 2016, in Stackage nightly | yes | `Base`, `ColumnDef(..)` in imports, `columnName`, `StmtID`, `InputStream` in its own API (`Nem.hs:24`, `:38`) |
| users-mysql-haskell | 0.5.2.0, 2016 | yes | `Base`, `System.IO.Streams.List.toList` on results (`Web/Users/MySQL.hs:321`); its tests already fail, they build a 5-field `ConnectInfo` |
| persistent-mysql-pure | 1.0.4, March 2026 | no (`< 2.0`) | `Base`, `Protocol.Escape`, `TLS`, a pattern match on `ConnectInfo`'s constructor (`Database/Persist/MySQL.hs:1289`), `putTextField`, `okAffectedRows`, `Streams.read` and `skipToEof` on results |
| persistent-mysql-haskell, strongswan-sql, mysql-haskell-openssl | 2019 to 2020 | no | bounds that already exclude current 1.x; mysql-haskell-openssl, the only Hackage user of `MySQLConn`'s constructor and the `Connection` plumbing, no longer builds against it |

On GitHub, outside copies of mysql-haskell:

- juspay/euler-hs and its active fork nammayatri/euler-hs build `ConnectInfo`
  with its constructor (`EulerHS/Core/Types/MySQL.hs:154`,
  `EulerHS/SqlDB/MySQL.hs:149`); kowainik/mysql-not-so-simple does in its
  tests. About 25 repositories only record-update `defaultConnectInfo`.
- ikaro1192/PureMyHA and an ICFP contest entry import `ERR` from
  `Database.MySQL.Protocol.Packet`; icfpc2024 and mysql-not-so-simple import
  `utf8mb4_unicode_ci` from `Database.MySQL.Connection`.
- No one outside mysql-haskell was found using `MySQLConn`'s internals,
  `Data.TLSSetting`, `System.IO.Streams.*` or `Data.Binary.Parser` from this
  package.

Steps 1 and 2 break nothing. With step 3 as written (the deprecated
`Database.MySQL.Base` keeping its 1.x types and constructors), the Hackage
packages keep compiling with deprecation warnings, and so does code that
builds `ConnectInfo`. What breaks is code that builds or matches
`MySQLConn`, once the connection gains its new fields.

## Order of work

1. 1.6.x, no break, each its own pull request: the Haddock `hide`
   attributes with `Database.MySQL.Base`'s explicit export list; dropping
   monad-loops; raising the tls floor and dropping data-default-class.
2. 2.0, when a fix needs the `MySQLConn` field or the io-streams move is
   worth it, one pull request per item: `Database.MySQL` with the abstract
   constructors and the deprecated `Database.MySQL.Base`; `RowStream` and
   the agnostic folds and sources, gated on the benchmarks; the fields on
   `MySQLConn` and the fixes they unlock.
3. A later major version removes `Database.MySQL.Base`, and io-streams with
   it.
