Decoding rows without `MySQLValue`
==================================

October 2026. Steps 1 and 2 of the plan below are built and measured. Step 3
belongs in persistent-mysql-haskell and waits on persistent's RFC. Step 4,
encoding, is not built; [the reason](#step-4-encoding-not-built) is at the end.

## Why

Every field of every row is decoded into a `MySQLValue` and consed onto a
list before the caller sees it. Callers that want their own types then
convert again: through `Database.MySQL.Field`, or in persistent-mysql-haskell
through `getGetter` into `PersistValue` and from there into the record. The
box and list cell cost about 5 words per field (a 2-word constructor and a
3-word cons cell), and decoding text rows was 47.5% of select CPU in
`benchmark/cpu-profile-1.3.3.md`.

persistent has an RFC for decoding straight into records,
[yesodweb/persistent#1617](https://github.com/yesodweb/persistent/issues/1617).
This work makes mysql-haskell the layer such a decoder can sit on.

## mysql-haskell needs no schema

Every result set starts with a `ColumnDef` per column: type, unsigned flag,
character set and decimals. That is all a decoder has to check, and it is
checked once per result set. Which Haskell type a column becomes is the
caller's choice, or that of a library above that knows the schema, such as
persistent's entity definitions.

## The plan

1. A strict row decoder behind the current API, measured against master.
2. Decoders as values (layer 1), for plain queries and prepared statements.
3. persistent-mysql-haskell on top of it (layer 2).
4. Encoding parameters without `MySQLValue`.

## Step 1: behind the existing API

`query_`, `queryVector_` and `queryMulti_` resolve each column to a
`ColumnKind` once, when the column definitions arrive, and decode every row
with `decodeTextRow`: one walk over the row packet as a strict `ByteString`
that calls the value parser directly and returns unboxed sums, so no `Either`
or tuple is allocated per field. `queryStmt` and `queryStmtVector` do the same
for the binary protocol with `decodeBinaryValues`. Before, both went through
binary's continuation-based `Get` over the lazy packet body, comparing the
column type against two dozen constants for every field of every row.

Values stay as lazy as before, so invalid UTF-8 still raises when a value is
used, and a malformed row still raises `DecodePacketFailed`. The two paths
differ only on rows a server does not send: an 8-byte length above
`maxBound :: Int` is rejected, and a zero-length BIT is 0 instead of the byte
after it. `getTextField`, `getTextRow`, `getBinaryField` and `getBinaryRow`
keep their `Get` interface.

## Step 2: decoders as values

`Database.MySQL.Decoder`, decoders as values in the style of hasql's, used
with the new query functions of `Database.MySQL.Base`:

```haskell
import qualified Database.MySQL.Decoder as Decode

data Employee = Employee !Int32 !Day !Text

employee :: Decode.RowDecoder Employee
employee = Employee
    <$> Decode.field Decode.int32
    <*> Decode.field Decode.day
    <*> Decode.field Decode.text

queryRows_    :: RowDecoder a -> MySQLConn -> Query -> IO ([ColumnDef], InputStream a)
queryRows     :: QueryParam p => RowDecoder a -> MySQLConn -> Query -> [p] -> IO ([ColumnDef], InputStream a)
queryStmtRows :: RowDecoder a -> MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream a)
```

The field decoders are `int8` to `word64`, `float`, `double`, `scientific`,
`text`, `bytes`, `day`, `localTime`, `timeOfDay`, `bool` and `mysqlValue`
(every column, as `query_` decodes it), plus `nullable`. `FieldDecoder` is a
`Functor` and `RowDecoder` an `Applicative` over the columns in order.

A decoder for a Haskell type accepts exactly the columns whose `MySQLValue`
constructor `Database.MySQL.Field` decodes into that type, so the two never
disagree about which column fits which type, and there is no implicit
widening. A result set a decoder does not fit raises `ColumnMismatch` (a
column's type, or the number of columns) before any row is read. A field that
does not decode raises `FieldError` with its column number and a
`FieldErrorKind`; invalid UTF-8 is `FieldInvalidUtf8` rather than an exception
from inside a value. Either way the remaining rows are skipped first, so the
connection stays usable. MySQL cannot stop a result set it is already sending
(short of `KILL QUERY` from a second connection), so on a large result set the
skipping costs the network read.

Values are evaluated as they are decoded, which typed errors require.

For libraries that decode by column number, such as a persistent backend:

```haskell
queryRawRows_    :: MySQLConn -> Query -> IO ([ColumnDef], InputStream (RawRow TextProtocol))
queryStmtRawRows :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream (RawRow BinaryProtocol))

prepareFieldParser :: RowProtocol protocol
                   => FieldDecoder a -> ColumnNumber -> ColumnDef -> Either ColumnMismatch (FieldParser protocol a)
runFieldParser :: FieldParser protocol a -> RawRow protocol -> ColumnNumber -> (FieldError -> r) -> (a -> r) -> r
```

A `RawRow` is the row's bytes plus each field's bounds, found in one pass
(`Database.MySQL.Protocol.RawRow`). Its `protocol` parameter keeps a text
parser off a binary row. `runFieldParser` passes the outcome to continuations
because the parser is a closure built at prepare time, which GHC cannot inline
across, so an `Either` result would be allocated per field.

### Where this departs from the plan

The plan had a `RowDecoder` read its fields through a `RawRow`, and the
existing API rebuilt as `RowDecoder [MySQLValue]`. Measurements changed both:

- A `RowDecoder` walks the row once, in column order, without storing field
  bounds. More of its cost came from elsewhere: on the employees table the
  typed path took 9,518 instructions per row through a `RawRow`, 9,327 with
  the in-order walk, 8,692 once the `Functor` and `Applicative` methods and
  the step combinators were inlined, and 8,065 once the shared lexers were
  specialised. Before inlining, every field paid a generic partial
  application of the record's constructor (`stg_PAP_apply`, `stg_ap_pp`:
  about 1,400 instructions per row). With it, a decoder written as one
  expression compiles to one walk that applies the constructor to all its
  fields. A decoder assembled at run time, say with `traverse` over a list,
  works the same but pays a closure call per field. Before specialisation,
  `Double` and `Scientific` fields went through bytestring-lexing's
  dictionary-passing digit loops.
- The existing API is not built on `RowDecoder`. `decodeTextRow` and
  `decodeBinaryValues` are plain loops that call the value parser directly;
  a `RowDecoder [MySQLValue]` would be assembled at run time from the column
  count and pay the per-field closure call above.
- A `FieldError` skips the remaining rows too, not just a `ColumnMismatch`:
  the caller only holds the decoded stream, so draining it would hit the next
  bad row again.

## Results

Measured on 8 October 2026 with the setup of `benchmark/cpu-profile-1.3.3.md`:
MySQL 8.0.45 on the same machine, GHC 9.10.3, `+RTS -N4`, about 300,000 rows
per thread. Instructions are user space, per row, at 1 thread; each figure is
the median of three rounds of five runs, the variants interleaved.
libmysqlclient, which only fetches rows, reads employees in 86 ms at 1 thread
and 189 ms at 10 threads, so the 1-thread times are mostly the server sending
rows; instructions and the 10-thread times show the client.

Two tables: `employees` (INT, two DATEs, three short strings) and
`mixed_types` (`benchmark/select/mixed_types.sql`: INT, DECIMAL,
DATETIME(6), DOUBLE and about 500 bytes of non-ASCII TEXT).

### Rows into a record

`benchmark/select/MySQLHaskellRecords.hs` decodes every row into a record
with strict fields, through `MySQLValue` and `Database.MySQL.Field` (what an
application does today) or through a `RowDecoder`:

| table | path | instructions/row | 1 thread | 10 threads | bytes allocated/row |
|-------|------|------------------|----------|------------|---------------------|
| employees | `query_` + Field, master | 9,022 | 175 ms | 674 ms | 5,618 |
| employees | `query_` + Field | 7,663 | 143 ms | 562 ms | 3,384 |
| employees | `queryRows_` | 8,079 | 153 ms | 551 ms | 2,886 |
| employees | `queryStmt` + Field, master | 7,969 | 164 ms | 615 ms | 4,777 |
| employees | `queryStmt` + Field | 7,188 | 143 ms | 519 ms | 2,866 |
| employees | `queryStmtRows` | 7,338 | 131 ms | 492 ms | 2,263 |
| mixed_types | `query_` + Field, master | 16,881 | 636 ms | 1,879 ms | 8,388 |
| mixed_types | `query_` + Field | 15,966 | 643 ms | 1,837 ms | 6,566 |
| mixed_types | `queryRows_` | 16,130 | 631 ms | 1,745 ms | 6,077 |
| mixed_types | `queryStmt` + Field, master | 11,914 | 548 ms | 1,436 ms | 6,601 |
| mixed_types | `queryStmt` + Field | 9,290 | 528 ms | 1,260 ms | 4,447 |
| mixed_types | `queryStmtRows` | 9,280 | 548 ms | 1,258 ms | 3,864 |

Most of the gain is behind the existing API: an application decoding into
its own types through `query_` runs 15% fewer instructions on employees and
5% fewer on mixed_types, and through `queryStmt` 10% and 22% fewer, allocating
22 to 40% less.

The typed decoders run about as many instructions as the faster
`MySQLValue` path (5% more for plain queries on employees, at most about 2%
apart elsewhere) and allocate 7 to 21% less. The box they remove is small, as
estimated before measuring; what they add is typed errors, no exception from
invalid UTF-8, a column check before the first row, and the `RawRow` access a
persistent backend needs. Most of the 5% is likely `Data.Text.decodeUtf8'`,
which reports invalid UTF-8 by catching an exception per field, three times
per employees row; validating with `isValidUtf8` first measured worse, as
long fields are then scanned twice.

What remains costly, whichever API: building each `Day` (`fromGregorian` does
`Integer` arithmetic, about a quarter of the samples on employees) and UTF-8
decoding. The `Day` cost is finding 5 of the profile report.

### Values unused and used, `query_` only

The README's select benchmark (`benchmark/select/MySQLHaskell.hs`, `bench`)
only counts rows and never looks at a value. On master the values stay
thunks, so it never decodes UTF-8 or builds a `Day`.
`benchmark/select/MySQLHaskellValues.hs` (`bench-values`) evaluates every
value. Step 1 was measured with two versions of the decoder: one evaluating
each value while decoding (eager) and one leaving values as lazy as master
does.

| table | values | decoder | instructions/row | 1 thread | 10 threads | bytes allocated/row |
|-------|--------|---------|------------------|----------|------------|---------------------|
| employees | unused | master | 4,121 | 90 ms | 340 ms | 4,175 |
| employees | unused | eager | 6,876 | 134 ms | 565 ms | 2,848 |
| employees | unused | lazy | 2,497 | 91 ms | 265 ms | 1,711 |
| employees | used | master | 8,845 | 179 ms | 722 ms | 5,433 |
| employees | used | eager | 7,005 | 143 ms | 538 ms | 2,844 |
| employees | used | lazy | 7,239 | 142 ms | 551 ms | 3,062 |
| mixed_types | unused | master | 14,927 | 627 ms | 1,750 ms | 7,417 |
| mixed_types | unused | lazy | 13,684 | 616 ms | 1,694 ms | 5,334 |
| mixed_types | used | master | 16,633 | 613 ms | 1,954 ms | 8,165 |
| mixed_types | used | lazy | 15,471 | 624 ms | 1,785 ms | 6,151 |

The existing API keeps the lazy decoder: evaluating every value while
decoding saved 3% of instructions when every value is used but took 2.75
times the instructions when none is, and it would move invalid UTF-8 errors
from use to read.

## Step 3: persistent-mysql-haskell

persistent-mysql-haskell would implement the RFC's classes on top of step 2:

| RFC (persistent#1617) | mysql-haskell |
|---|---|
| `Env backend`, one per row | a `RawRow` plus the column definitions |
| `prepareField env name col` | `prepareFieldParser` on that column's `ColumnDef` |
| `FieldRunner` / `runField` | `runFieldParser` on the `RawRow` |
| `directQuerySource` | `queryRawRows_` or `queryStmtRawRows` |

```haskell
data MySQLRowEnv = MySQLRowEnv
    { envColumns :: !(Vector ColumnDef)
    , envRow     :: !(RawRow TextProtocol)
    }

instance FieldDecode MySQLRowEnv Int64 where
    prepareField env _name col onErr onOk =
        case prepareFieldParser int64 col (columnAt env col) of
            Left mismatch -> onErr (renderColumnMismatch mismatch)
            Right parser  -> onOk $ FieldRunner $ \rowEnv onErr' onOk' ->
                runFieldParser parser (envRow rowEnv) col (onErr' . renderFieldError) onOk'
```

That drops both `MySQLValue` and `PersistValue`; only the record remains.
What the RFC needs from mysql-haskell is there: access by column number,
continuation-style running, and column definitions before the first row, so a
mismatch shows even on an empty result. Three points are on persistent's side:

- The release action of `directQuerySource`'s `Acquire` has to call
  `skipToEof` on the raw rows when the consumer stops early, or the connection
  is left out of step.
- The RFC's error continuation takes `Text`, so the typed errors are rendered
  there.
- Code that only knows the bare `SqlBackend` cannot reach the direct path,
  which is the RFC's open problem for every backend.

It waits on the RFC landing in persistent. persistent-mysql-haskell 0.6.0
still requires `mysql-haskell <1.0`, so it needs updating regardless.

For people using mysql-haskell directly, `Database.MySQL.Field` could gain a
decoder method whose default goes through `MySQLValue`, which keeps every
existing instance working. That waits until the RFC's shape settles.

## Step 4: encoding, not built

Parameters of plain queries do not need it: `query`, `execute` and
`queryRows` take any `QueryParam`, and `Param`'s `render` writes a value into
the query text without a `MySQLValue`. Prepared statements take
`[MySQLValue]`, but a statement has a handful of parameters, about 5 words of
box each, against about 29 µs per INSERT that is mostly kernel time (finding
3 of the profile report). The saving would not show in a benchmark, so it is
not worth a second API. Revisit when persistent's encode side
(`FieldEncode param a`) needs a binary parameter type for prepared
statements.

## Reproducing

The programs are in `benchmark/select`: `bench` (values unused),
`bench-values` (every value evaluated) and `bench-records` (into records,
`field`, `typed`, `field-stmt` or `typed-stmt` mode). Each takes a thread
count and a table. They were compiled with `ghc -O2 -threaded -rtsopts`
against the library built from `nix/hpkgs.nix`, and measured with
`perf stat -e instructions:u -r 5` and `+RTS -s`. The `.cabal` file in that
directory predates the current library and does not build.
