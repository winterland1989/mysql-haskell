Decoding rows without `MySQLValue`
==================================

Plan, October 2026. Only step 1 below is being built; everything after it
waits for step 1's measurements.

## Why

Every field of every row is decoded into a `MySQLValue` and consed onto a
list before the caller sees it. Callers that want their own types then
convert again: through `Database.MySQL.Field`, or in persistent-mysql-haskell
through `getGetter` into `PersistValue` and from there into the record. The
box and list cell cost about 5 words per field (a 2-word constructor and a
3-word cons cell), and decoding text rows is 47.5% of select CPU in
`benchmark/cpu-profile-1.3.3.md`.

persistent has an RFC for decoding straight into records,
[yesodweb/persistent#1617](https://github.com/yesodweb/persistent/issues/1617).
This plan makes mysql-haskell the layer such a decoder can sit on.

## mysql-haskell needs no schema

Every result set starts with a `ColumnDef` per column: type, unsigned flag,
character set and decimals. That is all a decoder has to check, and it is
checked once per result set. Which Haskell type a column becomes is the
caller's choice, or that of a library above that knows the schema, such as
persistent's entity definitions.

## Layer 1: decoders as values

No type classes, in the style of hasql's decoders, split into a step that runs
once per result set and a step that runs per row:

```haskell
-- Checks the column definition once per result set, then parses the raw
-- field bytes of each row. Works for text and prepared statements.
data FieldDecoder a
int32, int64, word64, double :: FieldDecoder ...
text :: FieldDecoder Text
bytes :: FieldDecoder ByteString
day :: FieldDecoder Day
localTime :: FieldDecoder LocalTime
nullable :: FieldDecoder a -> FieldDecoder (Maybe a)
mysqlValue :: FieldDecoder MySQLValue   -- today's behaviour, as one decoder

prepareFieldParser :: FieldDecoder a -> ColumnDef -> Either ColumnMismatch (FieldParser a)
runFieldParser :: FieldParser a -> FieldBytes -> (FieldError -> r) -> (a -> r) -> r

-- Applicative over the columns, in order.
data RowDecoder a
field :: FieldDecoder a -> RowDecoder a

queryRows :: RowDecoder a -> MySQLConn -> Query -> IO ([ColumnDef], InputStream a)
queryStmtRows :: RowDecoder a -> MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream a)
```

A decoder for a Haskell type accepts exactly the columns whose `MySQLValue`
constructor `Database.MySQL.Field` accepts for that type, so the two layers
never disagree about which column fits which type. There is no implicit
widening.

Rows are walked as strict `ByteString`s. Text-protocol fields are
length-prefixed, so in-order access needs no extra work. Access by column
number, which persistent's `FieldDecode` asks for, needs the field offsets:
a `RawRow` records them in one pass, one small unboxed array per row.

`runFieldParser` takes continuations (or returns an unboxed sum) because the
parser is a closure built at prepare time. GHC cannot inline across it, so an
`Either` result would be allocated for every field.

Errors are typed. A column that does not fit raises `ColumnMismatch` with the
column index, name and server type. By then the rows are already queued on the
socket, so the remaining row packets are skipped first, as `ExtraResultSets`
does; otherwise the connection is out of step. MySQL cannot stop a result set
it is already sending (short of `KILL QUERY` from a second connection), so on
a large result set the skipping costs the network read. A field that does not
parse raises an error with the column index and byte offset.

The existing API becomes `RowDecoder [MySQLValue]` built from `mysqlValue` per
column, so there is one decoder implementation and `query_` keeps its type.

## Layer 2: the type glue

persistent-mysql-haskell would implement the RFC's classes on top of layer 1:

| RFC (persistent#1617) | mysql-haskell layer 1 |
|---|---|
| `Env backend`, one per row | the row's bytes (`RawRow`) plus the column definitions |
| `prepareField env name col` | `prepareFieldParser` on that column's `ColumnDef` |
| `FieldRunner` / `runField` | `runFieldParser` on that column's bytes |
| `directQuerySource` | a query returning the column definitions and a stream of raw rows |

```haskell
data MySQLRowEnv = MySQLRowEnv
    { envColumns :: !(Vector ColumnDef)
    , envRow     :: !RawRow
    }

instance FieldDecode MySQLRowEnv Int64 where
    prepareField env _name col onErr onOk =
        case prepareFieldParser int64 (columnAt env col) of
            Left mismatch -> onErr (renderColumnMismatch mismatch)
            Right parser  -> onOk $ FieldRunner $ \rowEnv onErr' onOk' ->
                runFieldParser parser (rawField (envRow rowEnv) col)
                    (onErr' . renderFieldError) onOk'
```

That drops both `MySQLValue` and `PersistValue`; only the record remains. Two
points stay on persistent's side: the RFC's error continuation takes `Text`, so
the typed errors are rendered there, and code that only knows the bare
`SqlBackend` cannot reach the direct path (the RFC's open problem for every
backend). The release action of `directQuerySource`'s `Acquire` has to skip
unread rows when the consumer stops early. persistent-mysql-haskell 0.6.0
still requires `mysql-haskell <1.0`, so it needs updating regardless.

For people using mysql-haskell directly, `Database.MySQL.Field` could gain a
decoder method whose default goes through `MySQLValue`, which keeps every
existing instance working. That waits until the RFC's shape settles.

## Encoding

Parameters have the same box: `[MySQLValue]` is rendered into the query text
or the binary execute packet. A `FieldEncoder a` writing directly is the
counterpart of the RFC's `FieldEncode param a`. Phase 2.

## Order

1. A strict text-row decoder behind the current API: the column type is
   looked up once per result set, rows are walked as a strict `ByteString`,
   values are evaluated as they are decoded. Measured against master with the
   select benchmark: instructions per row (`perf stat`), bytes allocated per
   row (`+RTS -s`) and wall time at 1 and 10 threads.
2. Expose layer 1, prepared statements included.
3. persistent-mysql-haskell on top of it.
4. Encoding.

Steps 2 to 4 wait for the numbers from step 1.
