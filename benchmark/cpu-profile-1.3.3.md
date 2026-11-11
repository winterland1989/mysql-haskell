Where mysql-haskell 1.3.3 spends its time
-----------------------------------------

Measured on 7 October 2026 to find out what is worth optimising. Nothing
below has been implemented yet: each finding is a hypothesis with the
measurement behind it and a way to check a fix.

## Setup

* mysql-haskell 1.3.3 built by GHC 9.10.3 with `-O2 -threaded`, run with
  `+RTS -N4` unless a table says otherwise.
* MySQL 8.0.45 on the same machine, data directory in `/dev/shm`, binary log
  off. The C baseline links libmysqlclient (MariaDB Connector/C 3.3.5).
* AMD Ryzen AI 7 350: four cores up to 5.1 GHz and four up to 3.5 GHz, two
  hardware threads per core, 1 MB L2 per core, 16 MB L3.

The select benchmark is `select/MySQLHaskell.hs` against `select/libmysql.cpp`:
every thread reads all 300,024 rows of `employees` over the text protocol.

The insert measurements use one connection that runs `BEGIN`, N copies of the
INSERT from `insert/MySQLHaskell.hs`, then `COMMIT`. The C side is
`insert/libmysql.cpp` with its loop count changed to match. Per-statement cost
is the slope between N = 1,000 and N = 11,000 (median of nine interleaved runs
each); the intercept is the fixed cost per process.

Tools:

* `perf record -e task-clock:u`. With `perf_event_paranoid` at 2 this samples
  user space only. GHC's threads are named `ghc_worker`, so filter with
  `--comm ghc_worker,SelectHaskell`. Local symbols are stripped, so an
  unresolved address is mapped to the nearest preceding symbol in `nm -n`
  (the executable loads at 0x400000).
* `perf stat -e cycles:u,instructions:u,cache-misses:u`.
* `+RTS -s` for allocation and GC.
* `strace -f -c` at N = 1,000 and N = 2,000 inserts; the difference gives the
  syscalls per statement.

The overall gap on the select benchmark: Haskell executes about 4,070 user-space
instructions per row, the C client about 990 (12.2 G against 3.0 G
instructions for ten threads of 300,024 rows).

## Findings, largest expected gain first

### 1. Decoding text rows

`getTextField` in `Database.MySQL.Protocol.MySQLValue` takes 47.5% of select
CPU. For every field of every row it compares the column type against a chain
of `mySQLType…` constants to pick a parser, then runs that parser through
binary's continuation-passing `Get`. The cost of that style is visible in the
profile: calls to unknown functions (`stg_ap_pp`) are about 10% and thunk
updates about 3%.

Hypothesis: choosing one decoder per column once, when the column definitions
arrive, and parsing each length-encoded field straight from a strict
`ByteString` removes the per-field dispatch and most of the closures. The
47.5% is the ceiling. Check with instructions per row (`perf stat`) and bytes
allocated per row (`+RTS -s`, now about 4.2 KB).

### 2. Splitting the stream into packets

About 12% of select CPU. For every packet `decodeInputStream` in
`Database.MySQL.Connection` does `readExactly 4` for the header, then a
`Stream.read`/`splitAt`/`unRead` loop for the body, then
`L.fromChunks (reverse acc)`, and `getFromPacket` parses the resulting lazy
`ByteString`. An `employees` row is about 50 bytes, so this fixed overhead is
paid 300,024 times per thread.

Hypothesis: nearly every packet lies inside the current read chunk (16 KB,
`bUFSIZE`), so it can be sliced out as a strict `ByteString`, keeping the lazy
path only for a packet that crosses a chunk boundary.

### 3. Kernel time per insert

The C client spends 21.7 µs per INSERT, mysql-haskell 29.0 µs. Most of the
difference is kernel time. For 11,000 inserts the C process uses 4 to 10 ms
of user time and 22 to 30 ms of system time; mysql-haskell uses 27 to 41 ms
of user time and 58 to 69 ms of system time.

Syscalls per INSERT:

| syscall      | mysql-haskell | C |
|--------------|---------------|---|
| `sendto`     | 1             | 1 |
| `recvfrom`   | 2 (1 fails with EAGAIN) | 1 |
| `epoll_ctl`  | 1             | 0 |
| `epoll_wait` | 0.38          | 0 |
| total        | 4.4           | 2 |

The socket is non-blocking, so the first read after sending a statement finds
nothing yet, the thread registers the socket with the IO manager (a one-shot
`epoll_ctl`), waits, and reads again.

Hypothesis: a blocking `recv` through a safe or interruptible FFI call
removes the extra calls. What that does to timeouts and async exceptions has
to be tested before it can replace the current read.

### 4. A fresh receive buffer per read

`network`'s `recv` allocates a new pinned buffer of the requested size
(16 KB here) on every call and copies into a right-sized `ByteString` when
fewer bytes arrive. A test program on a socket pair measured 16,985 bytes
allocated per 11-byte send and read. An INSERT allocates about 32 KB in total
(`+RTS -s` at two insert counts), so the receive buffer is about half of it;
where the other ~15.5 KB comes from is not known yet.

Hypothesis: reading into a buffer the connection keeps, then copying out only
the bytes that arrived, removes the 16 KB allocation. The copy has to stay
because callers may hold on to the chunk.

### 5. Lexing dates and integers

6.4% of select CPU. The text-protocol date parser calls `readDecimal` from
bytestring-lexing three times and then `fromGregorian`, and every `employees`
row has two DATE columns. A text-protocol date is always `YYYY-MM-DD`, so
digit arithmetic at fixed offsets is enough.

### 6. UTF-8 decoding

`T.decodeUtf8` runs once per text field. It is part of the 47.5% of finding 1
and was not measured on its own.

## Ruled out

RTS flags do not close the gap. Ten select threads at `-N4` take 297 to
345 ms across sessions against 194 ms for C, and no setting got closer:

| flags          | 1 thread | 10 threads |
|----------------|----------|------------|
| `-N4`          | 93 ms    | 345 ms     |
| `-N4 -A1m`     | 100 ms   | 409 ms     |
| `-N4 -A256k`   | 147 ms   | 565 ms     |
| `-N10 -A256k`  | 205 ms   | 515 ms     |
| `-N10 -A16M`   |          | 495 ms     |

GC is not the bottleneck: it takes 26 to 90 ms of these runs. More
capabilities than free cores make things worse:

| flags        | wall   | mutator CPU | GC CPU  |
|--------------|--------|-------------|---------|
| `-N4`        | 297 ms | 1.01 s      | 0.026 s |
| `-N8`        | 313 ms | 1.75 s      | 0.040 s |
| `-N10`       | 381 ms | 2.96 s      | 0.045 s |
| `-N10 -qg`   | 356 ms | 2.69 s      | 0.002 s |

At `-N10` the program executes the same 12.2 G instructions as at `-N4` but
needs 9.8 G cycles instead of 3.6 G (instructions per cycle drop from 3.4 to
1.2), while cache misses rise only from 13.9 M to 20.6 M. The extra profile
samples land on the same decoding code, not in the scheduler or GC spin loops.
That points at threads sharing cores (ten client threads plus the server's own
threads on eight cores) rather than at allocation or GC. Only fewer
instructions per row (findings 1, 2 and 5) helps here.

Process startup is not part of the insert gap either. Starting and exiting
takes 2.25 ms for the Haskell client and 2.45 ms for the C client
(`perf stat -r 40`). Half of the Haskell startup CPU is glibc loading its
character-set conversion tables, not the dynamic linker resolving symbols.
The fixed cost per insert run is 9.5 ms for Haskell and 7.8 ms for C.

## Which fixes need an API change

A cost can be removed in three places: inside the library behind the current
API, by a JIT without touching the code, or by a new typed API that decodes
straight into the caller's record (a `FromRow`-style class, checked against the
column definitions once per result set). Only one cost needs the new API.

| cost | inside the library | by a JIT | needs a typed API |
|------|--------------------|----------|-------------------|
| type check per field (finding 1) | yes, one decoder per column chosen when the column definitions arrive | partly, see below | no |
| unknown calls through `Get` (finding 1, ~10%) | yes, a strict hand-written row decoder | mostly, through call-site caches and inlining | no |
| a thunk per field (finding 1, ~3%) | yes, evaluate each value while decoding | yes, with runtime speculation | no |
| a `MySQLValue` box and list cell per field | no | only when the values don't escape | yes |
| packet framing (finding 2) | yes | partly | no |
| syscalls per insert (finding 3) | yes | no | no |
| receive buffer (finding 4) | yes | no | no |
| lexing (finding 5) and UTF-8 (finding 6) | faster parsers, but the work stays | no | no |

The column type changes from field to field within a row, so a JIT sees five
or six targets at the one place that dispatches on it. To remove the dispatch
it would have to treat the column list as a constant and unroll the row loop
for every query shape. The library gets the same effect once per result set,
for the price of one indirect call per field.

GHC's demand analysis removes laziness only where it can prove a value will be
used, and the use of a decoded field happens in the caller's code, outside the
library. A JIT can observe at run time that every field gets forced and
evaluate it eagerly, with a way back to the thunk for a value whose evaluation
would fail or run long; optimistic evaluation (Ennals and Peyton Jones, ICFP 2003) did this inside GHC. THC does
not do this: it uses GHC's static demand signatures, opt-in through
`-Dthc.callDemands=true` (`docs/demand-probe.md` in ekmett/thc).

Graal's partial escape analysis removes allocations that do not escape the
compiled code. That covers a benchmark that throws rows away, but not an
application that keeps rows or passes them through io-streams.

How much the typed API alone would save is an estimate from the data layout,
not a measurement. Each field costs a `MySQLValue` constructor (2 words; small
strict fields are unpacked, the rest are one pointer) and a list cell
(3 words), so 40 bytes per field and 240 bytes per `employees` row, about 6% of
the 4.2 KB allocated per row today. A typed decoder still builds the caller's
record, so its saving is smaller than that, plus whatever the caller's own
conversion out of `MySQLValue` costs, which this benchmark does not include.

### THC

THC (ekmett/thc, Haskell on Truffle and Graal) is the JIT within reach, and it
is not an option yet. On the 1BR challenge it needs 108 to 115 s for a billion
rows where native GHC needs 1.32 s
([jappeace/1br `thc/PERFORMANCE.md`](https://github.com/jappeace/1br/blob/HEAD/thc/PERFORMANCE.md));
reading `Addr#` words, which is what parsing a `ByteString` does, is the main
cost there. THC requires GHC 9.14.1, which ships base 4.22.0.0, while this
package allows `base <4.22`. The C parts of crypton and network have not been
tried under it.

## Suggested order

1. Findings 1 and 2 inside the library, thunks included, measured by
   instructions per row. They need no API change.
2. Then measure what the `MySQLValue` boxes and list cells cost in what
   remains. Only that decides whether a typed API is worth adding next to the
   current one.
3. Findings 3 and 4 are each small and independent of the rest.
4. Revisit a JIT once THC parses a `ByteString` within a small factor of
   native GHC.
