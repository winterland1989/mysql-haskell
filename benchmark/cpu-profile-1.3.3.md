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

## Suggested order

Findings 1 and 2 together, since they share the decoding path, measured by
instructions per row. Findings 3 and 4 are each small and independent of the
rest.
