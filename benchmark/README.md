mysql-haskell benchmark
-----------------------

We mainly want to benchmark `mysql-haskell` against pure c `libmysql`, Haskell's FFI version `mysql` are not buildable from hackage, so here we include it, install it using `cabal install mysql-0.1.1.8.tar.gz`.

For where the time goes inside mysql-haskell 1.3.3 and what might speed it up, see [cpu-profile-1.3.3.md](cpu-profile-1.3.3.md).
