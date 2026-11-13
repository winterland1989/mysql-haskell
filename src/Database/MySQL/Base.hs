{-|
Module      : Database.MySQL.Base
Description : Prelude of mysql-haskell
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide common MySQL operations,

NOTEs on 'Exception's: This package use 'Exception' to deal with unexpected situations,
but you shouldn't try to catch them if you don't have a recovery plan,
for example: there's no meaning to catch a 'ERRException' during authentication unless you want to try different passwords.
By using this library you will meet:

    * 'NetworkException':  underline network is broken.
    * 'UnconsumedResultSet':  you should consume previous resultset before sending new command.
    * 'AbandonedReply': reading an earlier reply stopped part-way, e.g. on a timeout;
      connect again.
    * 'ERRException':  you receive a 'ERR' packet when you shouldn't.
    * 'UnexpectedPacket':  you receive a unexpected packet when you shouldn't.
    * 'DecodePacketException': there's a packet we can't decode.
    * 'WrongParamsCount': you're giving wrong number of params to 'renderParams'.
    * 'ExtraResultSets': a statement produced a result-set the function running it
      could not return.
    * t'Database.MySQL.Connection.MessageTooLarge': the server sent a message larger
      than the connection's 'Database.MySQL.Connection.messageSizeLimit'; connect again.
    * t'Database.MySQL.Connection.MoreResultsThanStatements': the server sent more OK
      packets than the query has statements; connect again.

Both 'UnexpectedPacket' and 'DecodePacketException' may indicate a bug of this library rather your code, so please report!

-}
module Database.MySQL.Base
    ( -- * Setting up and control connection
      MySQLConn
    , ConnectInfo(..)
    , defaultConnectInfo
    , defaultConnectInfoMB4
    , connect
    , connectDetail
    , connectUnixSocket
    , connectUnixSocketDetail
    , close
    , ping
    , resetConnection
      -- * Direct query
    , execute
    , executeMany
    , executeMany_
    , execute_
    , query_
    , queryVector_
    , query
    , queryVector
    , queryMulti_
    , queryMulti
    , StatementResult(..)
      -- * Prepared query statement
    , prepareStmt
    , prepareStmtDetail
    , executeStmt
    , queryStmt
    , queryStmtVector
    , closeStmt
    , resetStmt
      -- * Decoding rows without 'MySQLValue'
      -- | Rows decoded straight into Haskell values with the decoders of
      -- "Database.MySQL.Decoder", or handed over raw for libraries that decode
      -- by column number.
    , queryRows_
    , queryRows
    , queryStmtRows
    , queryRawRows_
    , queryStmtRawRows
      -- * Helpers
    , withTransaction
    , QueryParam(..)
    , Param (..)
    , Query(..)
    , renderParams
    , command
    , Stream.skipToEof
      -- * Exceptions
    , NetworkException(..)
    , UnconsumedResultSet(..)
    , AbandonedReply(..)
    , ERRException(..)
    , UnexpectedPacket(..)
    , DecodePacketException(..)
    , WrongParamsCount(..)
    , ExtraResultSets(..)
      -- * MySQL protocol
    , module  Database.MySQL.Protocol.Auth
    , module  Database.MySQL.Protocol.Command
    , module  Database.MySQL.Protocol.ColumnDef
    , module  Database.MySQL.Protocol.Packet
    , module  Database.MySQL.Protocol.MySQLValue
    , module  Database.MySQL.Protocol.RawRow
    ) where

import           Control.Exception                  (SomeException, catch, fromException,
                                                     mask, onException, throwIO)
import           Control.Monad
import           Data.Binary.Get                    (Get)
import           Data.Bits                          ((.&.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString.Lazy.Char8         as LC
import           Data.Int                           (Int64)
import           Data.IORef                         (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                         (isJust)
import           Data.Proxy                         (Proxy (..))
import           Database.MySQL.Connection
import qualified Database.MySQL.Decoder             as Decoder
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Protocol.RawRow

import           Database.MySQL.Query
import           System.IO.Streams                  (InputStream)
import qualified System.IO.Streams                  as Stream
import qualified Data.Vector                        as V
import qualified Unwitch.Convert.Int                as Int
import qualified Unwitch.Convert.Word64             as Word64

--------------------------------------------------------------------------------

-- | Execute a MySQL query with parameters which don't return a result-set.
--
-- The query may contain placeholders @?@, for filling up parameters, the parameters
-- will be escaped before get filled into the query. Under @NO_BACKSLASH_ESCAPES@
-- a backslash in a parameter is stored twice, and on a gbk, big5, sjis, cp932 or
-- gb18030 connection the escaping is not safe at all, see
-- "Database.MySQL.Protocol.Escape". You should consider using prepared statement
-- if this's not an one shot query, and must on those character sets.
--
execute :: QueryParam p => MySQLConn -> Query -> [p] -> IO OK
execute conn qry params = execute_ conn (renderParams qry params)

{-# SPECIALIZE execute :: MySQLConn -> Query -> [MySQLValue] -> IO OK #-}
{-# SPECIALIZE execute :: MySQLConn -> Query -> [Param]      -> IO OK #-}

-- | Execute a multi-row query which don't return result-set.
--
-- Leverage MySQL's multi-statement support to do batch insert\/update\/delete,
-- you may want to use 'withTransaction' to make sure it's atomic, and
-- use @sum . map okAffectedRows@ to get all affected rows count.
--
-- The result holds one 'OK' per statement, so a query with several statements
-- gives several per parameter set. A statement that returns rows raises
-- 'ExtraResultSets' once the whole reply has been read; 'queryMulti' returns
-- those. More OKs than the statements sent raise
-- t'Database.MySQL.Connection.MoreResultsThanStatements'.
-- No parameter sets gives @[]@ without contacting the server.
--
-- @since 0.2.0.0
--
executeMany :: QueryParam p => MySQLConn -> Query -> [[p]] -> IO [OK]
executeMany conn@(MySQLConn is os _ _) qry paramsList =
    case paramsList of
        -- Joining no statements would send an empty query, which the server refuses.
        [] -> guardUnconsumed conn >> pure []
        _ : _ -> runCommand conn $ do
            let qry' = L.intercalate ";" $ map (fromQuery . renderParams qry) paramsList
            writeCommand (COM_QUERY qry') os
            readStatementOKs (statementBound qry') 0 is

{-# SPECIALIZE executeMany :: MySQLConn -> Query -> [[MySQLValue]] -> IO [OK] #-}
{-# SPECIALIZE executeMany :: MySQLConn -> Query -> [[Param]]      -> IO [OK] #-}

-- | Execute multiple querys (without param) which don't return result-set.
--
-- This's useful when your want to execute multiple SQLs without params, e.g. from a
-- SQL dump, or a table migration plan.
--
-- A statement that returns rows raises 'ExtraResultSets' once the whole reply
-- has been read; 'queryMulti_' returns those. More OKs than the query has
-- statements raise t'Database.MySQL.Connection.MoreResultsThanStatements'.
--
-- @since 0.8.4.0
--
executeMany_ :: MySQLConn -> Query -> IO [OK]
executeMany_ conn@(MySQLConn is os _ _) (Query qry) = runCommand conn $ do
    writeCommand (COM_QUERY qry) os
    readStatementOKs (statementBound qry) 0 is

-- | Execute a MySQL query which don't return a result-set.
--
-- For a multi-statement query this is the first statement's 'OK'; the later
-- ones are read and discarded ('executeMany_' returns them all).
--
execute_ :: MySQLConn -> Query -> IO OK
execute_ conn (Query qry) = executeCommand conn (COM_QUERY qry)

-- | Execute a MySQL query which return a result-set with parameters.
--
-- Note that you must fully consumed the result-set before start a new query on
-- the same 'MySQLConn', or an 'UnconsumedResultSet' will be thrown.
-- if you want to skip the result-set, use 'Stream.skipToEof'.
--
query :: QueryParam p => MySQLConn -> Query -> [p] -> IO ([ColumnDef], InputStream [MySQLValue])
query conn qry params = query_ conn (renderParams qry params)

{-# SPECIALIZE query :: MySQLConn -> Query -> [MySQLValue] -> IO ([ColumnDef], InputStream [MySQLValue]) #-}
{-# SPECIALIZE query :: MySQLConn -> Query -> [Param]      -> IO ([ColumnDef], InputStream [MySQLValue]) #-}

-- | 'V.Vector' version of 'query'.
--
-- @since 0.5.1.0
--
queryVector :: QueryParam p => MySQLConn -> Query -> [p] -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryVector conn qry params = queryVector_ conn (renderParams qry params)

{-# SPECIALIZE queryVector :: MySQLConn -> Query -> [MySQLValue] -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue)) #-}
{-# SPECIALIZE queryVector :: MySQLConn -> Query -> [Param]      -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue)) #-}

-- | Execute a MySQL query which return a result-set.
--
-- A statement without a result-set, such as an INSERT, gives no columns and no
-- rows; use 'execute_' to get its 'OK'.
--
query_ :: MySQLConn -> Query -> IO ([ColumnDef], InputStream [MySQLValue])
query_ conn@(MySQLConn is os _ state) (Query qry) = runCommand conn $ do
    writeCommand (COM_QUERY qry) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state (decodeTextRowPacket (map columnKind fields))
            return (fields, rows)

-- | 'V.Vector' version of 'query_'.
--
-- @since 0.5.1.0
--
queryVector_ :: MySQLConn -> Query -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryVector_ conn@(MySQLConn is os _ state) (Query qry) = runCommand conn $ do
    writeCommand (COM_QUERY qry) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) V.empty <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- readColumnDefVector len is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state (decodeTextRowVectorPacket (V.map columnKind fields))
            return (fields, rows)

-- | 'query_' with each row decoded by a 'Decoder.RowDecoder' instead of into
-- 'MySQLValue's. The decoder is checked against the column definitions before
-- any row is read: a result set it does not fit raises 'Decoder.ColumnMismatch'.
-- A field that does not decode raises 'Decoder.FieldError' when its row is read.
-- Either way the remaining rows are skipped first, so the connection stays
-- usable.
--
-- A statement without a result-set, such as an INSERT, gives no columns and no
-- rows, as with 'query_'.
--
-- > import qualified Database.MySQL.Decoder as Decode
-- > import qualified System.IO.Streams as Streams
-- >
-- > data Employee = Employee { number :: Int32, born :: Day, name :: Text }
-- >
-- > employee :: Decode.RowDecoder Employee
-- > employee = Employee
-- >     <$> Decode.field Decode.int32
-- >     <*> Decode.field Decode.day
-- >     <*> Decode.field Decode.text
-- >
-- > (_, rows) <- queryRows_ employee conn "SELECT emp_no, birth_date, first_name FROM employees"
-- > employees <- Streams.toList rows
--
-- @since 1.4.0
queryRows_ :: Decoder.RowDecoder a -> MySQLConn -> Query -> IO ([ColumnDef], InputStream a)
queryRows_ decoder conn qry =
    queryRowBodies_ conn qry >>= decodeRowBodies (Proxy :: Proxy TextProtocol) decoder

-- | 'queryRows_' with parameters, filled in as 'query' does. With the
-- @employee@ decoder from 'queryRows_':
--
-- > (_, rows) <- queryRows employee conn
-- >     "SELECT emp_no, birth_date, first_name FROM employees WHERE first_name = ?"
-- >     [MySQLText "Georgi"]
-- > georgis <- Streams.toList rows
--
-- @since 1.4.0
queryRows :: QueryParam p => Decoder.RowDecoder a -> MySQLConn -> Query -> [p] -> IO ([ColumnDef], InputStream a)
queryRows decoder conn qry params = queryRows_ decoder conn (renderParams qry params)

{-# SPECIALIZE queryRows :: Decoder.RowDecoder a -> MySQLConn -> Query -> [MySQLValue] -> IO ([ColumnDef], InputStream a) #-}
{-# SPECIALIZE queryRows :: Decoder.RowDecoder a -> MySQLConn -> Query -> [Param]      -> IO ([ColumnDef], InputStream a) #-}

-- | 'queryStmt' with each row decoded by a 'Decoder.RowDecoder', as
-- 'queryRows_' does. With the @employee@ decoder from 'queryRows_':
--
-- > statement <- prepareStmt conn
-- >     "SELECT emp_no, birth_date, first_name FROM employees WHERE emp_no > ?"
-- > (_, rows) <- queryStmtRows employee conn statement [MySQLInt32 10001]
-- > later <- Streams.toList rows
--
-- @since 1.4.0
queryStmtRows :: Decoder.RowDecoder a -> MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream a)
queryStmtRows decoder conn stid params =
    queryStmtRowBodies conn stid params >>= decodeRowBodies (Proxy :: Proxy BinaryProtocol) decoder

decodeRowBodies :: forall protocol a. Decoder.RowProtocol protocol
                => Proxy protocol -> Decoder.RowDecoder a -> ([ColumnDef], InputStream ByteString)
                -> IO ([ColumnDef], InputStream a)
decodeRowBodies _ decoder (fields, bodies) = case fields of
    [] -> (,) [] <$> Stream.nullInput
    _  -> case Decoder.prepareRowDecoder @protocol decoder (V.fromList fields) of
        Left mismatch  -> Stream.skipToEof bodies >> throwIO mismatch
        Right prepared -> (,) fields <$> Stream.mapM (decodeRowBody prepared bodies) bodies

-- | A row that does not decode skips the rest first, so the connection stays
-- usable.
decodeRowBody :: Decoder.PreparedRow protocol a -> InputStream ByteString -> ByteString -> IO a
decodeRowBody prepared bodies row = case Decoder.runPreparedRow prepared row of
    Left fieldError -> Stream.skipToEof bodies >> throwIO fieldError
    Right value     -> pure value

-- | 'query_' handing each row over as a 'RawRow', with the bounds of its
-- fields, for libraries that decode by column number with
-- 'Decoder.prepareFieldParser' and 'Decoder.runFieldParser'. A parser is
-- prepared once per column from its definition, then run on every row; it
-- passes the field to a continuation instead of returning an 'Either':
--
-- > import qualified Database.MySQL.Decoder as Decode
-- > import qualified System.IO.Streams as Streams
-- >
-- > (columns, rows) <- queryRawRows_ conn "SELECT emp_no, first_name FROM employees"
-- > numberColumn : nameColumn : _ <- pure columns
-- > numbers <- either throwIO pure (Decode.prepareFieldParser Decode.int32 (ColumnNumber 0) numberColumn)
-- > names <- either throwIO pure (Decode.prepareFieldParser Decode.text (ColumnNumber 1) nameColumn)
-- > rawRows <- Streams.toList rows
-- > forM_ rawRows $ \row ->
-- >     Decode.runFieldParser numbers row (ColumnNumber 0) throwIO $ \number ->
-- >     Decode.runFieldParser names row (ColumnNumber 1) throwIO $ \name ->
-- >     print (number, name)
--
-- @since 1.4.0
queryRawRows_ :: MySQLConn -> Query -> IO ([ColumnDef], InputStream (RawRow TextProtocol))
queryRawRows_ conn qry = do
    (fields, bodies) <- queryRowBodies_ conn qry
    (,) fields <$> Stream.mapM (rawRowOrThrow (textRawRow (length fields))) bodies

-- | 'queryStmt' handing each row over as a 'RawRow', as 'queryRawRows_' does.
-- The parsers are prepared and run the same way; they read the binary
-- protocol's fields because the rows are 'RawRow' 'BinaryProtocol':
--
-- > statement <- prepareStmt conn "SELECT emp_no, first_name FROM employees WHERE emp_no > ?"
-- > (columns, rows) <- queryStmtRawRows conn statement [MySQLInt32 10001]
-- > numberColumn : nameColumn : _ <- pure columns
-- > numbers <- either throwIO pure (Decode.prepareFieldParser Decode.int32 (ColumnNumber 0) numberColumn)
-- > names <- either throwIO pure (Decode.prepareFieldParser Decode.text (ColumnNumber 1) nameColumn)
-- > rawRows <- Streams.toList rows
-- > forM_ rawRows $ \row ->
-- >     Decode.runFieldParser numbers row (ColumnNumber 0) throwIO $ \number ->
-- >     Decode.runFieldParser names row (ColumnNumber 1) throwIO $ \name ->
-- >     print (number, name)
--
-- @since 1.4.0
queryStmtRawRows :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream (RawRow BinaryProtocol))
queryStmtRawRows conn stid params = do
    (fields, bodies) <- queryStmtRowBodies conn stid params
    (,) fields <$> Stream.mapM (rawRowOrThrow (binaryRawRow (V.fromList (map binaryWidth fields)))) bodies

rawRowOrThrow :: (ByteString -> Either RowError (RawRow protocol)) -> ByteString -> IO (RawRow protocol)
rawRowOrThrow toRawRow row = either (throwRowError row) pure (toRawRow row)

-- | The column definitions that follow a column count.
--
-- Decision: read into a list first. 'V.replicateM' allocates a vector of the
-- count's size before reading any definition, so a count the server made up
-- would cost that much memory without being sent; see Note [Bounded replies].
readColumnDefVector :: Int -> InputStream Packet -> IO (V.Vector ColumnDef)
readColumnDefVector count inputStream = V.fromList <$> replicateM count ((decodeFromPacket <=< readPacket) inputStream)

-- | The bodies of a text-protocol result set's row packets.
queryRowBodies_ :: MySQLConn -> Query -> IO ([ColumnDef], InputStream ByteString)
queryRowBodies_ conn@(MySQLConn is os _ state) (Query qry) = runCommand conn $ do
    writeCommand (COM_QUERY qry) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state (pure . L.toStrict . pBody)
            return (fields, rows)

-- | The bodies of a prepared statement's row packets.
queryStmtRowBodies :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream ByteString)
queryStmtRowBodies conn@(MySQLConn is os _ state) stid params = runCommand conn $ do
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state binaryRowBody
            return (fields, rows)

-- | A binary-protocol row, whose packets start with 0x00 like an OK packet.
binaryRowBody :: Packet -> IO ByteString
binaryRowBody packet =
    if isOK packet then pure (L.toStrict (pBody packet)) else throwIO (UnexpectedPacket packet)

-- | One result of a statement run through 'queryMulti_'.
--
-- @since 1.3.3
data StatementResult
    = StatementOK OK                            -- ^ a statement without rows, e.g. an INSERT
    | StatementRows [ColumnDef] [[MySQLValue]]  -- ^ a result-set
    deriving (Show, Eq)

-- | Execute a MySQL query and return every result it produces, in order: one per
-- statement of a multi-statement query, and for a CALL one per result-set the
-- procedure returns followed by the CALL's own 'OK'.
--
-- Unlike 'query_' the rows are read up front, as each result-set has to be read
-- off the connection before the next one arrives. A failing statement raises its
-- 'ERRException'; the server runs no statement after it. More OKs than the
-- query has statements raise t'Database.MySQL.Connection.MoreResultsThanStatements'.
--
-- @since 1.3.3
queryMulti_ :: MySQLConn -> Query -> IO [StatementResult]
queryMulti_ conn@(MySQLConn is os _ _) (Query qry) = runCommand conn $ do
    writeCommand (COM_QUERY qry) os
    readStatementResults (statementBound qry) 0 is

-- | 'queryMulti_' with parameters, filled in as 'query' does.
--
-- @since 1.3.3
queryMulti :: QueryParam p => MySQLConn -> Query -> [p] -> IO [StatementResult]
queryMulti conn qry params = queryMulti_ conn (renderParams qry params)

{-# SPECIALIZE queryMulti :: MySQLConn -> Query -> [MySQLValue] -> IO [StatementResult] #-}
{-# SPECIALIZE queryMulti :: MySQLConn -> Query -> [Param]      -> IO [StatementResult] #-}

-- | Read a reply's results until one comes without SERVER_MORE_RESULTS_EXISTS,
-- given the most statements the query holds and the OKs read so far. See
-- Note [Bounded replies].
readStatementResults :: Int64 -> Int64 -> InputStream Packet -> IO [StatementResult]
readStatementResults statements oksRead inputStream = do
    packet <- readPacket inputStream
    if  | isERR packet -> decodeFromPacket packet >>= throwIO . ERRException
        | isOK  packet -> do
            ok <- decodeStatementOK statements oksRead packet
            if isThereMore ok
            then (StatementOK ok :) <$> readStatementResults statements (oksRead + 1) inputStream
            else pure [StatementOK ok]
        | otherwise -> do
            (result, eof) <- readResultSet inputStream packet
            if isThereMoreAfterEOF eof
            then (result :) <$> readStatementResults statements oksRead inputStream
            else pure [result]

-- | 'readStatementResults' for a reply that should hold only 'OK's. A
-- result-set raises 'ExtraResultSets' once the whole reply has been read,
-- skipping it and what follows rather than keeping it.
readStatementOKs :: Int64 -> Int64 -> InputStream Packet -> IO [OK]
readStatementOKs statements oksRead inputStream = do
    packet <- readPacket inputStream
    if  | isERR packet -> decodeFromPacket packet >>= throwIO . ERRException
        | isOK  packet -> do
            ok <- decodeStatementOK statements oksRead packet
            if isThereMore ok
            then (ok :) <$> readStatementOKs statements (oksRead + 1) inputStream
            else pure [ok]
        | otherwise -> do
            eof <- skipResultSet inputStream packet
            when (isThereMoreAfterEOF eof) (void (skipFurtherResults SomeResultSet inputStream))
            throwIO ExtraResultSets

-- | Decode one more of a reply's OK packets, refusing it when the statements
-- have all given theirs.
decodeStatementOK :: Int64 -> Int64 -> Packet -> IO OK
decodeStatementOK statements oksRead packet =
    if oksRead >= statements
    then throwIO (MoreResultsThanStatements statements)
    else decodeFromPacket packet

-- | The most statements a query holds: each but the last ends at a semicolon.
-- Semicolons inside strings, comments or compound statements only raise it.
statementBound :: L.ByteString -> Int64
statementBound queryText = LC.count ';' queryText + 1

-- | Read the text-protocol result-set this column-count packet starts, with the
-- EOF packet that closes it.
readResultSet :: InputStream Packet -> Packet -> IO (StatementResult, EOF)
readResultSet is columnCountPacket = do
    columnCount <- getFromPacket getColumnCount columnCountPacket
    fields <- replicateM columnCount ((decodeFromPacket <=< readPacket) is)
    _ <- readPacket is -- eof packet after the column definitions
    (rows, eof) <- readTextRows (map columnKind fields) [] is
    pure (StatementRows fields rows, eof)

-- | Read text-protocol rows up to the EOF packet that ends them; the rows read
-- so far are kept in reverse.
readTextRows :: [ColumnKind] -> [[MySQLValue]] -> InputStream Packet -> IO ([[MySQLValue]], EOF)
readTextRows columns earlierRows is = do
    q <- readPacket is
    if  | isEOF q -> (,) (reverse earlierRows) <$> decodeFromPacket q
        | isERR q -> decodeFromPacket q >>= throwIO . ERRException
        | otherwise -> do
            row <- decodeTextRowPacket columns q
            readTextRows columns (row : earlierRows) is

-- | How the server answered a statement sent through a query function.
data QueryReply
    = WithoutResultSet      -- ^ only OK packets, e.g. for an INSERT
    | ResultSetColumns Int  -- ^ a result-set with this many columns follows

-- | Read the reply to a query function's statement up to its first result-set.
--
-- An OK packet must be told apart here: its leading 0x00 also decodes as a
-- column count of zero, which used to leave the caller waiting forever for an
-- EOF packet the server never sends (issue #47). OKs flagged with more results
-- are skipped, so @SET \@x := 1; SELECT \@x@ gives the SELECT's rows.
--
-- Decision: answer such a statement with an empty result-set rather than an
-- exception, so code that worked around the hang keeps working and the fix is
-- not a breaking change. The OK's affected-rows count is dropped; 'execute_'
-- returns it.
readQueryReply :: InputStream Packet -> IO QueryReply
readQueryReply is = do
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> do
            ok <- decodeFromPacket p
            if isThereMore ok then readQueryReply is else pure WithoutResultSet
        | otherwise -> ResultSetColumns <$> getFromPacket getColumnCount p

-- | A result-set's column count. MySQL and MariaDB hold it in an unsigned int,
-- so a larger count means a broken packet, and reading on would take every
-- packet after it for a column definition. See Note [Bounded replies].
getColumnCount :: Get Int
getColumnCount = do
    count <- getLenEncWord64
    if count > 4294967295
    then fail ("column count " ++ show count ++ " is above 4294967295, the most a server sends")
    else maybe (fail ("column count " ++ show count ++ " does not fit in an Int")) pure (Word64.toInt count)

-- | A text-protocol row packet decoded with 'decodeTextRow', raising
-- 'DecodePacketFailed' as 'getFromPacket' does.
decodeTextRowPacket :: [ColumnKind] -> Packet -> IO [MySQLValue]
decodeTextRowPacket columns packet = do
    let row = L.toStrict (pBody packet)
    either (throwRowError row) pure (decodeTextRow columns row)

-- | 'V.Vector' version of 'decodeTextRowPacket'.
decodeTextRowVectorPacket :: V.Vector ColumnKind -> Packet -> IO (V.Vector MySQLValue)
decodeTextRowVectorPacket columns packet = do
    let row = L.toStrict (pBody packet)
    either (throwRowError row) pure (decodeTextRowVector columns row)

throwRowError :: ByteString -> RowError -> IO a
throwRowError row rowError =
    throwIO (DecodePacketFailed (B.drop offset row) (Int.toInt64 offset)
                                (describeRowError rowError))
  where
    offset = rowErrorOffset rowError

-- | The rows of a result-set, read as they are asked for, up to its EOF packet.
--
-- Once finished, also when 'finishAfterEOF' throws, further reads answer
-- Nothing rather than wait on the socket for packets that will never come.
-- A read stopped part-way leaves the connection at 'ReplyAbandoned', see
-- Note [Abandoned replies].
resultSetRows :: InputStream Packet -> IORef ReplyState -> (Packet -> IO row) -> IO (InputStream row)
resultSetRows is state decodeRow = do
    finished <- newIORef False
    Stream.makeInputStream $ mask $ \restore -> do
        alreadyFinished <- readIORef finished
        current <- readIORef state
        if  | alreadyFinished -> pure Nothing
            | current == ReplyAbandoned -> throwIO AbandonedReply
            | otherwise -> restore (nextRow is state finished decodeRow)
                `catch` settleFailedRowRead state finished

nextRow :: InputStream Packet -> IORef ReplyState -> IORef Bool -> (Packet -> IO row) -> IO (Maybe row)
nextRow is state finished decodeRow = do
    q <- readPacket is
    if  | isEOF q -> do
            writeIORef finished True
            finishAfterEOF is q
            writeIORef state RepliesRead
            pure Nothing
        | isERR q -> decodeFromPacket q >>= throwIO . ERRException
        | otherwise -> Just <$> decodeRow q

-- | Like 'settleFailedExchange', and a row that failed to decode was still
-- read in full, so the rest of the rows can be read or skipped.
settleFailedRowRead :: IORef ReplyState -> IORef Bool -> SomeException -> IO a
settleFailedRowRead state finished failure = do
    if  | endsReply failure -> writeIORef finished True >> writeIORef state RepliesRead
        | isJust (fromException failure :: Maybe DecodePacketException) -> pure ()
        | otherwise -> writeIORef state ReplyAbandoned
    throwIO failure

-- | A binary-protocol row, whose packets start with 0x00 like an OK packet. A
-- row that does not decode raises 'DecodePacketFailed', as it did when rows
-- were decoded with 'getBinaryRow'.
binaryValuesPacket :: Decoder.BinaryValueColumns -> Packet -> IO [MySQLValue]
binaryValuesPacket columns packet = do
    row <- binaryRowBody packet
    case Decoder.decodeBinaryValues columns row of
        Left fieldError -> throwIO (DecodePacketFailed row 0 (show fieldError))
        Right values    -> pure values

-- | What followed a reply flagged SERVER_MORE_RESULTS_EXISTS.
data FurtherResults
    = OnlyOKs        -- ^ e.g. the closing OK of a CALL, or more INSERTs' OKs
    | SomeResultSet  -- ^ at least one result-set, which was discarded

-- | Finish a reply that ended in this 'OK', see 'skipFurtherResults'.
finishAfterOK :: InputStream Packet -> OK -> IO ()
finishAfterOK is ok =
    when (isThereMore ok) (skipFurtherResults OnlyOKs is >>= throwOnResultSet)

-- | Finish a result-set at its EOF packet, see 'skipFurtherResults'.
finishAfterEOF :: InputStream Packet -> Packet -> IO ()
finishAfterEOF is eofPacket = do
    eof <- decodeFromPacket eofPacket
    when (isThereMoreAfterEOF eof) (skipFurtherResults OnlyOKs is >>= throwOnResultSet)

-- | SERVER_MORE_RESULTS_EXISTS on a result-set's closing EOF packet.
isThereMoreAfterEOF :: EOF -> Bool
isThereMoreAfterEOF eof = eofStatus eof .&. 0x08 /= 0

-- | Read every result that follows a reply flagged SERVER_MORE_RESULTS_EXISTS.
--
-- The client asks for multi-statements and multi-results, so a CALL or a query
-- holding several statements gets one result per statement. Any left unread
-- would be taken by the next command as its own reply, shifting every later
-- query's results by one.
skipFurtherResults :: FurtherResults -> InputStream Packet -> IO FurtherResults
skipFurtherResults seen is = do
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> do
            ok <- decodeFromPacket p
            if isThereMore ok then skipFurtherResults seen is else pure seen
        | otherwise -> do
            eof <- skipResultSet is p
            if isThereMoreAfterEOF eof
            then skipFurtherResults SomeResultSet is
            else pure SomeResultSet

-- | Read the result-set this column-count packet starts, returning its closing
-- EOF packet.
skipResultSet :: InputStream Packet -> Packet -> IO EOF
skipResultSet is columnCountPacket = do
    columnCount <- getFromPacket getColumnCount columnCountPacket
    replicateM_ columnCount (readPacket is)
    _ <- readPacket is -- eof packet after the column definitions
    skipRows is

-- | Read a result-set's rows up to and including the EOF packet that ends them.
skipRows :: InputStream Packet -> IO EOF
skipRows is = do
    q <- readPacket is
    if  | isEOF q -> decodeFromPacket q
        | isERR q -> decodeFromPacket q >>= throwIO . ERRException
        | otherwise -> skipRows is

throwOnResultSet :: FurtherResults -> IO ()
throwOnResultSet further = case further of
    OnlyOKs -> pure ()
    SomeResultSet -> throwIO ExtraResultSets

-- | Ask MySQL to prepare a query statement.
--
prepareStmt :: MySQLConn -> Query -> IO StmtID
prepareStmt conn@(MySQLConn is os _ _) (Query stmt) = runCommand conn $ do
    writeCommand (COM_STMT_PREPARE stmt) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        StmtPrepareOK stid colCnt paramCnt _ <- getFromPacket getStmtPrepareOK p
        _ <- replicateM_ paramCnt (readPacket is)
        _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
        _ <- replicateM_ colCnt (readPacket is)
        _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
        return stid

-- | Ask MySQL to prepare a query statement.
--
-- All details from @COM_STMT_PREPARE@ Response are returned: the 'StmtPrepareOK' packet,
-- params's 'ColumnDef', result's 'ColumnDef'.
--
prepareStmtDetail :: MySQLConn -> Query -> IO (StmtPrepareOK, [ColumnDef], [ColumnDef])
prepareStmtDetail conn@(MySQLConn is os _ _) (Query stmt) = runCommand conn $ do
    writeCommand (COM_STMT_PREPARE stmt) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        sOK@(StmtPrepareOK _ colCnt paramCnt _) <- getFromPacket getStmtPrepareOK p
        pdefs <- replicateM paramCnt ((decodeFromPacket <=< readPacket) is)
        _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
        cdefs <- replicateM colCnt ((decodeFromPacket <=< readPacket) is)
        _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
        return (sOK, pdefs, cdefs)

-- | Ask MySQL to closed a query statement.
--
closeStmt :: MySQLConn -> StmtID -> IO ()
closeStmt (MySQLConn _ os _ _) stid = do
    writeCommand (COM_STMT_CLOSE stid) os

-- | Ask MySQL to reset a query statement: discard the data sent for its
-- parameters and close its cursor.
--
-- It does not discard a result-set the statement returned: the server has
-- already sent its rows, so they must be read or skipped first, or this throws
-- 'UnconsumedResultSet'.
--
resetStmt :: MySQLConn -> StmtID -> IO ()
resetStmt conn@(MySQLConn is os _ _) stid = runCommand conn $ do
    writeCommand (COM_STMT_RESET stid) os
    p <- readPacket is
    when (isERR p) (decodeFromPacket p >>= throwIO . ERRException)

-- | Execute prepared query statement with parameters, expecting no resultset.
--
executeStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO OK
executeStmt conn stid params =
  executeCommand conn (COM_STMT_EXECUTE stid params (makeNullMap params))

-- | Send a statement that should answer with an 'OK' and read its whole reply,
-- see 'skipFurtherResults'. A result-set, also as the first reply (a SELECT or
-- a CALL running one), is read off the connection and raises 'ExtraResultSets',
-- unless a later statement fails: its 'ERRException' is raised instead.
executeCommand :: MySQLConn -> Command -> IO OK
executeCommand conn@(MySQLConn is os _ _) cmd = runCommand conn $ do
    writeCommand cmd os
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p -> do
            ok <- decodeFromPacket p
            finishAfterOK is ok
            pure ok
        | otherwise -> do
            eof <- skipResultSet is p
            when (isThereMoreAfterEOF eof) (void (skipFurtherResults SomeResultSet is))
            throwIO ExtraResultSets

-- | Execute prepared query statement with parameters, expecting resultset.
--
-- Rules about 'UnconsumedResultSet' applied here too. A statement without a
-- result-set gives no columns and no rows; use 'executeStmt' to get its 'OK'.
--
queryStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream [MySQLValue])
queryStmt conn@(MySQLConn is os _ state) stid params = runCommand conn $ do
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state (binaryValuesPacket (Decoder.binaryValueColumns fields))
            return (fields, rows)

-- | 'V.Vector' version of 'queryStmt'
--
-- @since 0.5.1.0
--
queryStmtVector :: MySQLConn -> StmtID -> [MySQLValue] -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryStmtVector conn@(MySQLConn is os _ state) stid params = runCommand conn $ do
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) V.empty <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- readColumnDefVector len is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef state RowsUnread
            rows <- resultSetRows is state
                (fmap V.fromList . binaryValuesPacket (Decoder.binaryValueColumns (V.toList fields)))
            return (fields, rows)

-- | Run querys inside a transaction, querys will be rolled back if exception arise.
--
-- The procedure's exception is the one rethrown. When the connection cannot
-- take the ROLLBACK, because the procedure left a result-set unread or was
-- interrupted mid-reply, it is left at 'AbandonedReply' instead, and the server
-- rolls the transaction back when the connection closes.
--
-- @since 0.2.0.0
--
withTransaction :: MySQLConn -> IO a -> IO a
withTransaction conn procedure = mask $ \restore -> do
  _ <- execute_ conn "BEGIN"
  r <- restore procedure `onException` rollbackOrAbandon conn
  _ <- execute_ conn "COMMIT"
  pure r

rollbackOrAbandon :: MySQLConn -> IO ()
rollbackOrAbandon conn@(MySQLConn _ _ _ state) = do
  current <- readIORef state
  case current of
    RepliesRead -> void (execute_ conn "ROLLBACK")
    RowsUnread -> writeIORef state ReplyAbandoned
    ReplyAbandoned -> pure ()
