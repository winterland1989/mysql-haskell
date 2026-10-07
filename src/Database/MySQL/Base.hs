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
    * 'ERRException':  you receive a 'ERR' packet when you shouldn't.
    * 'UnexpectedPacket':  you receive a unexpected packet when you shouldn't.
    * 'DecodePacketException': there's a packet we can't decode.
    * 'WrongParamsCount': you're giving wrong number of params to 'renderParams'.
    * 'ExtraResultSets': a statement produced a result-set the function running it
      could not return.

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
      -- * Direct query
    , execute
    , executeMany
    , executeMany_
    , execute_
    , query_
    , queryVector_
    , query
    , queryVector
      -- * Prepared query statement
    , prepareStmt
    , prepareStmtDetail
    , executeStmt
    , queryStmt
    , queryStmtVector
    , closeStmt
    , resetStmt
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
    ) where

import           Control.Exception                  (mask, onException, throwIO)
import           Control.Monad
import           Data.Binary                        (Get)
import           Data.Bits                          ((.&.))
import qualified Data.ByteString.Lazy               as L
import           Data.IORef                         (IORef, newIORef, readIORef, writeIORef)
import           Database.MySQL.Connection
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet

import           Database.MySQL.Query
import           System.IO.Streams                  (InputStream)
import qualified System.IO.Streams                  as Stream
import qualified Data.Vector                        as V

--------------------------------------------------------------------------------

-- | Execute a MySQL query with parameters which don't return a result-set.
--
-- The query may contain placeholders @?@, for filling up parameters, the parameters
-- will be escaped before get filled into the query, please DO NOT enable @NO_BACKSLASH_ESCAPES@,
-- and you should consider using prepared statement if this's not an one shot query.
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
-- @since 0.2.0.0
--
executeMany :: QueryParam p => MySQLConn -> Query -> [[p]] -> IO [OK]
executeMany conn@(MySQLConn is os _ _) qry paramsList = do
    guardUnconsumed conn
    let qry' = L.intercalate ";" $ map (fromQuery . renderParams qry) paramsList
    writeCommand (COM_QUERY qry') os
    mapM (\ _ -> waitCommandReply is) paramsList

{-# SPECIALIZE executeMany :: MySQLConn -> Query -> [[MySQLValue]] -> IO [OK] #-}
{-# SPECIALIZE executeMany :: MySQLConn -> Query -> [[Param]]      -> IO [OK] #-}

-- | Execute multiple querys (without param) which don't return result-set.
--
-- This's useful when your want to execute multiple SQLs without params, e.g. from a
-- SQL dump, or a table migration plan.
--
-- @since 0.8.4.0
--
executeMany_ :: MySQLConn -> Query -> IO [OK]
executeMany_ conn@(MySQLConn is os _ _) qry = do
    guardUnconsumed conn
    writeCommand (COM_QUERY (fromQuery qry)) os
    waitCommandReplys is

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
query_ conn@(MySQLConn is os _ consumed) (Query qry) = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef consumed False
            rows <- resultSetRows is consumed (getFromPacket (getTextRow fields))
            return (fields, rows)

-- | 'V.Vector' version of 'query_'.
--
-- @since 0.5.1.0
--
queryVector_ :: MySQLConn -> Query -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryVector_ conn@(MySQLConn is os _ consumed) (Query qry) = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) V.empty <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- V.replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef consumed False
            rows <- resultSetRows is consumed (getFromPacket (getTextRowVector fields))
            return (fields, rows)

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
        | otherwise -> ResultSetColumns <$> getFromPacket getLenEncInt p

-- | The rows of a result-set, read as they are asked for, up to its EOF packet.
--
-- Once finished, also when 'finishAfterEOF' throws, further reads answer
-- Nothing rather than wait on the socket for packets that will never come.
resultSetRows :: InputStream Packet -> IORef Bool -> (Packet -> IO row) -> IO (InputStream row)
resultSetRows is consumed decodeRow = do
    finished <- newIORef False
    Stream.makeInputStream $ do
        alreadyFinished <- readIORef finished
        if alreadyFinished then pure Nothing else do
            q <- readPacket is
            if  | isEOF q -> do
                    writeIORef finished True
                    writeIORef consumed True
                    finishAfterEOF is q
                    pure Nothing
                | isERR q -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> Just <$> decodeRow q

-- | A binary-protocol row, whose packets start with 0x00 like an OK packet.
decodeBinaryRow :: Get row -> Packet -> IO row
decodeBinaryRow getRow q =
    if isOK q then getFromPacket getRow q else throwIO (UnexpectedPacket q)

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
    columnCount <- getFromPacket getLenEncInt columnCountPacket
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
prepareStmt conn@(MySQLConn is os _ _) (Query stmt) = do
    guardUnconsumed conn
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
prepareStmtDetail conn@(MySQLConn is os _ _) (Query stmt) = do
    guardUnconsumed conn
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

-- | Ask MySQL to reset a query statement, all previous resultset will be cleared.
--
resetStmt :: MySQLConn -> StmtID -> IO ()
resetStmt (MySQLConn is os _ consumed) stid = do
    writeCommand (COM_STMT_RESET stid) os  -- previous result-set may still be unconsumed
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else writeIORef consumed True

-- | Execute prepared query statement with parameters, expecting no resultset.
--
executeStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO OK
executeStmt conn stid params =
  executeCommand conn (COM_STMT_EXECUTE stid params (makeNullMap params))

-- | Send a statement that should answer with an 'OK' and read its whole reply,
-- see 'skipFurtherResults'. A result-set, also as the first reply (a SELECT or
-- a CALL running one), is read off the connection and raises 'ExtraResultSets'.
executeCommand :: MySQLConn -> Command -> IO OK
executeCommand conn@(MySQLConn is os _ _) cmd = do
    guardUnconsumed conn
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
queryStmt conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) [] <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef consumed False
            rows <- resultSetRows is consumed (decodeBinaryRow (getBinaryRow fields len))
            return (fields, rows)

-- | 'V.Vector' version of 'queryStmt'
--
-- @since 0.5.1.0
--
queryStmtVector :: MySQLConn -> StmtID -> [MySQLValue] -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryStmtVector conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    reply <- readQueryReply is
    case reply of
        WithoutResultSet -> (,) V.empty <$> Stream.nullInput
        ResultSetColumns len -> do
            fields <- V.replicateM len $ (decodeFromPacket <=< readPacket) is
            _ <- readPacket is -- eof packet, we don't verify this though
            writeIORef consumed False
            rows <- resultSetRows is consumed (decodeBinaryRow (getBinaryRowVector fields len))
            return (fields, rows)

-- | Run querys inside a transaction, querys will be rolled back if exception arise.
--
-- @since 0.2.0.0
--
withTransaction :: MySQLConn -> IO a -> IO a
withTransaction conn procedure = mask $ \restore -> do
  _ <- execute_ conn "BEGIN"
  r <- restore procedure `onException` (execute_ conn "ROLLBACK")
  _ <- execute_ conn "COMMIT"
  pure r
