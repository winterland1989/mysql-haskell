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

    * 'UnconsumedResultSet':  you should consume previous resultset before sending new command.
    * 'ERRException':  you receive a 'ERR' packet when you shouldn't.
    * 'UnexpectedPacket':  you receive a unexpected packet when you shouldn't.
    * 'DecodePacketException': there's a packet we can't decode.
    * 'WrongParamsCount': you're giving wrong number of params to 'renderParams'.

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
    , ping
      -- * Direct query
    , execute
    , executeMany
    , executeMany_
    , execute_
    , query_
    , query
      -- * Prepared query statement
    , prepareStmt
    , prepareStmtDetail
    , executeStmt
    , queryStmt
    , closeStmt
    , resetStmt
      -- * Helpers
    , withTransaction
    , QueryParam(..)
    , Param (..)
    , Query(..)
    , renderParams
    , command
      -- * Exceptions
    , UnconsumedResultSet(..)
    , ERRException(..)
    , UnexpectedPacket(..)
    , WrongParamsCount(..)
      -- * MySQL protocol
    , module  Database.MySQL.Protocol.Auth
    , module  Database.MySQL.Protocol.Command
    , module  Database.MySQL.Protocol.ColumnDef
    , module  Database.MySQL.Protocol.Packet
    , module  Database.MySQL.Protocol.MySQLValue
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.IORef                         (writeIORef)
import           Database.MySQL.Connection
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet
import           Database.MySQL.Query
import           Z.Data.ASCII
import qualified Z.Data.Vector                      as V
import qualified Z.Data.Text                        as T
import           Z.IO

--------------------------------------------------------------------------------

-- | Execute a MySQL query with parameters which don't return a result-set.
--
-- The query may contain placeholders @?@, for filling up parameters, the parameters
-- will be escaped before get filled into the query, please DO NOT enable @NO_BACKSLASH_ESCAPES@,
-- and you should consider using prepared statement if this's not an one shot query.
--
execute :: QueryParam p => MySQLConn -> Query -> [p] -> IO OK
{-# INLINABLE execute #-}
execute conn qry params = execute_ conn (renderParams qry params)

-- | Execute a multi-row query which don't return result-set.
--
-- Leverage MySQL's multi-statement support to do batch insert\/update\/delete,
-- you may want to use 'withTransaction' to make sure it's atomic, and
-- use @sum . map okAffectedRows@ to get all affected rows count.
--
-- @since 0.2.0.0
--
executeMany :: QueryParam p => MySQLConn -> Query -> [[p]] -> IO [OK]
{-# INLINABLE executeMany #-}
executeMany conn@(MySQLConn is os _) qry paramsList = do
    guardUnconsumed conn
    let qry' = V.intercalate ";" $ map (fromQuery . renderParams qry) paramsList
    writeCommand (COM_QUERY qry') os
    mapM (\ _ -> waitCommandReply is) paramsList

-- | Execute multiple querys (without param) which don't return result-set.
--
-- This's useful when your want to execute multiple SQLs without params, e.g. from a
-- SQL dump, or a table migration plan.
--
-- @since 0.8.4.0
--
executeMany_ :: MySQLConn -> Query -> IO [OK]
{-# INLINABLE executeMany_ #-}
executeMany_ conn@(MySQLConn is os _) qry = do
    guardUnconsumed conn
    writeCommand (COM_QUERY (fromQuery qry)) os
    waitCommandReplys is

-- | Execute a MySQL query which don't return a result-set.
--
execute_ :: MySQLConn -> Query -> IO OK
execute_ conn (Query qry) = command conn (COM_QUERY qry)

-- | Execute a MySQL query which return a result-set with parameters.
--
-- Note that you must fully consumed the result-set before start a new query on
-- the same 'MySQLConn', or an 'UnconsumedResultSet' will be thrown.
-- if you want to skip the result-set, use 'Stream.skipToEof'.
--
query :: QueryParam p => MySQLConn -> Query -> [p] -> IO (V.Vector ColumnDef, Source (V.Vector MySQLValue))
query conn qry params = query_ conn (renderParams qry params)

readFields :: HasCallStack => Int -> BufferedInput -> IO (V.Vector ColumnDef)
{-# INLINABLE readFields #-}
readFields len is =
    V.replicateMVec len (decodeFromPacket decodeField =<< readPacket is)

-- | Execute a MySQL query which return a result-set.
--
query_ :: HasCallStack => MySQLConn -> Query -> IO (V.Vector ColumnDef, Source (V.Vector MySQLValue))
query_ conn@(MySQLConn is os consumed) (Query qry) = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    p <- readPacket is
    len <- decodeFromPacket decodeLenEncInt p
    fields <- readFields len is
    _ <- readPacket is -- eof packet, we don't verify this though
    writeIORef consumed False
    let rows = sourceFromIO $ do
            q <- readPacket is
            if isEOF q
            then writeIORef consumed True >> return Nothing
            else Just <$> decodeFromPacket (decodeTextRow fields) q
    return (fields, rows)

-- | Ask MySQL to prepare a query statement.
--
prepareStmt :: HasCallStack => MySQLConn -> Query -> IO StmtID
prepareStmt conn@(MySQLConn is os _) (Query stmt) = do
    guardUnconsumed conn
    writeCommand (COM_STMT_PREPARE stmt) os
    p <- readPacket is
    StmtPrepareOK stid colCnt paramCnt _ <- decodeFromPacket decodeStmtPrepareOK p
    _ <- replicateM_ (fromIntegral paramCnt) (readPacket is)
    _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
    _ <- replicateM_ (fromIntegral colCnt) (readPacket is)
    _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
    return stid

-- | Ask MySQL to prepare a query statement.
--
-- All details from @COM_STMT_PREPARE@ Response are returned: the 'StmtPrepareOK' packet,
-- params's 'ColumnDef', result's 'ColumnDef'.
--
prepareStmtDetail :: HasCallStack => MySQLConn -> Query -> IO (StmtPrepareOK, V.Vector ColumnDef, V.Vector ColumnDef)
prepareStmtDetail conn@(MySQLConn is os _) (Query stmt) = do
    guardUnconsumed conn
    writeCommand (COM_STMT_PREPARE stmt) os
    p <- readPacket is
    sOK@(StmtPrepareOK _ colCnt paramCnt _) <- decodeFromPacket decodeStmtPrepareOK p
    pdefs <- readFields (fromIntegral paramCnt) is
    _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
    cdefs <- readFields (fromIntegral colCnt) is
    _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
    return (sOK, pdefs, cdefs)

-- | Ask MySQL to closed a query statement.
--
closeStmt :: MySQLConn -> StmtID -> IO ()
closeStmt (MySQLConn _ os _) stid = do
    writeCommand (COM_STMT_CLOSE stid) os

-- | Ask MySQL to reset a query statement, all previous resultset will be cleared.
--
-- This function can be used when you want to stop a long running query from another thread.
-- Which will lead a thread running `queryStmt` reach its EOF.
resetStmt :: MySQLConn -> StmtID -> IO ()
resetStmt (MySQLConn _ os _) stid = do
    writeCommand (COM_STMT_RESET stid) os  -- previous result-set may still be unconsumed

-- | Execute prepared query statement with parameters, expecting no resultset.
--
executeStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO OK
executeStmt conn stid params =
  command conn (COM_STMT_EXECUTE stid params (makeNullMap params))

-- | Execute prepared query statement with parameters, expecting resultset.
--
-- Rules about 'UnconsumedResultSet' applied here too.
--
queryStmt :: HasCallStack
          => MySQLConn -> StmtID -> [MySQLValue] -> IO (V.Vector ColumnDef, Source (V.Vector MySQLValue))
queryStmt conn@(MySQLConn is os consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    p <- readPacket is
    len <- decodeFromPacket decodeLenEncInt p
    fields <- readFields len is
    _ <- readPacket is -- eof packet, we don't verify this though
    writeIORef consumed False
    let rows = sourceFromIO $ do
            q <- readPacket is
            if  | isOK  q  -> Just <$> decodeFromPacket (decodeBinaryRow fields len) q
                | isEOF q  -> writeIORef consumed True >> return Nothing
                | otherwise -> throwIO (UnexpectedPacket q callStack)
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
