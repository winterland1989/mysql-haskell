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
    , close
    , ping
      -- * Direct query
    , execute
    , executeMany
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
      -- * MySQL protocol
    , module  Database.MySQL.Protocol.Auth
    , module  Database.MySQL.Protocol.Command
    , module  Database.MySQL.Protocol.ColumnDef
    , module  Database.MySQL.Protocol.Packet
    , module  Database.MySQL.Protocol.MySQLValue
    ) where

import           Control.Applicative
import           Control.Exception                  (mask, onException, throwIO)
import           Control.Monad
import qualified Data.ByteString.Lazy               as L
import           Data.IORef                         (writeIORef)
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
query_ :: MySQLConn -> Query -> IO ([ColumnDef], InputStream [MySQLValue])
query_ conn@(MySQLConn is os _ consumed) (Query qry) = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        len <- getFromPacket getLenEncInt p
        fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
        _ <- readPacket is -- eof packet, we don't verify this though
        writeIORef consumed False
        rows <- Stream.makeInputStream $ do
            q <- readPacket is
            if  | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> Just <$> getFromPacket (getTextRow fields) q
        return (fields, rows)

-- | 'V.Vector' version of 'query_'.
--
-- @since 0.5.1.0
--
queryVector_ :: MySQLConn -> Query -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryVector_ conn@(MySQLConn is os _ consumed) (Query qry) = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        len <- getFromPacket getLenEncInt p
        fields <- V.replicateM len $ (decodeFromPacket <=< readPacket) is
        _ <- readPacket is -- eof packet, we don't verify this though
        writeIORef consumed False
        rows <- Stream.makeInputStream $ do
            q <- readPacket is
            if  | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> Just <$> getFromPacket (getTextRowVector fields) q
        return (fields, rows)

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
  command conn (COM_STMT_EXECUTE stid params (makeNullMap params))

-- | Execute prepared query statement with parameters, expecting resultset.
--
-- Rules about 'UnconsumedResultSet' applied here too.
--
queryStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream [MySQLValue])
queryStmt conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        len <- getFromPacket getLenEncInt p
        fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
        _ <- readPacket is -- eof packet, we don't verify this though
        writeIORef consumed False
        rows <- Stream.makeInputStream $ do
            q <- readPacket is
            if  | isOK  q  -> Just <$> getFromPacket (getBinaryRow fields len) q
                | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> throwIO (UnexpectedPacket q)
        return (fields, rows)

-- | 'V.Vector' version of 'queryStmt'
--
-- @since 0.5.1.0
--
queryStmtVector :: MySQLConn -> StmtID -> [MySQLValue] -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
queryStmtVector conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params (makeNullMap params)) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        len <- getFromPacket getLenEncInt p
        fields <- V.replicateM len $ (decodeFromPacket <=< readPacket) is
        _ <- readPacket is -- eof packet, we don't verify this though
        writeIORef consumed False
        rows <- Stream.makeInputStream $ do
            q <- readPacket is
            if  | isOK  q  -> Just <$> getFromPacket (getBinaryRowVector fields len) q
                | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> throwIO (UnexpectedPacket q)
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
