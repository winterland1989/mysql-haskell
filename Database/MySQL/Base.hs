{-|
Module      : Database.MySQL.Base
Description : Prelude of mysql-haskell
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide common MySQL operations,

NOTEs on 'Exception's: This package use 'Exception' to deal with unexpected situations, and
all exception type are documented, but you shouldn't try to catch them if you don't have a recover plan,
for example: 1) there's no meaning to catch a 'AuthException' unless you want to try different passwords.
2) a 'UnconsumedResultSet' indicates your program have operation problems.
.

-}
module Database.MySQL.Base
    ( -- * Setting up and control connection
      MySQLConn
    , ConnectInfo(..)
    , defaultConnectInfo
    , connect
    , connectDetail
    , close
    , ping
      -- * direct query
    , execute
    , execute_
    , query_
    , query
      -- * prepared query statement
    , prepareStmt
    , prepareStmtDetail
    , executeStmt
    , queryStmt
    , closeStmt
    , resetStmt
      -- * MySQL protocol
    , module  Database.MySQL.Protocol.Auth
    , module  Database.MySQL.Protocol.Command
    , module  Database.MySQL.Protocol.ColumnDef
    , module  Database.MySQL.Protocol.Packet
    , module  Database.MySQL.Protocol.MySQLValue
      -- * helpers
    , Query(..)
    , renderParams
    , command
    , Stream.skipToEof
    ) where

import           Control.Applicative
import           Control.Exception         (throwIO)
import           Control.Monad
import           Data.IORef                (writeIORef)
import           Database.MySQL.Connection
import           Database.MySQL.Protocol.Auth
import           Database.MySQL.Protocol.ColumnDef
import           Database.MySQL.Protocol.Command
import           Database.MySQL.Protocol.MySQLValue
import           Database.MySQL.Protocol.Packet

import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
import           Database.MySQL.Query

--------------------------------------------------------------------------------

-- | Execute a MySQL query with parameters which don't return a resultSet.
--
-- The query may contain placeholders @?@, for filling up parameters, the parameters
-- will be escaped before get filled into the query, please DO NOT enable @NO_BACKSLASH_ESCAPES@,
-- and you should consider using prepared statement if this's not an one shot query.
--
execute :: MySQLConn -> Query -> [MySQLValue] -> IO OK
execute conn qry params = execute_ conn (renderParams qry params)

-- | Execute a MySQL query which don't return a resultSet.
--
execute_ :: MySQLConn -> Query -> IO OK
execute_ conn (Query qry) = command conn (COM_QUERY qry)

{-
executeBatch :: MySQLConn -> Query -> OutputStream [MySQLValue] -> IO (InputStream OK)
executeBatch
-}

-- | Execute a MySQL query which return a result-set with parameters.
--
-- Note that you must fully consumed the result-set before start a new query on
-- the same 'MySQLConn', or an 'UnconsumedResultSet' will be thrown.
-- if you want to skip the result-set, use 'Stream.skipToEof'.
--
query :: MySQLConn -> Query -> [MySQLValue] -> IO ([ColumnDef], InputStream [MySQLValue])
query conn qry params = query_ conn (renderParams qry params)

-- | Execute a MySQL query which return a resultSet.
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
        _ <- replicateM paramCnt (readPacket is)
        _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
        _ <- replicateM colCnt (readPacket is)
        _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
        return stid

-- | Ask MySQL to prepare a query statement.
--
-- All details from @COM_STMT_PREPARE@ Response are returned: the 'StmtPrepareOK' packet,
-- params's 'ColumnDef', table's 'ColumnDef'.
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
        _ <- unless (colCnt == 0) (void (readPacket is))  -- EOF
        cdefs <- replicateM colCnt ((decodeFromPacket <=< readPacket) is)
        _ <- unless (paramCnt == 0) (void (readPacket is))  -- EOF
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
executeStmt conn stid params = command conn (COM_STMT_EXECUTE stid params)

-- | Execute prepared query statement with parameters, expecting resultset.
--
queryStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO ([ColumnDef], InputStream [MySQLValue])
queryStmt conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params) os
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
            if  | isOK  q  -> Just <$> getFromPacket (getBinaryRow fields len) q -- fast path for decode rows
                | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>= throwIO . ERRException
                | otherwise -> throwIO NetworkException
        return (fields, rows)
