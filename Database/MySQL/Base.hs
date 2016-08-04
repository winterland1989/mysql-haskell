{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Database.MySQL.Base
Description : Prelude of mysql-haskell
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : PORTABLE

This module provide common MySQL operations.

-}
module Database.MySQL.Base
    ( -- * Setting up and control connection
      MySQLConn
    , ConnInfo(..)
    , defaultConnectInfo
    , connect
    , close
    , ping
      -- * direct query
    , execute
    , query
      -- * prepared query statement
    , prepareStmt
    , executeStmt
    , queryStmt
    , closeStmt
    , resetStmt
      -- * helpers
    , command
    ) where

import           Control.Exception        (throwIO)
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.IORef               (writeIORef)
import           Database.MySQL.Protocol
import           Database.MySQL.Connection
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream

--------------------------------------------------------------------------------

-- | Execute a MySQL query which don't return a resultSet.
--
execute :: MySQLConn -> ByteString -> IO OK
execute conn qry = command conn (COM_QUERY qry)

{-
executeBatch :: MySQL -> OutputStream ByteString -> IO (InputStream OK)
executeBatch
-}

-- | Execute a MySQL query which return a resultSet.
--
query :: MySQLConn -> ByteString -> IO ([ColumnDef], InputStream [MySQLValue])
query conn@(MySQLConn is os _ consumed) qry = do
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
            p <- readPacket is
            if  | isEOF p  -> writeIORef consumed True >> return Nothing
                | isERR p  -> decodeFromPacket p >>=throwIO . ERRException
                | otherwise -> Just <$> getFromPacket (getTextRow fields) p
        return (fields, rows)

-- | Ask MySQL to prepare a query statement.
--
prepareStmt :: MySQLConn -> ByteString -> IO StmtID
prepareStmt conn@(MySQLConn is os _ _) stmt = do
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

-- | Ask MySQL to closed a query statement.
--
closeStmt :: MySQLConn -> StmtID -> IO ()
closeStmt (MySQLConn _ os _ _) stid = do
    writeCommand (COM_STMT_CLOSE stid) os

-- | Ask MySQL to reset a query statement, all previous resultset will be cleared.
--
resetStmt :: MySQLConn -> StmtID -> IO ()
resetStmt (MySQLConn is os _ consumed) stid = do
    writeCommand (COM_STMT_RESET stid) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else writeIORef consumed True

-- | Execute prepared query statement with parameters, expecting no resultset.
--
executeStmt :: MySQLConn -> StmtID -> [MySQLValue] -> IO OK
executeStmt conn@(MySQLConn is os _ consumed) stid params = do
    guardUnconsumed conn
    writeCommand (COM_STMT_EXECUTE stid params) os
    p <- readPacket is
    if  | isERR p -> decodeFromPacket p >>= throwIO . ERRException
        | isOK  p ->  decodeFromPacket p
        | otherwise -> throwIO (UnexpectedPacket p)

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
            p <- readPacket is
            if  | isEOF p  -> writeIORef consumed True >> return Nothing
                | isERR p  -> decodeFromPacket p >>=throwIO . ERRException
                | isOK  p  -> Just <$> getFromPacket (getBinaryRow fields len) p
                | otherwise -> throwIO (UnexpectedPacket p)
        return (fields, rows)

