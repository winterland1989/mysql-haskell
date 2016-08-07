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
    , execute_
    , query_
    , query
    , Query(..)
    , renderParams
      -- * prepared query statement
    , prepareStmt
    , executeStmt
    , queryStmt
    , closeStmt
    , resetStmt
      -- * MySQL datatype
    , MySQLValue(..)
      -- * helpers
    , command
    ) where

import           Control.Arrow             (first)
import           Control.Exception         (Exception, throw, throwIO)
import           Control.Monad
import           Data.Binary.Put
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as L
import           Data.IORef                (writeIORef)
import           Data.String               (IsString (..))
import           Data.Typeable
import           Database.MySQL.Connection
import           Database.MySQL.Protocol
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream

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
executeBatch :: MySQL -> OutputStream ByteString -> IO (InputStream OK)
executeBatch
-}

-- | Execute a MySQL query which return a resultSet with parameters.
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
            p' <- readPacket is
            if  | isEOF p'  -> writeIORef consumed True >> return Nothing
                | isERR p'  -> decodeFromPacket p' >>=throwIO . ERRException
                | otherwise -> Just <$> getFromPacket (getTextRow fields) p'
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
executeStmt conn@(MySQLConn is os _ _) stid params = do
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
            q <- readPacket is
            if  | isEOF q  -> writeIORef consumed True >> return Nothing
                | isERR q  -> decodeFromPacket q >>=throwIO . ERRException
                | isOK  q  -> Just <$> getFromPacket (getBinaryRow fields len) q
                | otherwise -> throwIO (UnexpectedPacket q)
        return (fields, rows)


-- | Query string type borrowed from @mysql-simple@.
--
-- This type is intended to make it difficult to
-- construct a SQL query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce SQL injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- The underlying type is a 'ByteString', and literal Haskell strings
-- that contain Unicode characters will be correctly transformed to
-- UTF-8.
--
newtype Query = Query { fromQuery :: ByteString } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . L.toStrict . BB.toLazyByteString . BB.stringUtf8

instance Monoid Query where
    mempty = Query B.empty
    mappend (Query a) (Query b) = Query (B.append a b)
    {-# INLINE mappend #-}

renderParams :: Query -> [MySQLValue] -> Query
renderParams (Query qry) params =
    let fragments = BC.split '?' qry
    in Query . L.toStrict . runPut $ merge fragments params
  where
    merge (x:[])  []    = putByteString x
    merge (x:xs) (y:ys) = putByteString x >> putTextField y >> merge xs ys
    merge _     _       = throw WrongParamsCount

data WrongParamsCount = WrongParamsCount deriving (Show, Typeable)
instance Exception WrongParamsCount
