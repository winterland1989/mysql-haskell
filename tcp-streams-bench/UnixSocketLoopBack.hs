{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,
                                                 takeMVar)
import           Control.Monad
import qualified Control.Exception              as E
import qualified Network.Socket                 as N
import           Data.Connection
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           System.Directory               (removeFile)
------------------------------------------------------------------------------
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.UnixSocket   as UnixSocket
import qualified System.IO.Streams.TCP          as TCP

main :: IO ()
main = N.withSocketsDo $ do
    portMVar   <- newEmptyMVar
    resultMVar <- newEmptyMVar
    forkIO $ server portMVar
    client portMVar resultMVar
    takeMVar resultMVar
  where
    chunk = replicate 1024 $ B.replicate (1024 * 1024) 64
    client mvar resultMVar = do
        _ <- takeMVar mvar
        conn <- UnixSocket.connect "./test.sock"
        send conn (L.fromChunks chunk)
        -- echo <- Stream.foldM (\ i s -> when (i `mod` 100 == 0) (print i) >> return (i+1)) 0 is
        echo <- Stream.readExactly (1024 * 1024 * 1024) (source conn)
        print (B.length echo)
        -- print echo
        putMVar resultMVar ()
        close conn

    server mvar = do
        E.try (removeFile "./test.sock") :: IO (Either E.IOException ())
        sock <- UnixSocket.bindAndListen 1024 "./test.sock"
        putMVar mvar ()
        conn <- TCP.accept sock
        req <- Stream.readExactly (1024 * 1024 * 1024) (source conn)
        send conn (L.fromStrict req)
