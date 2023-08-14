{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,threadDelay,
                                                 takeMVar)
import qualified Control.Exception              as E
import qualified Network.Socket                 as N
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
------------------------------------------------------------------------------
import           Data.Connection
import qualified System.IO.Streams              as Stream
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
        conn <- TCP.connect "127.0.0.1" 8123
        send conn (L.fromChunks chunk)
        echo <- Stream.readExactly (1024 * 1024 * 1024) (source conn)
        print (B.length echo)
        putMVar resultMVar ()
        close conn

    server mvar = do
        sock <- TCP.bindAndListen 1024 8123
        putMVar mvar ()
        conn <- TCP.accept sock
        echo <- Stream.readExactly (1024 * 1024 * 1024) (source conn)
        send conn (L.fromStrict echo)
