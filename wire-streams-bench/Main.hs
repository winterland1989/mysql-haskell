{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-------------------------------------------------------------------------------

import           Control.Exception        (evaluate)
import           Control.Monad            (replicateM_)
import           Control.Monad.IO.Class
import           Criterion.Main
import           Data.Binary              (Binary)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit
import qualified Data.Conduit.Binary      as Conduit
import qualified Data.Conduit.Cereal      as Conduit
import           Data.Serialize           (Serialize)
import           Data.Serialize           (Get, get, put, runPutLazy)
import           GHC.Generics

-------------------------------------------------------------------------------

import qualified System.IO.Streams        as Streams
import qualified System.IO.Streams.Binary as Binary
import qualified System.IO.Streams.Cereal as Cereal

-------------------------------------------------------------------------------

main :: IO ()
main = do
  let lstring = BL.concat $ map (runPutLazy . put) foos
      foos = map exFoo [0..1000]
      exFoo x = Foo x "oh look, a Foo!"
  defaultMain
    [ bgroup "decode one element wire-streams/cereal" [
         bench "1000 items" $ whnfIO $ benchCS lstring ]
    , bgroup "decode one element wire-streams/binary" [
         bench "1000 items" $ whnfIO $ benchBS lstring ]
    , bgroup "decode one element cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCC lstring ]
    , bgroup "decode 1000 elements from wire-streams/cereal" [
         bench "1000 items" $ whnfIO $ benchCSA lstring ]
    , bgroup "decode 1000 elements from wire-streams/binary" [
         bench "1000 items" $ whnfIO $ benchBSA lstring ]
    , bgroup "decode 1000 elements cereal-conduit" [
         bench "1000 items" $ whnfIO $ benchCCA lstring ]
    ]

benchCS lstring = do
    s <- Cereal.decodeInputStream =<< Streams.fromLazyByteString lstring
    a <- Streams.read s :: IO (Maybe Foo)
    evaluate a

benchBS lstring = do
    s <- Binary.decodeInputStream =<< Streams.fromLazyByteString lstring
    a <- Streams.read s :: IO (Maybe Foo)
    evaluate a

benchCC lstring = do
    Conduit.sourceLbs lstring =$= Conduit.conduitGet2 (get :: Get Foo) $$ do
        a <- await
        liftIO (evaluate a)

benchCSA lstring = do
    s <- Cereal.decodeInputStream =<< Streams.fromLazyByteString lstring
    replicateM_ 1000 $ do
        a <- Streams.read s :: IO (Maybe Foo)
        evaluate a

benchBSA lstring = do
    s <- Binary.decodeInputStream =<< Streams.fromLazyByteString lstring
    replicateM_ 1000 $ do
        a <- Streams.read s :: IO (Maybe Foo)
        evaluate a

benchCCA lstring = do
    Conduit.sourceLbs lstring =$= Conduit.conduitGet2 (get :: Get Foo) $$
        replicateM_ 1000 $ do
            a <- await
            liftIO (evaluate a)

-------------------------------------------------------------------------------

data Foo = Foo Int ByteString deriving (Generic, Show, Eq)

instance Serialize Foo
instance Binary    Foo
