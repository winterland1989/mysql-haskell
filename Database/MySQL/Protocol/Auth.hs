{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Database.MySQL.Protocol.Auth where

import           Control.Monad
import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
-- Authentications

data Greeting = Greeting
    { protocol :: !Word8
    , version  :: !B.ByteString
    , tid      :: !Word32
    , salt1    :: !B.ByteString
    , caps     :: !Word16
    , lang     :: !Word8
    , status   :: !Word16
    , salt2    :: !B.ByteString
    } deriving (Show, Eq)

putGreeting :: Greeting -> Put
putGreeting (Greeting p v t s1 c l st s2) = do
    putWord8 p
    putByteString v
    putWord8 0
    putWord32le t
    putByteString s1
    putWord16le c
    putWord8 l
    putWord16le st
    replicateM_ 13 $ putWord8 0
    putByteString s2

getGreeting :: Get Greeting
getGreeting = do
    p  <- getWord8
    v  <- getLazyByteStringNul
    t  <- getWord32le
    s1 <- getLazyByteStringNul
    c  <- getWord16le
    l  <- getWord8
    st <- getWord16le
    skip 13
    s2 <- getLazyByteStringNul
    _ <- getLazyByteStringNul
    return $ Greeting p (L.toStrict v) t (L.toStrict s1) c l st $ L.toStrict s2

instance Binary Greeting where
    get = getGreeting
    put = putGreeting

data Auth = Auth
    { authCaps      :: !Word32
    , authMaxPacket :: !Word32
    , authCharset   :: !Word8
    , authName      :: !ByteString
    , authPassword  :: !ByteString
    , authSchema    :: !ByteString
    } deriving (Show, Eq)

getAuth :: Get Auth
getAuth = do
    a <- getWord32le
    m <- getWord32le
    c <- getWord8
    skip 23
    n <- getByteStringNul
    return $ Auth a m c n B.empty B.empty

putAuth :: Auth -> Put
putAuth (Auth cap m c n p s) = do
    putWord32le cap
    putWord32le m
    putWord8 c
    replicateM_ 23 (putWord8 0)
    putByteString n >> putWord8 0
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s
    putWord8 0

instance Binary Auth where
    get = getAuth
    put = putAuth
