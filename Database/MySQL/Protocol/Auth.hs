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
import           Database.MySQL.Protocol.Packet

--------------------------------------------------------------------------------
-- Authentications

data Greeting = Greeting
    { greetingProtocol :: !Word8
    , greetingVersion  :: !B.ByteString
    , greetingTid      :: !Word32
    , greetingSalt1    :: !B.ByteString
    , greetingCaps     :: !Word16
    , greetingLang     :: !Word8
    , greetingStatus   :: !Word16
    , greetingSalt2    :: !B.ByteString
    } deriving (Show, Eq)

putGreeting :: Greeting -> Put
putGreeting (Greeting p v t s1 c l st s2) = do
    putWord8 p
    putByteString v
    putWord8 0x00
    putWord32le t
    putByteString s1
    putWord16le c
    putWord8 l
    putWord16le st
    replicateM_ 13 (putWord8 0x00)
    putByteString s2

getGreeting :: Get Greeting
getGreeting = Greeting
    <$> getWord8
    <*> getByteStringNul
    <*> getWord32le
    <*> getByteStringNul
    <*> getWord16le
    <*> getWord8
    <*> getWord16le
    <*  skip 13
    <*> getByteStringNul
    <*  getByteStringNul

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
    replicateM_ 23 (putWord8 0x00)
    putByteString n >> putWord8 0x00
    putWord8 $ fromIntegral (B.length p)
    putByteString p
    putByteString s
    putWord8 0x00

instance Binary Auth where
    get = getAuth
    put = putAuth
