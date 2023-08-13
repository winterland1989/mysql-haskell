{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Sytem.IO.Streams.Cereal
-- Copyright   :  Soostone Inc, Winterland
-- License     :  BSD3
--
-- Maintainer  :  Winterland
-- Stability   :  experimental
--
-- Use cereal to encode/decode io-streams.
----------------------------------------------------------------------------

module System.IO.Streams.Cereal (
    -- * single element encode/decode
      getFromStream
    , decodeFromStream
    , putToStream
    -- * 'InputStream' encode/decode
    , getInputStream
    , decodeInputStream
    -- * 'OutputStream' encode
    , putOutputStream
    , encodeOutputStream
    -- * exception type
    , DecodeException(..)
    ) where

-------------------------------------------------------------------------------

import           Control.Exception      (Exception, throwIO)
import           Control.Monad          (unless)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as S
import           Data.Serialize
import           Data.Typeable
import qualified System.IO.Streams      as Streams
import           System.IO.Streams.Core

-------------------------------------------------------------------------------

-- | An Exception raised when cereal decoding fails.
data DecodeException = DecodeException String
  deriving (Typeable)

instance Show DecodeException where
    show (DecodeException s) = "System.IO.Streams.Cereal: cereal decode exception: " ++ s

instance Exception DecodeException

-------------------------------------------------------------------------------

-- | write a instance of 'Serialize' to an 'OutputStream'
--
putToStream :: Serialize a => Maybe a -> OutputStream ByteString -> IO ()
putToStream Nothing  = Streams.write Nothing
putToStream (Just a) = (Streams.writeLazyByteString . runPutLazy . put) a
{-# INLINE putToStream #-}

-------------------------------------------------------------------------------

-- | Take a 'Get' and an 'InputStream' and decode a
-- value. Consumes only as much input as necessary to decode the
-- value. Unconsumed input will be unread. If there is
-- an error while deserializing, a 'DecodeException' is thrown, and
-- unconsumed part will be unread. To simplify upstream generation,
-- all empty 'ByteString' will be filtered out and not passed to cereal,
-- only EOFs/Nothing will close a cereal decoder.
--
-- Examples:
--
-- >>> import qualified System.IO.Streams as Streams
-- >>> getFromStream (get :: Get String) =<< Streams.fromByteString (Data.ByteString.drop 1 $ runPut $ put "encode me")
-- *** Exception: System.IO.Streams.Cereal: cereal decode exception: too few bytes
-- From:	demandInput
-- <BLANKLINE>
--
getFromStream :: Get a -> InputStream ByteString -> IO (Maybe a)
getFromStream g is =
    Streams.read is >>= maybe (return Nothing) (go . runGetPartial g)
  where
    go (Fail msg s) = do
        unless (S.null s) (Streams.unRead s is)
        throwIO (DecodeException msg)
    go (Done r s) = do
         unless (S.null s) (Streams.unRead s is)
         return (Just r)
    go c@(Partial cont) =
        Streams.read is >>= maybe (go (cont S.empty))   -- use 'empty' to notify cereal ending.
        (\ s -> if S.null s then go c else go (cont s))
{-# INLINE getFromStream #-}

-- | typeclass version of 'getFromStream'
decodeFromStream :: Serialize a => InputStream ByteString -> IO (Maybe a)
decodeFromStream = getFromStream get
{-# INLINE decodeFromStream #-}

-------------------------------------------------------------------------------

-- | Convert a stream of individual encoded 'ByteString's to a stream
-- of Results. Throws a 'DecodeException' on error.
getInputStream :: Get a -> InputStream ByteString -> IO (InputStream a)
getInputStream g is = makeInputStream (getFromStream g is)
{-# INLINE getInputStream #-}

-- | typeclass version of 'getInputStream'
decodeInputStream :: Serialize a => InputStream ByteString -> IO (InputStream a)
decodeInputStream = getInputStream get
{-# INLINE decodeInputStream #-}

-------------------------------------------------------------------------------

-- | create an 'OutputStream' of serializable values from an 'OutputStream'
-- of bytestrings with a 'Putter'.
putOutputStream :: Putter a -> OutputStream ByteString -> IO (OutputStream a)
putOutputStream p os = Streams.makeOutputStream $ \ ma ->
    case ma of Nothing -> Streams.write Nothing os
               Just a  -> Streams.writeLazyByteString (runPutLazy (p a)) os
{-# INLINE putOutputStream #-}

-- | typeclass version of 'putOutputStream'
encodeOutputStream :: Serialize a => OutputStream ByteString -> IO (OutputStream a)
encodeOutputStream = putOutputStream put
{-# INLINE encodeOutputStream #-}
