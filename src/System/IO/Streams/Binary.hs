{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sytem.IO.Streams.Binary
-- Copyright   :  Petter Bergman, Winterland
-- License     :  BSD3
--
-- Maintainer  :  Winterland
-- Stability   :  experimental
--
-- Use binary to encode/decode io-streams.
--------------------------------------------------------------------------------

module System.IO.Streams.Binary (
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

--------------------------------------------------------------------------------

import           Control.Exception            (Exception, throwIO)
import           Control.Monad                (unless)
import           Data.Binary                  (Binary, get, put)
import qualified Data.Binary.Parser           as P
import           Data.Binary.Get              (ByteOffset, Decoder(..), Get)
import           Data.Binary.Put              (runPut, Put)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import           Data.Typeable                (Typeable)
import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.ByteString (writeLazyByteString)

--------------------------------------------------------------------------------

-- | An Exception raised when binary decoding fails.
--
-- it contains offset information where cereal don't.
data DecodeException = DecodeException ByteString ByteOffset String
  deriving (Typeable)

instance Show DecodeException where
  show (DecodeException buf offset message) =
        "DecodeException\nbuf:" ++ show buf ++ "\noffset:" ++ show offset ++ "\nmessage:" ++ show message

instance Exception DecodeException

--------------------------------------------------------------------------------

-- | Write an instance of 'Binary' to an 'OutputStream'.
putToStream :: Binary a => Maybe a -> OutputStream ByteString -> IO ()
putToStream Nothing  os = Streams.write Nothing os
putToStream (Just x) os = writeLazyByteString ((runPut . put) x) os
{-# INLINE putToStream #-}

--------------------------------------------------------------------------------

-- | Take a 'Get' and an 'InputStream' and decode a
-- value. Consumes only as much input as necessary to decode the
-- value. Unconsumed input will be unread. If there is
-- an error while deserializing, a 'DecodeException' is thrown, and
-- unconsumed part will be unread. binary decoder use 'Nothing'
-- to indicate input end, so EOFs/Nothing will close a binary decoder.
-- Examples:
--
-- >>> import qualified System.IO.Streams as Streams
-- >>> getFromStream (get :: Get String) =<< Streams.fromLazyByteString (Data.ByteString.Lazy.drop 1 $ runPut $ put "encode me")
-- *** Exception: System.IO.Streams.Binary: binary decode exception: offset 16, "not enough bytes"
--
getFromStream :: Get a -> InputStream ByteString -> IO (Maybe a)
getFromStream g is = Streams.read is >>= maybe (return Nothing) (go . P.parse g)
  where go (Fail s offset message) = do
            unless (S.null s) (Streams.unRead s is)
            throwIO $ DecodeException s offset message
        go (Done s _ x) = do
            unless (S.null s) (Streams.unRead s is)
            return (Just x)
        go (Partial p) = Streams.read is >>= go .  p
{-# INLINE getFromStream #-}

-- | typeclass version of 'getFromStream'
decodeFromStream :: Binary a => InputStream ByteString -> IO (Maybe a)
decodeFromStream = getFromStream get
{-# INLINE decodeFromStream #-}

--------------------------------------------------------------------------------

-- | Convert a stream of individual encoded 'ByteString's to a stream
-- of Results. Throws a 'DecodeException' on error.
getInputStream :: Get a -> InputStream ByteString -> IO (InputStream a)
getInputStream g = Streams.makeInputStream . getFromStream g
{-# INLINE getInputStream #-}

-- | typeclass version of 'getInputStream'
decodeInputStream :: Binary a => InputStream ByteString -> IO (InputStream a)
decodeInputStream = Streams.makeInputStream . decodeFromStream
{-# INLINE decodeInputStream #-}

--------------------------------------------------------------------------------

-- | create an 'OutputStream' of serializable values from an 'OutputStream'
-- of bytestrings with a 'Putter'.
putOutputStream :: (a -> Put) -> OutputStream ByteString -> IO (OutputStream a)
putOutputStream p os = Streams.makeOutputStream $ \ ma ->
    case ma of Nothing -> Streams.write Nothing os
               Just a -> writeLazyByteString (runPut (p a)) os
{-# INLINE putOutputStream #-}

-- | typeclass version of 'putOutputStream'
encodeOutputStream :: Binary a => OutputStream ByteString -> IO (OutputStream a)
encodeOutputStream = putOutputStream put
{-# INLINE encodeOutputStream #-}
