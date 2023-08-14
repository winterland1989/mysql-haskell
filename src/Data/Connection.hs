module Data.Connection where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy.Internal as L
import qualified System.IO.Streams         as S

-- | A simple connection abstraction.
--
-- 'Connection' s from this package are supposed to have following properties:
--
--  * 'S.InputStream' is choosen to simplify streaming processing.
--  You can easily push back some data with 'S.unRead',
--  reading 'S.InputStream' will block until GHC IO manager find data is ready, for example:
--  @'S.readExactly' 1024@ will block until at least 1024 bytes are available.
--
--  * The type @'L.ByteString' -> 'IO' ()@ is choosen because it worked well with haskell's builder infrastructure.
--  <http://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket-ByteString.html#g:2 vector-IO>
--  is used automatically when there's more than one chunk to send to save system call.
--
--  * 'connExtraInfo' field store extra data about the connection, 'N.SockAddr' for example.
--  You can also use this field as a type tag to distinguish different type of connection.
--
--  * 'close' should close connection resource, thus the 'Connection' shouldn't be used anymore
--  after 'close' is called.
--
--  * You should make sure there's no pending recv/send before you 'close' a 'Connection'.
--  That means either you call 'close' in the same thread you recv/send, or use async exception
--  to terminate recv/send thread before call 'close' in other thread(such as a reaper thread).
--  Otherwise you may run into
--  <https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html race-connections>.
--
--  * Exception or closed by other peer during recv/send will NOT close underline socket,
--  you should always use 'close' with 'E.bracket' to ensure safety.
--
--  @since 1.0
--
data Connection a = Connection
    { source        :: {-# UNPACK #-} !(S.InputStream B.ByteString)   -- ^ receive data as 'S.InputStream'
    , send          :: L.ByteString -> IO ()                          -- ^ send data with connection
    , close         :: IO ()                                          -- ^ close connection
    , connExtraInfo :: a                                              -- ^ extra info
    }
