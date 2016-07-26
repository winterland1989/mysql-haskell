{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BangPatterns          #-}

module Database.MySQL.Base where

import           Control.Exception        (Exception, bracketOnError, throw, throwIO)
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as L
import qualified Crypto.Hash              as Crypto
import qualified Data.ByteArray           as BA
import qualified Data.Bits                as Bit
import           Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word8)
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified System.IO.Streams.TCP    as TCP
import qualified Network.Socket as N
import           Network.Socket  (HostName, PortNumber)
import           Database.MySQL.Protocol
import qualified Data.Binary as Binary

--------------------------------------------------------------------------------

data MySQLConn = MySQLConn {
        mysqlRead         :: !(InputStream  Packet)
    ,   mysqlWrite        :: !(OutputStream Packet)
    ,   mysqlCloseSocket  :: !(IO ())
    ,   isConsumed        :: !(IORef Bool)
    }

data ConnInfo = ConnInfo
    { ciHost     :: HostName
    , ciPort     :: PortNumber
    , ciDatabase :: ByteString
    , ciUser     :: ByteString
    , ciPassword :: ByteString
    , ciTLSInfo  :: Maybe (FilePath, [FilePath], FilePath)
    }
    deriving Show

defaultConnectInfo :: ConnInfo
defaultConnectInfo = ConnInfo "127.0.0.1" 3306 "" "root" "" Nothing

--------------------------------------------------------------------------------

readPacket :: InputStream Packet -> IO Packet
readPacket is = Stream.read is >>= maybe (throwIO NetworkException) (\ p@(Packet len _ bs) ->
        if len < 16777215 then return p else go len [bs]
    )
  where
    go len acc = Stream.read is >>= maybe (throwIO NetworkException) (\ p@(Packet len' seqN bs) -> do
            let !len'' = len + len'
                acc' = bs:acc
            if len' < 16777215
            then return (Packet len'' seqN (L.concat . reverse $ acc'))
            else go len'' acc'
        )

writeCommand :: Command -> OutputStream Packet -> IO ()
writeCommand a = let bs = Binary.runPut (Binary.put a) in
    go (fromIntegral (L.length bs)) 0 bs
  where
    go len seqN bs =
        if len < 16777215
        then Stream.write (Just (Packet len seqN bs))
        else go (len - 16777215) (seqN + 1) (L.drop 16777215 bs)

connect :: ConnInfo -> IO MySQLConn
connect ci@(ConnInfo host port _ _ _ tls) =
    bracketOnError (TCP.connect host port)
       (\(_, _, sock) -> N.close sock) $ \ (is, os, sock) -> do
            is' <- Binary.decodeInputStream is
            os' <- Binary.encodeOutputStream os
            p <- readPacket is'
            greet <- decodeFromPacket p
            let auth = mkAuth ci greet
            Binary.putToStream (Just (encodeToPacket auth 1)) os
            p <- readPacket is'
            if isOK p
            then do
                seqN <- newIORef 0
                isConsumed <- newIORef True
                return (MySQLConn is' os' (N.close sock) isConsumed)
            else Stream.write Nothing os' >> decodeFromPacket p >>= throwIO . AuthException
  where
    mkAuth :: ConnInfo -> Greeting -> Auth
    mkAuth (ConnInfo _ _ db user pass _) greet =
        let salt = salt1 greet `B.append` salt2 greet
            scambleBuf = scramble salt pass
        in Auth clientCap clientMaxPacketSize clientCharset user scambleBuf db

    scramble :: ByteString -> ByteString -> ByteString
    scramble salt pass
        | B.null pass = B.empty
        | otherwise   = B.pack (B.zipWith Bit.xor sha1pass withSalt)
        where sha1pass = sha1 pass
              withSalt = sha1 (salt `B.append` sha1 sha1pass)

    sha1 :: ByteString -> ByteString
    sha1 = BA.convert . (Crypto.hash :: ByteString -> Crypto.Digest Crypto.SHA1)

close :: MySQLConn -> IO ()
close (MySQLConn _ os close _) = Stream.write Nothing os >> close

query_ :: MySQLConn -> ByteString -> IO OK
query_ conn qry = command conn (COM_QUERY qry)

ping :: MySQLConn -> IO OK
ping = flip command COM_PING

command :: MySQLConn -> Command -> IO OK
command conn@(MySQLConn is os _ isConsumed) cmd = do
    guardUnconsumed conn
    writeCommand cmd os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else decodeFromPacket p

-- [Result Set Header] we simpilify it here, omit extra info
-- [Field]
-- [EOF]
-- [Row Data]
-- [EOF]
query :: MySQLConn -> ByteString -> IO ([Field], InputStream TextRow)
query conn@(MySQLConn is os _ isConsumed) qry = do
    guardUnconsumed conn
    writeCommand (COM_QUERY qry) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do
        LenEncInt len <- decodeFromPacket p
        fields <- replicateM len $ (decodeFromPacket <=< readPacket) is
        _ <- readPacket is -- eof packet, we don't verify this though
        writeIORef isConsumed False
        rows <- Stream.makeInputStream $ do
            p@(Packet _ _ bs) <- readPacket is
            if  | isEOF p  -> writeIORef isConsumed True >> return Nothing
                | isERR p  -> decodeFromPacket p >>=throwIO . ERRException
                | otherwise -> (return (Just (TextRow bs)))
        return (fields, rows)

guardUnconsumed :: MySQLConn -> IO ()
guardUnconsumed conn@(MySQLConn _ _ _ isConsumed)= do
    consumed <- readIORef isConsumed
    unless consumed (throwIO UnconsumedResultSet)

{-

prepareStmt :: MySQLConn -> ByteString -> IO StmtId

executeStmt :: MySQLConn -> StmtId -> IO ([Field], InputStream BinaryRow)
executeStmt (MySQLConn is os _ seqN isConsumed) qry = do
    unless <$> readIORef isConsumed <*> throwIO ResultUnconsumed
    Stream.write qry os
    p <- Binary.decodeFromStream is
    if isERR p then throwIO ...
    else let LenEncInt len = decodeFromPacket p
        field <- decodeFromPacket p . Binary.decodeFromStream is

-}


