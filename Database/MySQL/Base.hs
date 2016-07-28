{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.MySQL.Base where

import           Control.Exception        (bracketOnError, throwIO)
import           Control.Monad
import qualified Crypto.Hash              as Crypto
import qualified Data.Binary              as Binary
import qualified Data.Binary.Put          as Binary
import qualified Data.Bits                as Bit
import qualified Data.ByteArray           as BA
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as L
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Database.MySQL.Protocol
import           Database.MySQL.MySQLValue
import           Network.Socket           (HostName, PortNumber)
import qualified Network.Socket           as N
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Stream
import qualified System.IO.Streams.Binary as Binary
import qualified System.IO.Streams.TCP    as TCP

--------------------------------------------------------------------------------

data MySQLConn = MySQLConn {
        mysqlRead        :: {-# UNPACK #-} !(InputStream  Packet)
    ,   mysqlWrite       :: {-# UNPACK #-} !(OutputStream Packet)
    ,   mysqlCloseSocket :: IO ()
    ,   isConsumed       :: {-# UNPACK #-} !(IORef Bool)
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
    go len acc = Stream.read is >>= maybe (throwIO NetworkException) (\ (Packet len' seqN bs) -> do
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
                consumed <- newIORef True
                return (MySQLConn is' os' (N.close sock) consumed)
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
close (MySQLConn _ os closeSocket _) = Stream.write Nothing os >> closeSocket

query_ :: MySQLConn -> ByteString -> IO OK
query_ conn qry = command conn (COM_QUERY qry)

ping :: MySQLConn -> IO OK
ping = flip command COM_PING

command :: MySQLConn -> Command -> IO OK
command conn@(MySQLConn is os _ _) cmd = do
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
query :: MySQLConn -> ByteString -> IO ([Field], InputStream [MySQLValue])
query conn@(MySQLConn is os _ consumed) qry = do
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
            p <- readPacket is
            if  | isEOF p  -> writeIORef consumed True >> return Nothing
                | isERR p  -> decodeFromPacket p >>=throwIO . ERRException
                | otherwise -> Just <$> getFromPacket (getTextRow fields) p
        return (fields, rows)

guardUnconsumed :: MySQLConn -> IO ()
guardUnconsumed (MySQLConn _ _ _ consumed) = do
    c <- readIORef consumed
    unless c (throwIO UnconsumedResultSet)


{-
prepareStmt :: MySQLConn -> ByteString -> IO StmtId
prepareStmt conn@(MySQLConn is os _ _) stmt = do
    guardUnconsumed conn
    writeCommand (COM_STMT_PREPARE stmt) os
    p <- readPacket is
    if isERR p
    then decodeFromPacket p >>= throwIO . ERRException
    else do




executeStmt :: MySQLConn -> StmtId -> IO ([Field], InputStream BinaryRow)
executeStmt (MySQLConn is os _ seqN isConsumed) qry = do
    unless <$> readIORef isConsumed <*> throwIO ResultUnconsumed
    Stream.write qry os
    p <- Binary.decodeFromStream is
    if isERR p then throwIO ...
    else let LenEncInt len = decodeFromPacket p
        field <- decodeFromPacket p . Binary.decodeFromStream is

-}

