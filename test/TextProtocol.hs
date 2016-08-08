module TextProtocol where

import Database.MySQL.Base
import Database.MySQL.Protocol
import qualified System.IO.Streams as Stream
import Test.HUnit

tests :: MySQLConn -> Test
tests c = TestCase $ do
    execute_ c "UPDATE test SET _bit = 0b'11100000' WHERE _id=0;"
    ([f], is) <- query_ c "SELECT _bit FROM test;"
    assertEqual "MySQL_TYPE_BIT" (columnType f) MYSQL_TYPE_BIT
    Just v <- Stream.read is
    assertEqual "MySQLBit" v [MySQLBit 0b11100000]
    Stream.skipToEof is

    return ()

