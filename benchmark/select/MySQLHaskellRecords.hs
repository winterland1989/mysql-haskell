{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Each row decoded into a record, the way an application wants it: either
-- through 'MySQLValue' and "Database.MySQL.Field" (@field@, @field-stmt@), or
-- straight from the row's bytes with a 'Decode.RowDecoder' (@typed@,
-- @typed-stmt@). The @-stmt@ modes use a prepared statement.
module Main where

import           Control.Concurrent.Async
import           Control.Exception        (throwIO)
import           Control.Monad
import           Data.Int                 (Int32)
import           Data.Scientific          (Scientific)
import           Data.Text                (Text)
import           Data.Time.Calendar       (Day)
import           Data.Time.LocalTime      (LocalTime)
import           Database.MySQL.Base
import qualified Database.MySQL.Decoder   as Decode
import           Database.MySQL.Field
import           System.Environment
import           System.IO.Streams        (InputStream, fold)
import qualified System.IO.Streams        as Stream

data Employee = Employee !Int32 !Day !Text !Text !Text !Day

data Mixed = Mixed !Int32 !Scientific !LocalTime !Double !Text

main :: IO ()
main = do
    args <- getArgs
    case args of
        [threadNum, "employees", mode] -> go (read threadNum) mode employeeQuery employeeDecoder employeeFromValues
        [threadNum, "mixed_types", mode] -> go (read threadNum) mode mixedQuery mixedDecoder mixedFromValues
        _ -> putStrLn "Usage: THREADS employees|mixed_types field|typed|field-stmt|typed-stmt"

employeeQuery :: Query
employeeQuery = "SELECT * FROM employees"

mixedQuery :: Query
mixedQuery = "SELECT * FROM mixed_types"

employeeDecoder :: Decode.RowDecoder Employee
employeeDecoder = Employee
    <$> Decode.field Decode.int32
    <*> Decode.field Decode.day
    <*> Decode.field Decode.text
    <*> Decode.field Decode.text
    <*> Decode.field Decode.text
    <*> Decode.field Decode.day

employeeFromValues :: [MySQLValue] -> Either DecodeError Employee
employeeFromValues values = case values of
    [number, born, first, lastName, gender, hired] -> Employee
        <$> decodeInt32 number <*> decodeDay born <*> decodeText first
        <*> decodeText lastName <*> decodeText gender <*> decodeDay hired
    _ -> Left (DecodeTypeMismatch (ExpectedTypeName "6 columns") MySQLNull)

mixedDecoder :: Decode.RowDecoder Mixed
mixedDecoder = Mixed
    <$> Decode.field Decode.int32
    <*> Decode.field Decode.scientific
    <*> Decode.field Decode.localTime
    <*> Decode.field Decode.double
    <*> Decode.field Decode.text

mixedFromValues :: [MySQLValue] -> Either DecodeError Mixed
mixedFromValues values = case values of
    [number, amount, created, ratio, body] -> Mixed
        <$> decodeInt32 number <*> decodeScientific amount <*> decodeLocalTime created
        <*> decodeDouble ratio <*> decodeText body
    _ -> Left (DecodeTypeMismatch (ExpectedTypeName "5 columns") MySQLNull)

go :: Int -> String -> Query -> Decode.RowDecoder row -> ([MySQLValue] -> Either DecodeError row) -> IO ()
go n mode qry decoder fromValues = void . flip mapConcurrently [1..n] $ \ _ -> do
    c <- connect defaultConnectInfo { ciUser = "testMySQLHaskell"
                                    , ciDatabase = "testMySQLHaskell"
                                    }
    rowCount <- case mode of
        "field" -> query_ c qry >>= countConverted fromValues . snd
        "typed" -> queryRows_ decoder c qry >>= countRecords . snd
        "field-stmt" -> do
            stmt <- prepareStmt c qry
            queryStmt c stmt [] >>= countConverted fromValues . snd
        "typed-stmt" -> do
            stmt <- prepareStmt c qry
            queryStmtRows decoder c stmt [] >>= countRecords . snd
        _ -> fail ("unknown mode " ++ mode)
    putStr "numbers of rows: "
    print rowCount

-- | The records' fields are strict, so a record in weak head normal form is
-- fully decoded.
countRecords :: InputStream row -> IO Int
countRecords = fold (\count row -> row `seq` count + 1) 0

countConverted :: ([MySQLValue] -> Either DecodeError row) -> InputStream [MySQLValue] -> IO Int
countConverted fromValues values =
    Stream.mapM (either throwIO pure . fromValues) values >>= countRecords
