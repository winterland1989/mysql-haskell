-- | The calls the integration tests read rows with. Every suite that reads
-- rows takes a 'QueryApi' and runs once for each of 'queryApis', so the old
-- 'MySQLValue' functions and each path of "Database.MySQL.Decoder" go through
-- the very same cases, and a test differs between them only in the API.
module QueryApi
    ( QueryApi (..)
    , ColumnCount (..)
    , queryApis
    ) where

import           Control.Exception                  (throwIO)
import           Control.Monad                      (replicateM)
import qualified Data.Vector                        as V
import           Database.MySQL.Base
import qualified Database.MySQL.Decoder             as Decode
import           System.IO.Streams                  (InputStream)
import           TypedValue                         (typedDecoder)
import qualified System.IO.Streams                  as Stream

-- | How many columns a query returns. Only the 'Decode.RowDecoder' path uses
-- it: a row decoder states its columns before the query runs, where the other
-- paths learn them from the result set.
newtype ColumnCount = ColumnCount Int

data QueryApi = QueryApi
    { apiName            :: String
    , apiQuery_          :: ColumnCount -> MySQLConn -> Query
                         -> IO ([ColumnDef], InputStream [MySQLValue])
    , apiQuery           :: ColumnCount -> MySQLConn -> Query -> [Param]
                         -> IO ([ColumnDef], InputStream [MySQLValue])
    , apiQueryVector_    :: ColumnCount -> MySQLConn -> Query
                         -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
    , apiQueryStmt       :: ColumnCount -> MySQLConn -> StmtID -> [MySQLValue]
                         -> IO ([ColumnDef], InputStream [MySQLValue])
    , apiQueryStmtVector :: ColumnCount -> MySQLConn -> StmtID -> [MySQLValue]
                         -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
    }

queryApis :: [QueryApi]
queryApis = [valueApi, rowDecoderApi, rawRowApi, typedRawRowApi]

-- | The API the tests were written for.
valueApi :: QueryApi
valueApi = QueryApi
    { apiName            = "MySQLValue API"
    , apiQuery_          = const query_
    , apiQuery           = const query
    , apiQueryVector_    = const queryVector_
    , apiQueryStmt       = const queryStmt
    , apiQueryStmtVector = const queryStmtVector
    }

-- | 'queryRows_', 'queryRows' and 'queryStmtRows' with 'Decode.mysqlValue' for
-- every column.
rowDecoderApi :: QueryApi
rowDecoderApi = QueryApi
    { apiName            = "RowDecoder API"
    , apiQuery_          = queryRows_ . everyColumn
    , apiQuery           = queryRows . everyColumn
    , apiQueryVector_    = rowDecoderQueryVector_
    , apiQueryStmt       = queryStmtRows . everyColumn
    , apiQueryStmtVector = rowDecoderQueryStmtVector
    }

everyColumn :: ColumnCount -> Decode.RowDecoder [MySQLValue]
everyColumn (ColumnCount count) = replicateM count (Decode.field Decode.mysqlValue)

rowDecoderQueryVector_ :: ColumnCount -> MySQLConn -> Query
                       -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
rowDecoderQueryVector_ count conn qry = toVectors =<< queryRows_ (everyColumn count) conn qry

rowDecoderQueryStmtVector :: ColumnCount -> MySQLConn -> StmtID -> [MySQLValue]
                          -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
rowDecoderQueryStmtVector count conn stmt params =
    toVectors =<< queryStmtRows (everyColumn count) conn stmt params

-- | 'queryRawRows_' and 'queryStmtRawRows', each column read by its number with
-- 'Decode.mysqlValue', as a library decoding by column number would.
rawRowApi :: QueryApi
rawRowApi = rawRowApiWith "RawRow API" valueDecoder

-- | 'queryRawRows_' and 'queryStmtRawRows', each column read with the typed
-- decoder for its type, the value put back into the 'MySQLValue' constructor
-- the old API returns, so the same expectations hold.
typedRawRowApi :: QueryApi
typedRawRowApi = rawRowApiWith "typed decoders" typedDecoder

rawRowApiWith :: String -> (ColumnKind -> Decode.FieldDecoder MySQLValue) -> QueryApi
rawRowApiWith name decoderFor = QueryApi
    { apiName            = name
    , apiQuery_          = \_ conn qry -> rawValues decoderFor =<< queryRawRows_ conn qry
    , apiQuery           = \_ conn qry params ->
        rawValues decoderFor =<< queryRawRows_ conn (renderParams qry params)
    , apiQueryVector_    = \_ conn qry -> toVectors =<< rawValues decoderFor =<< queryRawRows_ conn qry
    , apiQueryStmt       = \_ conn stmt params -> rawValues decoderFor =<< queryStmtRawRows conn stmt params
    , apiQueryStmtVector = \_ conn stmt params ->
        toVectors =<< rawValues decoderFor =<< queryStmtRawRows conn stmt params
    }

valueDecoder :: ColumnKind -> Decode.FieldDecoder MySQLValue
valueDecoder _ = Decode.mysqlValue

-- | Each raw row as the list of its columns' values, every column with the
-- parser 'prepareColumn' picks for it.
rawValues :: Decode.RowProtocol protocol
          => (ColumnKind -> Decode.FieldDecoder MySQLValue)
          -> ([ColumnDef], InputStream (Decode.RawRow protocol))
          -> IO ([ColumnDef], InputStream [MySQLValue])
rawValues decoderFor (columns, rows) = do
    parsers <- either throwIO pure
        (traverse (prepareColumn decoderFor) (zip (map Decode.ColumnNumber [0 ..]) columns))
    (,) columns <$> Stream.mapM (rowValues parsers) rows

prepareColumn :: Decode.RowProtocol protocol
              => (ColumnKind -> Decode.FieldDecoder MySQLValue) -> (Decode.ColumnNumber, ColumnDef)
              -> Either Decode.ColumnMismatch (Decode.ColumnNumber, Decode.FieldParser protocol MySQLValue)
prepareColumn decoderFor (column, definition) =
    (,) column <$> Decode.prepareFieldParser (decoderFor (columnKind definition)) column definition

rowValues :: [(Decode.ColumnNumber, Decode.FieldParser protocol MySQLValue)] -> Decode.RawRow protocol
          -> IO [MySQLValue]
rowValues parsers row = traverse (fieldValue row) parsers

fieldValue :: Decode.RawRow protocol -> (Decode.ColumnNumber, Decode.FieldParser protocol MySQLValue)
           -> IO MySQLValue
fieldValue row (column, parser) = Decode.runFieldParser parser row column throwIO pure

toVectors :: ([ColumnDef], InputStream [MySQLValue])
          -> IO (V.Vector ColumnDef, InputStream (V.Vector MySQLValue))
toVectors (columns, rows) = (,) (V.fromList columns) <$> Stream.map V.fromList rows
