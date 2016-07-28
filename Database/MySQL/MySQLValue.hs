module Database.MySQL.MySQLValue where

import Control.Monad
import qualified Data.ByteString.Lex.Integral   as LexInt
import qualified Data.ByteString.Lex.Fractional as LexFrac
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)
import Data.Time.Format (parseTime, defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime, TimeOfDay, makeTimeOfDayValid)
import Data.Binary.Get
import Database.MySQL.Protocol (Field(..), FieldType(..), getLenEncBytes)

data MySQLValue
    = MySQLFixedPoint    !Rational     -- ^ DECIMAL, NEWDECIMAL
    | MySQLInt           !Int          -- ^ TINY, SHORT, LONG, INT24, YEAR
    | MySQLInt64         !Int64
    | MySQLDouble        !Double       -- ^ IEEE 754 double precision format
    | MySQLDateTime      !LocalTime
    | MySQLDate          !Day
    | MySQLTime          !TimeOfDay
    | MySQLBytes         !ByteString
    | MySQLText          !Text
    | MySQLNull
  deriving (Show, Eq)

getTextField :: Field -> ByteString -> Get MySQLValue
getTextField f bs
    | t == MYSQL_TYPE_NULL              = MySQLNull
    | t == MYSQL_TYPE_DECIMAL
        || t == MYSQL_TYPE_NEWDECIMAL   = MySQLFixedPoint (fracLexer bs)
    | t == MYSQL_TYPE_TINY
        || t == MYSQL_TYPE_SHORT
        || t == MYSQL_TYPE_LONG
        || t == MYSQL_TYPE_INT24
        || t == MYSQL_TYPE_YEAR         = MySQLInt (intLexer bs)
    | t == MYSQL_TYPE_LONGLONG          = MySQLInt64 (intLexer bs)
    | t == MYSQL_TYPE_FLOAT
        || t == MYSQL_TYPE_DOUBLE            = MySQLDouble (fracLexer bs)
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_DATETIME
        || t == MYSQL_TYPE_TIMESTAMP2
        || t == MYSQL_TYPE_DATETIME2    = case parseTime defaultTimeLocale "%F %T" (BC.unpack bs) of
                  Just t -> MySQLDateTime t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = case parseTime defaultTimeLocale "%F" (BC.unpack bs) of
                  Just t -> MySQLDate t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_TIME
        || t == MYSQL_TYPE_TIME2        = case parseTime defaultTimeLocale "%T" (BC.unpack bs) of
                  Just t -> MySQLTime t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_GEOMETRY          = MySQLBytes bs
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_BIT
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = if isText then MySQLText (T.decodeUtf8 bs) else MySQLBytes bs

  where
    t = fieldType f
    isText = fieldCharSet f /= 63
    intLexer bs = maybe 0 id (fst <$> LexInt.readSigned LexInt.readDecimal bs)
    fracLexer bs = maybe 0 id (fst <$> LexFrac.readSigned LexFrac.readDecimal bs)

getTextRow :: [Field] -> Get [MySQLValue]
getTextRow fs = forM fs $ \ f -> do
    p <- lookAhead getWord8
    if p == 0x79
    then getWord8 >> return MySQLNull
    else do
        bs <- getLenEncBytes
        getTextField f bs

getFieldBinary :: Field -> ByteString -> Get MySQLValue
getFieldBinary f bs
    | t == MYSQL_TYPE_NULL              = MySQLNull
    | t == MYSQL_TYPE_DECIMAL           =
    | t == MYSQL_TYPE_NEWDECIMAL   = MySQLFixedPoint (fracLexer bs)
    | t == MYSQL_TYPE_TINY
        || t == MYSQL_TYPE_SHORT
        || t == MYSQL_TYPE_LONG
        || t == MYSQL_TYPE_INT24
        || t == MYSQL_TYPE_YEAR         = MySQLInt (intLexer bs)
    | t == MYSQL_TYPE_LONGLONG          = MySQLInt64 (intLexer bs)
    | t == MYSQL_TYPE_FLOAT
        || t == MYSQL_TYPE_DOUBLE            = MySQLDouble (fracLexer bs)
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_DATETIME
        || t == MYSQL_TYPE_TIMESTAMP2
        || t == MYSQL_TYPE_DATETIME2    = case parseTime defaultTimeLocale "%F %T" (BC.unpack bs) of
                  Just t -> MySQLDateTime t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = case parseTime defaultTimeLocale "%F" (BC.unpack bs) of
                  Just t -> MySQLDate t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_TIME
        || t == MYSQL_TYPE_TIME2        = case parseTime defaultTimeLocale "%T" (BC.unpack bs) of
                  Just t -> MySQLTime t
                  Nothing -> MySQLNull
    | t == MYSQL_TYPE_GEOMETRY          = MySQLBytes bs
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_BIT
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = if isText then MySQLText (T.decodeUtf8 bs) else MySQLBytes bs
  where
    t = fieldType f
    isText = fieldCharSet f /= 63
    intLexer bs = maybe 0 id (fst <$> LexInt.readSigned LexInt.readDecimal bs)
    fracLexer bs = maybe 0 id (fst <$> LexFrac.readSigned LexFrac.readDecimal bs)
    -- please check https://github.com/jeremycole/mysql_binlog
    decimalLexer =


getBinaryRow :: [Field] -> Get [MySQLValue]
getBinaryRow fs = forM fs $ \ f -> do
    p <- lookAhead getWord8
    if p == 0x79
    then getWord8 >> return MySQLNull
    else do
        bs <- getLenEncBytes
        getTextField f bs
