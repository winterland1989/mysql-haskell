module Database.MySQL.Protocol.MySQLValue where

import Control.Monad
import qualified Data.ByteString.Lex.Integral   as LexInt
import qualified Data.ByteString.Lex.Fractional as LexFrac
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Bits as Bit
import Data.Bits ((.|.))
import Data.Fixed (Pico)
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Binary.Get
import Database.MySQL.Protocol.Field
import Database.MySQL.Protocol.Packet

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

getTextField :: Field -> Get MySQLValue
getTextField f
    | t == MYSQL_TYPE_NULL              = pure MySQLNull
    | t == MYSQL_TYPE_DECIMAL
        || t == MYSQL_TYPE_NEWDECIMAL   = MySQLFixedPoint . fracLexer <$> getLenEncBytes
    | t == MYSQL_TYPE_TINY
        || t == MYSQL_TYPE_SHORT
        || t == MYSQL_TYPE_LONG
        || t == MYSQL_TYPE_INT24
        || t == MYSQL_TYPE_YEAR         = MySQLInt . intLexer <$> getLenEncBytes
    | t == MYSQL_TYPE_LONGLONG          = MySQLInt64 . intLexer <$> getLenEncBytes
    | t == MYSQL_TYPE_FLOAT
        || t == MYSQL_TYPE_DOUBLE       = MySQLDouble . fracLexer <$> getLenEncBytes
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_DATETIME
        || t == MYSQL_TYPE_TIMESTAMP2
        || t == MYSQL_TYPE_DATETIME2    = do dt <- parseTimeM True defaultTimeLocale "%F %T" . BC.unpack <$> getLenEncBytes
                                             case dt of Just dt' -> pure (MySQLDateTime dt')
                                                        Nothing -> fail "Database.MySQL.MySQLValue: wrong TIMESTAMP/DATETIME format"
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = do d <- parseTimeM True defaultTimeLocale "%F" . BC.unpack <$> getLenEncBytes
                                             case d of Just d' -> pure (MySQLDateTime d')
                                                       Nothing -> fail "Database.MySQL.MySQLValue: wrong DATE format"
    | t == MYSQL_TYPE_TIME
        || t == MYSQL_TYPE_TIME2        = do td <- parseTimeM True defaultTimeLocale "%T" . BC.unpack <$> getLenEncBytes
                                             case td of Just td' -> pure (MySQLTime td')
                                                        Nothing -> fail "Database.MySQL.MySQLValue: wrong TIME format"
    | t == MYSQL_TYPE_GEOMETRY          = MySQLBytes <$> getLenEncBytes
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_BIT
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = (if isText then MySQLText . T.decodeUtf8 else MySQLBytes) <$> getLenEncBytes

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
    else getTextField f

getBinaryField :: Field -> Get MySQLValue
getBinaryField f
    | t == MYSQL_TYPE_NULL              = pure MySQLNull
    | t == MYSQL_TYPE_DECIMAL           = fail "Database.MySQL.MySQLValue: unsupported type MYSQL_TYPE_DECIMAL"
    | t == MYSQL_TYPE_NEWDECIMAL        = fail "Database.MySQL.MySQLValue: unsupported type MYSQL_TYPE_NEWDECIMAL"
    | t == MYSQL_TYPE_TINY              = MySQLInt . fromIntegral <$> getWord8
    | t == MYSQL_TYPE_SHORT             = MySQLInt . fromIntegral <$> getWord16le
    | t == MYSQL_TYPE_LONG              = MySQLInt . fromIntegral <$> getWord32le
    | t == MYSQL_TYPE_INT24             = MySQLInt . fromIntegral <$> getWord32le
    | t == MYSQL_TYPE_YEAR              = MySQLInt . fromIntegral <$> getWord16le
    | t == MYSQL_TYPE_LONGLONG          = MySQLInt64 . fromIntegral <$> getWord64le
    | t == MYSQL_TYPE_FLOAT             = MySQLDouble . realToFrac <$> getFloatle
    | t == MYSQL_TYPE_DOUBLE            = MySQLDouble <$> getDoublele
    | t == MYSQL_TYPE_TIMESTAMP
        || t == MYSQL_TYPE_DATETIME     = do n <- getLenEncInt
                                             case n of
                                                0 -> pure $ MySQLDateTime (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
                                                4 -> do
                                                    d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                                                    pure $ MySQLDateTime (LocalTime d (TimeOfDay 0 0 0))
                                                7 -> do
                                                    d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                                                    td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4
                                                    pure $ MySQLDateTime (LocalTime d td)
                                                11 -> do
                                                    d <- fromGregorian <$> getYear <*> getInt8' <*> getInt8'
                                                    td <- TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8
                                                    pure $ MySQLDateTime (LocalTime d td)
                                                _ -> fail $ "Database.MySQL.MySQLValue: wrong TIMESTAMP/DATETIME length"


    | t == MYSQL_TYPE_TIMESTAMP2        = fail "Database.MySQL.MySQLValue: unsupported type MYSQL_TYPE_TIMESTAMP2"
    | t == MYSQL_TYPE_DATETIME2         = fail "Database.MySQL.MySQLValue: unsupported type MYSQL_TYPE_DATETIME2"
    | t == MYSQL_TYPE_DATE
        || t == MYSQL_TYPE_NEWDATE      = do n <- getLenEncInt
                                             case n of
                                                0 -> pure $ MySQLDate (fromGregorian 0 0 0)
                                                4 -> MySQLDate <$> (fromGregorian <$> getYear <*> getInt8' <*> getInt8')
                                                _ -> fail $ "Database.MySQL.MySQLValue: wrong DATE/NEWDATE length"

    | t == MYSQL_TYPE_TIME              = do n <- getLenEncInt
                                             case n of
                                                0 -> pure $ MySQLTime (TimeOfDay 0 0 0)
                                                8 -> do
                                                    _ <- getWord8  -- we ignore sign here because 'TimeOfDay' doesn't support,
                                                    _ <- getWord32le   -- we also ignore the day part
                                                    MySQLTime <$> (TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond4)

                                                12 -> do
                                                    _ <- getWord8  -- we ignore sign here because 'TimeOfDay' doesn't support,
                                                    _ <- getWord32le   -- we also ignore the day part
                                                    MySQLTime <$> (TimeOfDay <$> getInt8' <*> getInt8' <*> getSecond8)
                                                _ -> fail $ "Database.MySQL.MySQLValue: wrong TIME length"

    | t == MYSQL_TYPE_TIME2             = fail "Database.MySQL.MySQLValue: unsupported type MYSQL_TYPE_TIME2"
    | t == MYSQL_TYPE_GEOMETRY          = MySQLBytes <$> getLenEncBytes
    | t == MYSQL_TYPE_VARCHAR
        || t == MYSQL_TYPE_BIT
        || t == MYSQL_TYPE_ENUM
        || t == MYSQL_TYPE_SET
        || t == MYSQL_TYPE_TINY_BLOB
        || t == MYSQL_TYPE_MEDIUM_BLOB
        || t == MYSQL_TYPE_LONG_BLOB
        || t == MYSQL_TYPE_BLOB
        || t == MYSQL_TYPE_VAR_STRING
        || t == MYSQL_TYPE_STRING       = if isText then MySQLText . T.decodeUtf8 <$> getLenEncBytes
                                                    else MySQLBytes <$> getLenEncBytes
  where
    t = fieldType f
    isText = fieldCharSet f /= 63
    -- please check https://github.com/jeremycole/mysql_binlog
    getYear :: Get Integer
    getYear = fromIntegral <$> getWord16le
    getInt8' :: Get Int
    getInt8' = fromIntegral <$> getWord8
    getSecond4 :: Get Pico
    getSecond4 = realToFrac <$> getWord8
    getSecond8 :: Get Pico
    getSecond8 = realToFrac <$> do
        s <- getInt8'
        ms <- fromIntegral <$> getWord32le
        pure $! (s + ms `div` 1000)

getBinaryRow :: [Field] -> Int -> Get [MySQLValue]
getBinaryRow fs flen = do
    _ <- getWord8           -- 0x00
    let maplen = (flen + 7 + 2) `Bit.shiftR` 3
    nullmap <- getByteString maplen
    go fs nullmap (0 :: Int)
  where
    go [] _       _        = pure []
    go (f:fs) nullmap pos = do
        r <- if isNull nullmap pos
                then return MySQLNull
                else getBinaryField f
        let pos' = pos + 1
        rest <- pos' `seq` go fs nullmap pos'
        return (r `seq` rest `seq` (r : rest))

    isNull nullmap pos =
        let (i, j) = (pos + 2) `divMod` 8
        in (nullmap `B.unsafeIndex` i) `Bit.testBit` j

-- | make a nullmap for params without offset
makeNullMap :: [MySQLValue] -> ByteString
makeNullMap vs = B.pack (reverse (go vs [] 0x00 0))
  where
    go :: [MySQLValue] -> [Word8] -> Word8 -> Int -> [Word8]
    go []     _   _     _ = []
    go va@(v:vs) acc byte pos
        | pos == 8          = go va (byte : acc) 0x00 0
        | v == MySQLNull    = let pos' = pos + 1 in pos' `seq` go vs acc byte pos'
        | otherwise         = let pos' = pos + 1
                                  byte' = byte .|. Bit.bit pos
                              in pos' `seq` byte' `seq` go vs acc byte' pos

