module Database.MySQL.Query where

import           Data.String               (IsString (..))
import           Control.Exception         (throw, Exception)
import           Data.Typeable
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Builder   as BB
import           Data.Word                 (Word8)
import           Control.Arrow             (first)
import           Database.MySQL.Protocol.MySQLValue
import           Data.Binary.Put

-- | Query string type borrowed from @mysql-simple@.
--
-- This type is intended to make it difficult to
-- construct a SQL query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce SQL injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- The underlying type is a 'L.ByteString', and literal Haskell strings
-- that contain Unicode characters will be correctly transformed to
-- UTF-8.
--
newtype Query = Query { fromQuery :: L.ByteString } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . BB.toLazyByteString . BB.stringUtf8

-- | A type to wrap a query parameter in to allow for single and multi-valued parameters.
--
-- The behavior of 'Param' can be illustrated by following example:
--
-- @
--    render $ One (MySQLText "hello") = hello
--    render $ Many [MySQLText "hello", MySQLText "world"] = hello, world
--    render $ Many [] = null
-- @
--
-- So you can now write a query like this: @ SELECT * FROM test WHERE _id IN (?, 888) @
-- and use 'Many' 'Param' to fill the hole. There's no equivalent for prepared statement sadly.
--
data Param = One  MySQLValue
           | Many [MySQLValue]

-- | A type that may be used as a single parameter to a SQL query. Inspired from @mysql-simple@.
class QueryParam a where
    render :: a -> Put
    -- ^ Prepare a value for substitution into a query string.

instance QueryParam Param where
    render (One x)      = putTextField x
    render (Many [])    = putTextField MySQLNull
    render (Many (x:[]))= putTextField x
    render (Many (x:xs))= do putTextField x
                             mapM_ (\f -> putCharUtf8 ',' >> putTextField f) xs

instance QueryParam MySQLValue where
    render = putTextField

-- | Fill the query's @?@ placeholders with the rendered parameters, in order.
--
-- A @?@ inside a quoted string or identifier (@\'...\'@, @\"...\"@,
-- @\`...\`@) or a comment (@-- @, @#@, @\/* *\/@) is not a placeholder. The
-- quotes are read with MySQL's default escaping, where a backslash escapes
-- the next character; under @NO_BACKSLASH_ESCAPES@ a string ending in a
-- backslash, such as @\'C:\\\'@, looks unterminated, so the placeholders
-- after it are missed. That mode breaks the escaping of the rendered values
-- as well.
--
-- Throws 'WrongParamsCount' when the number of placeholders and parameters
-- differ.
renderParams :: QueryParam p => Query -> [p] -> Query
renderParams (Query qry) params =
    let fragments = map L.fromStrict (splitAtPlaceholders (L.toStrict qry))
    in Query . runPut $ merge fragments params
  where
    merge [x]    []     = putLazyByteString x
    merge (x:xs) (y:ys) = putLazyByteString x >> render y >> merge xs ys
    merge _     _       = throw WrongParamsCount

-- | The query text between its placeholders, see 'renderParams': one more
-- fragment than there are placeholders.
splitAtPlaceholders :: B.ByteString -> [B.ByteString]
splitAtPlaceholders query = scanCode query 0 0

-- | Scan SQL outside quotes and comments; @start@ is where the current
-- fragment began.
scanCode :: B.ByteString -> Int -> Int -> [B.ByteString]
scanCode query start position = case byteAt query position of
    Nothing -> [B.drop start query]
    Just byte -> if
        | byte == questionMark ->
            fragment query start position : scanCode query (position + 1) (position + 1)
        | byte == singleQuote || byte == doubleQuote || byte == backtick ->
            scanQuoted query start byte (position + 1)
        | byte == hash ->
            scanLineComment query start (position + 1)
        | byte == dash && byteAt query (position + 1) == Just dash
            && opensDashComment (byteAt query (position + 2)) ->
            scanLineComment query start (position + 2)
        | byte == slash && byteAt query (position + 1) == Just star
            && not (isExecutableComment query (position + 2)) ->
            scanBlockComment query start (position + 2)
        | otherwise -> scanCode query start (position + 1)

-- | Scan inside a quote opened by @quote@. A doubled quote stands for the
-- quote character itself; a backslash escapes the next byte in strings but
-- not in backtick-quoted identifiers.
scanQuoted :: B.ByteString -> Int -> Word8 -> Int -> [B.ByteString]
scanQuoted query start quote position = case byteAt query position of
    Nothing -> [B.drop start query]
    Just byte -> if
        | byte == backslash && quote /= backtick -> scanQuoted query start quote (position + 2)
        | byte == quote -> if byteAt query (position + 1) == Just quote
            then scanQuoted query start quote (position + 2)
            else scanCode query start (position + 1)
        | otherwise -> scanQuoted query start quote (position + 1)

scanLineComment :: B.ByteString -> Int -> Int -> [B.ByteString]
scanLineComment query start position = case B.elemIndex newline (B.drop position query) of
    Nothing -> [B.drop start query]
    Just offset -> scanCode query start (position + offset + 1)

scanBlockComment :: B.ByteString -> Int -> Int -> [B.ByteString]
scanBlockComment query start position = case B.breakSubstring "*/" (B.drop position query) of
    (comment, rest) -> if B.null rest
        then [B.drop start query]
        else scanCode query start (position + B.length comment + 2)

-- | MySQL only takes @--@ for a comment when a space or control character
-- follows it, so @1--1@ stays arithmetic.
opensDashComment :: Maybe Word8 -> Bool
opensDashComment next = case next of
    Nothing -> True
    Just byte -> byte <= 0x20

-- | @\/*!@ and MariaDB's @\/*M!@ wrap SQL the server runs, so their
-- placeholders count.
isExecutableComment :: B.ByteString -> Int -> Bool
isExecutableComment query position =
    B.isPrefixOf "!" (B.drop position query) || B.isPrefixOf "M!" (B.drop position query)

-- | 'B.indexMaybe' needs bytestring 0.11, the bounds still admit 0.10.
byteAt :: B.ByteString -> Int -> Maybe Word8
byteAt query position =
    if position < B.length query then Just (B.index query position) else Nothing

fragment :: B.ByteString -> Int -> Int -> B.ByteString
fragment query start end = B.take (end - start) (B.drop start query)

questionMark, singleQuote, doubleQuote, backtick, backslash, hash, dash, slash, star, newline :: Word8
questionMark = 0x3F
singleQuote = 0x27
doubleQuote = 0x22
backtick = 0x60
backslash = 0x5C
hash = 0x23
dash = 0x2D
slash = 0x2F
star = 0x2A
newline = 0x0A

data WrongParamsCount = WrongParamsCount deriving (Show, Typeable)
instance Exception WrongParamsCount
