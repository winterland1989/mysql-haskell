module Database.MySQL.Query where

import           Data.String               (IsString (..))
import           Control.Exception         (throw, Exception)
import           Data.Typeable
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.Char8     as LC
import qualified Data.ByteString.Builder   as BB
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

renderParams :: QueryParam p => Query -> [p] -> Query
renderParams (Query qry) params =
    let fragments = LC.split '?' qry
    in Query . runPut $ merge fragments params
  where
    merge [x]    []     = putLazyByteString x
    merge (x:xs) (y:ys) = putLazyByteString x >> render y >> merge xs ys
    merge _     _       = throw WrongParamsCount

data WrongParamsCount = WrongParamsCount deriving (Show, Typeable)
instance Exception WrongParamsCount
