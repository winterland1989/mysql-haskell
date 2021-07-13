module Database.MySQL.Query where

import           Control.Arrow             (first)
import           Database.MySQL.Protocol.MySQLValue
import           Data.String               (IsString (..))
import           Data.Typeable
import           Z.Data.ASCII
import qualified Z.Data.Builder            as B
import qualified Z.Data.Vector             as V
import qualified Z.Data.Text               as T
import           Z.IO.Exception

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
-- The underlying type is a 'V.Bytes', and literal Haskell strings
-- that contain Unicode characters will be correctly transformed to
-- UTF-8.
--
newtype Query = Query { fromQuery :: V.Bytes } deriving (Eq, Ord, Typeable)

instance Show Query where
    show = show . fromQuery

instance Read Query where
    readsPrec i = fmap (first Query) . readsPrec i

instance IsString Query where
    fromString = Query . fromString

-- | A type to wrap a query parameter in to allow for single and multi-valued parameters.
--
-- The behavior of 'Param' can be illustrated by following example:
--
-- @
--    buildParam $ One (MySQLText "hello") = hello
--    buildParam $ Many [MySQLText "hello", MySQLText "world"] = hello, world
--    buildParam $ Many [] = null
-- @
--
-- So you can now write a query like this: @ SELECT * FROM test WHERE _id IN (?, 888) @
-- and use 'Many' 'Param' to fill the hole. There's no equivalent for prepared statement sadly.
--
data Param = One  MySQLValue
           | Many [MySQLValue]

-- | A type that may be used as a single parameter to a SQL query. Inspired from @mysql-simple@.
class QueryParam a where
    buildParam :: a -> B.Builder ()
    -- ^ Prepare a value for substitution into a query string.

instance QueryParam Param where
    {-# INLINE buildParam #-}
    buildParam (One x)   = encodeTextField x
    buildParam (Many []) = "NULL"
    buildParam (Many xs) = B.intercalateList (B.comma) encodeTextField xs

instance QueryParam MySQLValue where
    {-# INLINE buildParam #-}
    buildParam = encodeTextField

renderParams :: HasCallStack => QueryParam p => Query -> [p] -> Query
renderParams (Query qry) params =
    let fragments = V.split QUESTION qry
    in Query . B.build $ merge fragments params
  where
    merge [x]    []     = B.bytes x
    merge (x:xs) (y:ys) = B.bytes x >> buildParam y >> merge xs ys
    merge _     _       = throw (WrongParamsCount callStack)

data WrongParamsCount = WrongParamsCount CallStack deriving Show
instance Exception WrongParamsCount
