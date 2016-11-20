module Database.MySQL.Param
    ( render
    , Param (..)
    , Parametric
    ) where

import Database.MySQL.Protocol.MySQLValue (MySQLValue(..), putTextField)
import Data.Binary.Put (Put, putCharUtf8)

-- | A type to wrap a query parameter in to allow for single and multi-valued parameters.
data Param = One  MySQLValue
           | Many [MySQLValue]

-- | A type that may be used as a single parameter to a SQL query.
class Parametric a where
    render :: a -> Put
    -- ^ Prepare a value for substitution into a query string.

instance Parametric Param where
    render (One x)      = putTextField x
    render (Many [])    = putTextField MySQLNull
    render (Many (x:[]))= putTextField x
    render (Many (x:xs))= do putTextField x
                             mapM_ (\f -> putCharUtf8 ',' >> putTextField f) xs

instance Parametric MySQLValue where
    render = putTextField