-- | A column read with the typed decoder of "Database.MySQL.Decoder" for its
-- type, then put back into the 'MySQLValue' constructor the old API returns,
-- so that tests written against the old API hold for the typed decoders too.
module TypedValue (typedDecoder) where

import           Data.Maybe                         (fromMaybe)
import qualified Database.MySQL.Decoder             as Decode
import           Database.MySQL.Protocol.MySQLValue

-- | The typed decoder for a column's type. TIME has none here, as
-- 'Decode.timeOfDay' takes only non-negative times and the tests read
-- negative ones; YEAR, BIT and GEOMETRY have no typed decoder at all.
typedDecoder :: ColumnKind -> Decode.FieldDecoder MySQLValue
typedDecoder column = fromMaybe MySQLNull <$> Decode.nullable (case column of
    NullColumn -> Decode.mysqlValue
    ValueColumn _ kind -> case kind of
        KindDecimal     -> MySQLDecimal <$> Decode.scientific
        KindInt8U       -> MySQLInt8U <$> Decode.word8
        KindInt8        -> MySQLInt8 <$> Decode.int8
        KindInt16U      -> MySQLInt16U <$> Decode.word16
        KindInt16       -> MySQLInt16 <$> Decode.int16
        KindInt32U      -> MySQLInt32U <$> Decode.word32
        KindInt32       -> MySQLInt32 <$> Decode.int32
        KindInt64U      -> MySQLInt64U <$> Decode.word64
        KindInt64       -> MySQLInt64 <$> Decode.int64
        KindFloat       -> MySQLFloat <$> Decode.float
        KindDouble      -> MySQLDouble <$> Decode.double
        KindYear        -> Decode.mysqlValue
        KindTimeStamp   -> MySQLTimeStamp <$> Decode.localTime
        KindDateTime    -> MySQLDateTime <$> Decode.localTime
        KindDate        -> MySQLDate <$> Decode.day
        KindTime        -> Decode.mysqlValue
        KindGeometry    -> Decode.mysqlValue
        KindText        -> MySQLText <$> Decode.text
        KindBytes       -> MySQLBytes <$> Decode.bytes
        KindBit         -> Decode.mysqlValue
        KindUnsupported -> Decode.mysqlValue)
