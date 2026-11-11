{-|
Module      : Database.MySQL.BinLogProtocol.CRC32
Description : The checksum of binlog events
License     : BSD
Maintainer  : hi@jappie.me
Stability   : experimental
Portability : PORTABLE

The CRC32 that MySQL and MariaDB end each binlog event with when
@binlog_checksum@ is @CRC32@.
-}

module Database.MySQL.BinLogProtocol.CRC32
    ( crc32
    ) where

import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bits             (complement, shiftR, xor, (.&.))
import qualified Data.ByteString.Lazy  as L
import           Data.Word             (Word32, Word8)
import qualified Unwitch.Convert.Word8 as Word8

-- Decision: our own CRC32, as no dependency has one: crypton and ram offer
-- cryptographic and FNV/SipHash digests only, and zlib, whose Haskell binding
-- exports no crc32 anyway, is not a dependency. The digest package would add
-- a C library for these few lines.

-- | The CRC-32 of IEEE 802.3 and zlib (reflected polynomial 0xEDB88320,
-- initial and final XOR 0xFFFFFFFF): @crc32 "123456789" == 0xCBF43926@.
--
-- @since 1.5.0
crc32 :: L.ByteString -> Word32
crc32 bytes = complement (L.foldl' crc32Step 0xFFFFFFFF bytes)

crc32Step :: Word32 -> Word8 -> Word32
crc32Step crc byte =
    (crc32Table ! ((crc `xor` Word8.toWord32 byte) .&. 0xFF)) `xor` (crc `shiftR` 8)

-- Decision: a UArray indexed by Word32, so a lookup takes the masked CRC as
-- it is. A Vector needs an Int index, and unwitch converts Word32 to Int only
-- as a Maybe, whose Nothing could not happen here but would need handling.

-- | The CRC of each byte value, so that a byte costs one lookup instead of
-- eight shifts.
crc32Table :: UArray Word32 Word32
crc32Table = listArray (0, 255) (map crc32OfByte [0 .. 255])

crc32OfByte :: Word32 -> Word32
crc32OfByte byte = iterate reflectedShift byte !! 8

reflectedShift :: Word32 -> Word32
reflectedShift crc =
    if crc .&. 1 /= 0 then 0xEDB88320 `xor` (crc `shiftR` 1) else crc `shiftR` 1
