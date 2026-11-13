-- | How the TLS clients in this package name the server they connect to.
module Data.TLSSetting.ServerIdentification
    ( identifyServer
    ) where

import           Control.Applicative ((<|>))
import           Data.ByteString     (ByteString)
import           Data.Char           (isDigit, isHexDigit)
import           Data.List           (stripPrefix)
import           Data.Maybe          (fromMaybe)
import           Network.Socket      (HostName)
import qualified Network.TLS         as TLS

-- | Validates the server's certificate against @subjectName@ and sends it as
-- the server name indication (SNI), so a proxy or managed database that
-- routes TLS by server name reaches the right server. No server name is sent
-- for an IP literal or an empty name, which RFC 6066 section 3 forbids (tls
-- sends an IP literal as given, and before 2.4.9 an empty name too), nor when
-- @params@ turn SNI off.
--
-- Decision: a caller's 'TLS.clientUseServerNameIndication' set to 'False'
-- stands, rather than SNI always being sent for a host name. tls defaults it
-- to 'True', so 'False' is a deliberate choice, such as keeping the database's
-- name off the wire or a server that refuses names it does not know.
identifyServer :: HostName -> ByteString -> TLS.ClientParams -> TLS.ClientParams
identifyServer subjectName serviceSuffix params = params
    { TLS.clientServerIdentification = (subjectName, serviceSuffix)
    , TLS.clientUseServerNameIndication =
        TLS.clientUseServerNameIndication params
            && not (null subjectName)
            && not (isIPLiteral subjectName)
    }

-- | A name holding a colon is an IPv6 address, bracketed or not. A name is an
-- IPv4 address when it ends in a number, by the URL Standard's ends-in-a-number
-- checker (<https://url.spec.whatwg.org/#ends-in-a-number-checker>), which
-- covers what the resolver also takes as numeric, such as @127.1@,
-- @0x7f000001@ and @127.0.0.1.@.
isIPLiteral :: HostName -> Bool
isIPLiteral name = elem ':' name || isIPv4Number (lastLabel name)

-- | The label after the last dot, ignoring one trailing dot.
lastLabel :: HostName -> String
lastLabel name =
    let reversed = reverse name
    in reverse (takeWhile (/= '.') (fromMaybe reversed (stripPrefix "." reversed)))

-- | Decimal, octal (also all digits) or hexadecimal after @0x@, where the URL
-- Standard's IPv4 number parser takes a bare @0x@ as zero.
isIPv4Number :: String -> Bool
isIPv4Number label =
    case stripPrefix "0x" label <|> stripPrefix "0X" label of
        Just hexDigits -> all isHexDigit hexDigits
        Nothing -> not (null label) && all isDigit label
