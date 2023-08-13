{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpReq (headers) where

import Common (pathTo, rechunkBS)
import Control.Applicative
import Criterion.Main (bench, bgroup, nf, nfIO)
import Control.DeepSeq (NFData(..))
import Criterion.Types (Benchmark)
import Network.Wai.Handler.Warp.RequestHeader (parseHeaderLines)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString.Char8 as BC
import qualified Data.Binary.Parser as BP
import qualified Data.Binary.Parser.Char8 as BPC
import Network.HTTP.Types.Version (HttpVersion, http11)
import qualified Scanner as SC

headers :: IO [Benchmark]
headers = do
  req <- BC.readFile =<< pathTo "http-request.txt"
  return [
        bench "http-req/attoparsec" $ nf (AP.parseOnly attoRequest) req
      , bench "http-req/binary-parsers" $ nf (BP.parseOnly bpRequest) req
      , bench "http-req/scanner" $ nf (SC.scanOnly scRequest) req
      , bench "http-req/warp" $ nfIO (parseHeaderLines (BC.lines req))
      ]

--------------------------------------------------------------------------------

instance NFData HttpVersion where
    rnf !_ = ()

attoHeader = do
  name <- APC.takeWhile1 (APC.inClass "a-zA-Z0-9_-") <* APC.char ':' <* APC.skipSpace
  body <- attoBodyLine
  return (name, body)

attoBodyLine = APC.takeTill (\c -> c == '\r' || c == '\n') <* APC.endOfLine

attoReqLine = do
  m <- (APC.takeTill APC.isSpace <* APC.char ' ')
  (p,q) <- BC.break (=='?') <$> (APC.takeTill APC.isSpace <* APC.char ' ')
  v <- attoHttpVersion
  return (m,p,q,v)

attoHttpVersion = http11 <$ APC.string "HTTP/1.1"

attoRequest = (,) <$> (attoReqLine <* APC.endOfLine) <*> attoManyHeader

attoManyHeader = do
  c <- APC.peekChar'
  if c == '\r' || c == '\n'
    then return []

    else (:) <$> attoHeader <*> attoManyHeader

--------------------------------------------------------------------------------

bpHeader = do
  name <- BPC.takeWhile1 isHeaderChar <* BPC.char ':' <* BP.skipSpaces
  body <- bpBodyLine
  return (name, body)
  where
    isHeaderChar c = ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '0')
                  || c == '_' || c == '-'

bpBodyLine = BPC.takeTill (\c -> c == '\r' || c == '\n') <* BP.endOfLine

bpReqLine = do
  m <- (BPC.takeTill BPC.isSpace <* BPC.char ' ')
  (p,q) <- BC.break (=='?') <$> (BPC.takeTill BPC.isSpace <* BPC.char ' ')
  v <- bpHttpVersion
  return (m,p,q,v)

bpHttpVersion = http11 <$ BP.string "HTTP/1.1"

bpRequest = (,) <$> (bpReqLine <* BP.endOfLine) <*> bpManyHeader

bpManyHeader = do
  c <- BPC.peek
  if c == '\r' || c == '\n'
    then return []
    else (:) <$> bpHeader <*> bpManyHeader

--------------------------------------------------------------------------------

scHeader = do
  name <- takeWhile1 (isHeaderChar . w2c) <* SC.char8 ':' <* SC.skipSpace
  body <- scBodyLine
  return (name, body)
  where
    isHeaderChar c = ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '0')
                  || c == '_' || c == '-'

    takeWhile1 p = do
        bs <- SC.takeWhile p
        if BC.null bs then fail "takeWhile1" else return bs

scEndOfLine = do        -- scanner doesn't provide endOfLine, so we roll one here
    w <- SC.anyWord8
    case w of
        10 -> return ()
        13 -> SC.word8 10
        _  -> fail "endOfLine"
{-# INLINE scEndOfLine #-}

scBodyLine = SC.takeWhile (\w -> let c = w2c w in c /= '\r' && c /= '\n') <* scEndOfLine

scReqLine = do
  m <- (SC.takeWhile (not . BP.isSpace) <* SC.char8 ' ')
  (p,q) <- BC.break (=='?') <$> (SC.takeWhile (not . BP.isSpace) <* SC.char8 ' ')
  v <- scHttpVersion
  return (m,p,q,v)

scHttpVersion = http11 <$ SC.string "HTTP/1.1"

scRequest = (,) <$> (scReqLine <* scEndOfLine) <*> scManyHeader

scManyHeader = do
  w <- SC.lookAhead
  case w of
    Just w' -> do
      let c = w2c w'
      if c == '\r' || c == '\n'
        then return []
        else (:) <$> scHeader <*> scManyHeader
    _ -> fail "scManyHeader"
