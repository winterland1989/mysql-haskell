{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}
module QC.ByteString (tests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Char (chr, ord, toUpper)
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import QC.Common (ASCII(..), liftOp, parseBS, toStrictBS)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Scientific as Sci
import qualified Data.ByteString.Builder.Scientific as Sci
import qualified Data.Binary.Parser as P
import qualified Data.Binary.Parser.Char8 as P8
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

newtype ASCIIChar = ASCIIChar Char
    deriving (Eq, Ord, Show, Read)
instance Arbitrary ASCIIChar where
    arbitrary = ASCIIChar <$> choose ('\0', '\127')
    shrink (ASCIIChar c) = ASCIIChar <$> shrink c

-- Basic byte-level combinators.

satisfy :: Word8 -> L.ByteString -> Property
satisfy w s = parseBS (P.satisfy (<=w)) (L.cons w s) === Just w

satisfyWith :: ASCIIChar -> L.ByteString -> Property
satisfyWith (ASCIIChar c) s = parseBS (P.satisfyWith (chr . fromIntegral) (<=c))
                         (L.cons (fromIntegral (ord c)) s) === Just c

word8 :: Word8 -> L.ByteString -> Property
word8 w s = parseBS (P.word8 w *> pure w) (L.cons w s) === Just w

skipWord8 :: Word8 -> L.ByteString -> Property
skipWord8 w s =
  case (parseBS (P.skipWord8 (<w)) s, L.uncons s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . it) mcs
    (Just _,  mcs) -> maybe (property False) it mcs
  where it cs = liftOp "<" (<) (fst cs) w

anyWord8 :: L.ByteString -> Property
anyWord8 s
    | L.null s  = p === Nothing
    | otherwise = p === Just (L.head s)
  where p = parseBS P.anyWord8 s

peekMaybe :: L.ByteString -> Property
peekMaybe s
    | L.null s  = p === Just (Nothing, s)
    | otherwise = p === Just (Just (L.head s), s)
  where p = parseBS ((,) <$> P.peekMaybe <*> P.getRemainingLazyByteString) s

peek :: L.ByteString -> Property
peek s = parseBS P.peek s === (fst <$> L.uncons s)

string :: L.ByteString -> L.ByteString -> Property
string s t = parseBS (P.string s' *> pure s') (s `L.append` t) === Just s'
  where s' = toStrictBS s

stringCI :: ASCII L.ByteString -> ASCII L.ByteString -> Property
stringCI (ASCII s) (ASCII t) =
    parseBS (P8.stringCI up) (s `L.append` t) === Just s'
  where s' = toStrictBS s
        up = B8.map toUpper s'

strings :: L.ByteString -> L.ByteString -> L.ByteString -> Property
strings s t u =
    parseBS (P.string (toStrictBS s) >> (P.string t' *> pure t')) (L.concat [s,t,u])
    === Just t'
  where t' = toStrictBS t

skipWhile :: Word8 -> L.ByteString -> Property
skipWhile w s =
    let t = L.dropWhile (<=w) s
    in case P.runGetOrFail (P.skipWhile (<=w)) s of
         Right (t', _, _) -> t === t'
         Left  _          -> property False

takeCount :: Positive Int -> L.ByteString -> Property
takeCount (Positive k) s =
    case parseBS (P.getByteString k) s of
      Nothing -> liftOp ">" (>) (fromIntegral k) (L.length s)
      Just _s -> liftOp "<=" (<=) (fromIntegral k) (L.length s)

takeWhile :: Word8 -> L.ByteString -> Property
takeWhile w s =
    let (h,t) = L.span (==w) s
    in case P.runGetOrFail (P.takeWhile (==w)) s of
         Right (t', _, h') -> t === t' .&&. toStrictBS h === h'
         Left _            -> property False

take :: Int -> L.ByteString -> Property
take n s = maybe (property $ L.length s < fromIntegral n)
           (=== B.take n (toStrictBS s)) $
           parseBS (P.getByteString n) s

remaining :: L.ByteString -> Property
remaining s = maybe (property False) (=== s) .
                       parseBS P.getRemainingLazyByteString $ s

takeWhile1 :: Word8 -> L.ByteString -> Property
takeWhile1 w s =
    let s'    = L.cons w s
        (h,t) = L.span (<=w) s'
    in case P.runGetOrFail (P.takeWhile1 (<=w)) s' of
         Right (t', _, h') -> t === t' .&&. toStrictBS h === h'
         _                 -> property False

takeTill :: Word8 -> L.ByteString -> Property
takeTill w s =
    let (h,t) = L.break (==w) s
    in case P.runGetOrFail (P.takeTill (==w)) s of
         Right (t', _, h') -> t === t' .&&. toStrictBS h === h'
         _                 -> property False

takeWhile1_empty :: Property
takeWhile1_empty = parseBS (P.takeWhile1 undefined) L.empty === Nothing

endOfInput :: L.ByteString -> Property
endOfInput s = parseBS P.endOfInput s === if L.null s
                                          then Just ()
                                          else Nothing

endOfLine :: L.ByteString -> Property
endOfLine s =
  case (parseBS P.endOfLine s, L8.uncons s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . eol) mcs
    (Just _,  mcs) -> maybe (property False) eol mcs
  where eol (c,s') = c === '\n' .||.
                     (c, fst <$> L8.uncons s') === ('\r', Just '\n')

scan :: L.ByteString -> Positive Int64 -> Property
scan s (Positive k) = parseBS p s === Just (toStrictBS $ L.take k s)
  where p = P.scan k $ \ n _ ->
            if n > 0 then let !n' = n - 1 in Just n' else Nothing

decimal :: Integer -> Property
decimal d =
    let dBS = BB.toLazyByteString $ BB.integerDec d
    in parseBS (P.signed P.decimal) dBS === Just d

double :: Double -> Property
double d =
    let dBS = BB.toLazyByteString $ BB.doubleDec d
    in parseBS P.double dBS === Just d

scientific :: Sci.Scientific -> Property
scientific sci =
    let sciBS = BB.toLazyByteString $ Sci.formatScientificBuilder Sci.Generic Nothing sci
    in parseBS P.scientific sciBS === Just sci

tests :: [TestTree]
tests = [
      testProperty "anyWord8" anyWord8
    , testProperty "endOfInput" endOfInput
    , testProperty "endOfLine" endOfLine
    , testProperty "peekMaybe" peekMaybe
    , testProperty "peek" peek
    , testProperty "satisfy" satisfy
    , testProperty "satisfyWith" satisfyWith
    , testProperty "scan" scan
    , testProperty "skipWord8" skipWord8
    , testProperty "skipWhile" skipWhile
    , testProperty "string" string
    , testProperty "stringCI" stringCI
    , testProperty "strings" strings
    , testProperty "take" take
    , testProperty "takeCount" takeCount
    , testProperty "remaining" remaining
    , testProperty "takeTill" takeTill
    , testProperty "takeWhile" takeWhile
    , testProperty "takeWhile1" takeWhile1
    , testProperty "takeWhile1_empty" takeWhile1_empty
    , testProperty "word8" word8
    , testProperty "decimal" decimal
    , testProperty "double" double
    , testProperty "scientific" scientific
  ]
