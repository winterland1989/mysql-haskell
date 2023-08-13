{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Data.Binary.Parser
-- Copyright   :  Daan Leijen 1999-2001, Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provide parsec\/attoparsec style parsing combinators for
-- <http://hackage.haskell.org/package/binary binary>
-- package. By default, this module export combinators in "Data.Binary.Get",
-- "Data.Binary.Parser.Word8" and "Data.Binary.Parser.Numeric", for additional ASCII char parser,
-- please check "Data.Binary.Parser.Char8" module.
--
-- The behaviour of parsers here is different to that of the
-- similarly-named parser in Parsec, as this one is all-or-nothing.
-- To illustrate the difference, the following parser will fail under
-- Parsec given an input of @\"for\"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that the first branch is a
-- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- before failing.  In binary-parsers, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
--
-- There're some redundant combinators get removed, for example:
--
-- @
-- choice == asum
-- count == replicateM
-- atEnd == isEmpty
-- take == getByteString
-- many1 == some
-- @
--
-- For fast byte set operations, please use <http://hackage.haskell.org/package/charset charset>
-- package.
--
-- It's recommanded to use 'parseOnly', 'parseDetail'... functions to run your parsers since these
-- functions are faster than binary's counter part by avoiding a small constant overhead.
-- Check 'parse' for detail.
--
-- = A few words on performance and backtracking
--
-- There's a common belief that parsers which support backtracking are slow, but it's not neccessarily
-- true in binary, because binary doesn't do book keeping if you doesn't use '<|>', 'lookAhead' or their
-- friends. Combinators in this library like 'peek', 'string'... also try to avoid backtracking so
-- it's faster to use them rather than do backtracking yourself, for example, 'peek' is faster than
-- @'lookAhead' 'getWord8'@. In practice, protocols are often designed to avoid backtracking.
-- For example, if you have following parser:
--
-- >branch1 <|> branch2 <|> (skipN 1 >> branch3)
--
-- And if you can select the right branch just by looking ahead one byte, then you can rewrite it to:
--
-- @
-- w <- peek
-- if  | w == b1 -> branch1
--     | w == b2 -> branch2
--     | w == b3 -> skipN 1 >> branch3
-- @
--
-- Binary performs as fast as a non-backtracking parser as long as you construct your parser
-- without using backtracking. And sometime backtracking is indeed neccessary, for example 'scientifically'
-- is almost impossible to implement correctly if you don't do backtracking.
--
module Data.Binary.Parser
    (
    -- * Running parsers
      Parser
    , parseOnly
    , parseLazy
    , parseDetail
    , parseDetailLazy
    , parse
    -- * Decoder conversion
    , maybeDecoder
    , eitherDecoder
    -- * Combinators
    , (<?>)
    , endOfInput
    , option
    , eitherP
    , match
    , many'
    , some'
    , sepBy
    , sepBy'
    , sepBy1
    , sepBy1'
    , manyTill
    , manyTill'
    , skipMany
    , skipMany1
    -- * Re-exports
    , module Data.Binary.Get
    , module Data.Binary.Parser.Word8
    , module Data.Binary.Parser.Numeric
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary.Get
import qualified Data.Binary.Get.Internal   as I
import           Data.Binary.Parser.Numeric
import           Data.Binary.Parser.Word8
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Internal as L (ByteString(..))

--------------------------------------------------------------------------------

-- | Alias to 'Get' for attoparsec compatibility.
type Parser a = Get a

-- | Run a parser on 'B.ByteString'.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @parseOnly (myParser <* endOfInput)@
--
parseOnly :: Get a -> B.ByteString -> Either String a
parseOnly g bs =
    case pushEndOfInput (parse g bs) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}


-- | Similar to 'parseOnly', but run a parser on lazy 'L.ByteString'.
--
parseLazy :: Get a -> L.ByteString -> Either String a
parseLazy g (L.Chunk bs lbs) =
    case pushEndOfInput (pushChunks (parse g bs) lbs) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> error "parseOnly: impossible error!"
parseLazy g L.Empty =
    case pushEndOfInput (parse g B.empty) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> error "parseOnly: impossible error!"
{-# INLINE parseLazy #-}

-- | Run a parser on 'B.ByteString'.
--
-- This function return full parsing results: the rest of input, stop offest and fail
-- message or parsing result.
--
-- /Since: 0.2.1.0/
--
parseDetail :: Get a
            -> B.ByteString
            -> Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, a)
parseDetail g bs =
    case pushEndOfInput (parse g bs) of
        Fail rest offset err -> Left (rest, offset, err)
        Done rest offset a   -> Right (rest, offset, a)
        _ -> error "parseOnly: impossible error!"
{-# INLINE parseDetail #-}

-- | Similar to 'parseDetail', but run a parser on lazy 'L.ByteString'.
--
-- /Since: 0.2.1.0/
--
parseDetailLazy :: Get a
                -> L.ByteString
                -> Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, a)
parseDetailLazy g (L.Chunk bs lbs) =
    case pushEndOfInput (pushChunks (parse g bs) lbs) of
        Fail rest offset err -> Left (rest, offset, err)
        Done rest offset a   -> Right (rest, offset, a)
        _ -> error "parseOnly: impossible error!"
parseDetailLazy g L.Empty =
    case pushEndOfInput (parse g B.empty) of
        Fail rest offset err -> Left (rest, offset, err)
        Done rest offset a   -> Right (rest, offset, a)
        _ -> error "parseOnly: impossible error!"
{-# INLINE parseDetailLazy #-}

-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoding errors and to get the output value.
--
-- This's faster than 'runGetIncremental' becuase it provides an initial chunk rather
-- than feeding 'B.empty' and waiting for chunks, this overhead is noticeable when you're
-- running small getters over short 'ByteString' s.
--
-- /Since: 0.2.1.0/
--
parse :: Get a -> B.ByteString -> Decoder a
parse g bs = calculateOffset (loop (I.runCont g bs I.Done)) (fromIntegral $ B.length bs)
  where
    calculateOffset r !acc = case r of
        I.Done inp a -> Done inp (acc - fromIntegral (B.length inp)) a
        I.Fail inp s -> Fail inp (acc - fromIntegral (B.length inp)) s
        I.Partial k -> Partial $ \ms -> case ms of
                Nothing -> calculateOffset (k Nothing) acc
                Just i -> calculateOffset (k ms) (acc + fromIntegral (B.length i))
        I.BytesRead unused k -> calculateOffset (k $! (acc - unused)) acc

    loop r = case r of
        I.Partial k -> I.Partial $ \ms -> case ms of Just _ -> loop (k ms)
                                                     Nothing -> completeLoop (k ms)
        I.BytesRead n k -> I.BytesRead n (loop . k)
        I.Done _ _ -> r
        I.Fail _ _ -> r

    completeLoop r = case r of
        I.Partial k -> completeLoop (k Nothing)
        I.BytesRead n k -> I.BytesRead n (completeLoop . k)
        I.Fail _ _ -> r
        I.Done _ _ -> r

--------------------------------------------------------------------------------

-- | Convert a 'Decoder' value to a 'Maybe' value. A 'Partial' result
-- is treated as failure.
--
-- /Since: 0.2.3.0/
--
maybeDecoder :: Decoder r -> Maybe r
maybeDecoder (Done _ _ r) = Just r
maybeDecoder _            = Nothing
{-# INLINE maybeDecoder #-}

-- | Convert a 'Decoder' value to an 'Either' value. A 'Partial'
-- result is treated as failure.
--
-- /Since: 0.2.3.0/
--
eitherDecoder :: Decoder r -> Either String r
eitherDecoder (Done _ _ r)   = Right r
eitherDecoder (Fail _ _ msg) = Left msg
eitherDecoder _              = Left "Decoder: incomplete input"
{-# INLINE eitherDecoder #-}

--------------------------------------------------------------------------------

-- | Name the parser, in case failure occurs.
(<?>) :: Get a -> String -> Get a
(<?>) = flip label
infix 0 <?>
{-# INLINE (<?>) #-}

-- | Match only if all input has been consumed.
endOfInput :: Get ()
endOfInput = do
    e <- isEmpty
    unless e (fail "endOfInput")
{-# INLINE endOfInput #-}

-- | @option x p@ tries to apply action @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x
{-# SPECIALIZE option :: a -> Get a -> Get a #-}

-- | Combine two alternatives.
eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: Get a -> Get (B.ByteString, a)
match p = do
    pos1 <- bytesRead
    (x, pos2) <- lookAhead $ (,) <$> p <*> bytesRead
    (,) <$> (getByteString . fromIntegral) (pos2 - pos1) <*> pure x
{-# INLINE match #-}

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | @many' p@ applies the action @p@ /zero/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many' letter
many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p
  where many_p = some_p `mplus` return []
        some_p = liftM2' (:) p many_p
{-# INLINE many' #-}

-- | @some' p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = some' letter
some' :: (MonadPlus m) => m a -> m [a]
some' p = liftM2' (:) p (many' p)
{-# INLINE some' #-}

-- | @sepBy p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (char ',')
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
{-# SPECIALIZE sepBy :: Get a -> Get s -> Get [a] #-}

-- | @sepBy' p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy'` (char ',')
sepBy' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy' p s = go `mplus` return []
  where go = liftM2' (:) p ((s >> sepBy1' p s) `mplus` return [])
{-# SPECIALIZE sepBy' :: Get a -> Get s -> Get [a] #-}

-- | @sepBy1 p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy1` (char ',')
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = go
    where go = liftA2 (:) p ((s *> go) <|> pure [])
{-# SPECIALIZE sepBy1 :: Get a -> Get s -> Get [a] #-}

-- | @sepBy1' p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy1'` (char ',')
sepBy1' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy1' p s = go
    where go = liftM2' (:) p ((s >> go) `mplus` return [])
{-# SPECIALIZE sepBy1' :: Get a -> Get s -> Get [a] #-}

-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = go
    where go = (end *> pure []) <|> liftA2 (:) p go
{-# SPECIALIZE manyTill :: Get a -> Get b -> Get [a] #-}

-- | @manyTill' p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill' anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
--
-- The value returned by @p@ is forced to WHNF.
manyTill' :: (MonadPlus m) => m a -> m b -> m [a]
manyTill' p end = go
    where go = (end >> return []) `mplus` liftM2' (:) p go
{-# SPECIALIZE manyTill' :: Get a -> Get b -> Get [a] #-}

-- | Skip zero or more instances of an action.
skipMany :: Alternative f => f a -> f ()
skipMany p = go
    where go = (p *> go) <|> pure ()
{-# SPECIALIZE skipMany :: Get a -> Get () #-}

-- | Skip one or more instances of an action.
skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p
{-# SPECIALIZE skipMany1 :: Get a -> Get () #-}
