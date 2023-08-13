{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Common
    (
      ASCII(..)
    , parseBS
    , toLazyBS
    , toStrictBS
    , Repack
    , repackBS
    , repackBS_
    , liftOp
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Char (isAlpha)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Parser as P

#if !MIN_VERSION_base(4,4,0)
-- This should really be a dependency on the random package :-(
instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary = choose (minBound, maxBound)
#endif

parseBS :: P.Get r -> BL.ByteString -> Maybe r
parseBS p lbs = case P.parseLazy p lbs of
    Left _ -> Nothing
    Right v -> Just v

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks

toLazyBS :: B.ByteString -> BL.ByteString
toLazyBS = BL.fromChunks . (:[])

newtype ASCII a = ASCII { fromASCII :: a }
                  deriving (Eq, Ord, Show)

instance Arbitrary (ASCII B.ByteString) where
    arbitrary = (ASCII . B.pack) <$> listOf (choose (0,127))
    shrink = map (ASCII . B.pack) . shrink . B.unpack . fromASCII

instance Arbitrary (ASCII BL.ByteString) where
    arbitrary = ASCII <$> (repackBS <$> arbitrary <*> (fromASCII <$> arbitrary))
    shrink = map (ASCII . BL.pack) . shrink . BL.unpack . fromASCII

type Repack = NonEmptyList (Positive (Small Int))

repackBS :: Repack -> B.ByteString -> BL.ByteString
repackBS (NonEmpty bs) =
    BL.fromChunks . repackBS_ (map (getSmall . getPositive) bs)

repackBS_ :: [Int] -> B.ByteString -> [B.ByteString]
repackBS_ = go . cycle
  where go (b:bs) s
          | B.null s = []
          | otherwise = let (h,t) = B.splitAt b s
                        in h : go bs t
        go _ _ = error "unpossible"

liftOp :: (Show a, Testable prop) =>
          String -> (a -> a -> prop) -> a -> a -> Property
liftOp name f x y = counterexample desc (f x y)
  where op = case name of
               (c:_) | isAlpha c -> " `" ++ name ++ "` "
                     | otherwise -> " " ++ name ++ " "
               _ -> " ??? "
        desc = "not (" ++ show x ++ op ++ show y ++ ")"
