module QCUtils where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Int
import Data.Int.Int24
import Data.Word
import Data.Word.Word24

-- Arbitrary/CoArbitrary instances for Int24 and Word24

instance Arbitrary Int24 where
  arbitrary = arbitraryBoundedIntegral
  shrink = shrinkIntegral

instance CoArbitrary Int24 where
  coarbitrary = coarbitraryIntegral

instance Arbitrary Word24 where
  arbitrary = arbitraryBoundedIntegral
  shrink = shrinkIntegral

instance CoArbitrary Word24 where
  coarbitrary = coarbitraryIntegral
