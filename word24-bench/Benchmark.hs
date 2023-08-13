module Main where

import Criterion.Main
import Control.DeepSeq
import Data.Int.Int24
import Data.Word.Word24
import Data.Int
import Data.Word
import Data.List

main = defaultMain
  [ bgroup "Int24" intses
  , bgroup "Word24" wordses
  , bgroup "Int16"    baseses
  , bgroup "Word16" basessW
  ]

benches :: (Enum i, Num i, NFData i, Integral i) => i -> [Benchmark]
benches x =
   [ bench "Add" $ nf (\i -> foldl' (+) i [1..100]) x
   , bench "Mul" $ nf (\i -> foldl' (*) i [1..100]) x
   , bench "quot" $ nf (\i -> map (flip quot i) [1..25]) x
   , bench "rem" $ nf (\i -> map (flip rem i) [1..25]) x
   , bench "div" $ nf (\i -> map (flip div i) [1..25]) x
   , bench "mod" $ nf (\i -> map (flip mod i) [1..25]) x
   ]
{-# INLINE benches #-}

intses  = benches (1 :: Int24)
wordses = benches (1 :: Word24)
baseses = benches (1 :: Int16)
basessW = benches (1 :: Word16)
