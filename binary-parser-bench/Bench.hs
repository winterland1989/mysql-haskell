{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import qualified Aeson
import qualified AesonBP
import qualified HttpReq
import Data.List

main :: IO ()
main = do
    http <- HttpReq.headers

    putStrLn "start benchmark http request parser"

    defaultMain http

    putStrLn "start benchmark JSON parser"

    aeson <- Aeson.aeson
    aesonbp <- AesonBP.aeson
    aesonLazy <- Aeson.aesonLazy
    aesonbpLazy <- AesonBP.aesonLazy

    (defaultMain . concat . transpose) [ aeson, aesonbp, aesonLazy, aesonbpLazy ]
