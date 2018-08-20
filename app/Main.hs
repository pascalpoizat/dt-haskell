module Main where

import Lib

main :: IO ()
main = do
  print aV1
  print aV2
  print $ append aV1 aV2

