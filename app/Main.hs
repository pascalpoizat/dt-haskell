module Main where

import           Data.Type.Natural
import           Data.Vect

type IVect3 = Vect N3 Integer

aV1 :: IVect3
aV1 = 1 :- 2 :- 3 :- Nil

aV2 :: IVect3
aV2 = 4 :- 5 :- 6 :- Nil

main :: IO ()
main = do
  print aV1
  print aV2
  print $ append aV1 aV2

