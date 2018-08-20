{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Data.Type.Natural
import           Data.Vect
import           Prelude           (IO, Integer, Maybe (..), String, print,
                                    pure, putStrLn, ($), (+), (<$>), (>>=))

--
-- consider writing real tests using Tasty
--

type IVect n = Vect n Integer
type IVect3 = IVect N3

aV1 :: IVect3
aV1 = 1 :- 2 :- 3 :- Nil

aV2 :: IVect3
aV2 = 4 :- 5 :- 6 :- Nil

aV3 :: IVect3
aV3 = replicate sN3 0

aV4 :: IVect3
aV4 = replicate' 0

aV5 :: Maybe (IVect N6)
aV5 = fromList' [1,1,3,4,7,11]

testAppend :: IO ()
testAppend = print $ append aV2 aV1

testFromList :: IO ()
testFromList = case aV5 of
  Nothing -> print "nothing"
  Just v  -> print v

testShow :: IO ()
testShow = do
  print aV1
  print aV2

testReplicate :: IO ()
testReplicate = do
  print aV3
  print aV4

testMap :: IO ()
testMap = print $ (+ 1) <$> append aV2 aV1

testSort :: IO ()
testSort = print $ sort $ append aV2 aV1

test :: String -> IO () -> IO ()
test what f = putStrLn what >>= pure f

main :: IO ()
main = do
  test "show" testShow
  test "replicate" testReplicate
  test "append" testAppend
  test "fromList" testFromList
  test "map" testMap
  test "sort" testSort
