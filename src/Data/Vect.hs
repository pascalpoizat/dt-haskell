{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

--
-- This module is a tentative to learn how to make dependent types in Haskell
-- by porting the Vect Idris module to Haskell (with some addings & differences when required).
-- https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/Vect.idr
-- See also the references in
-- https://github.com/pascalpoizat/dt-haskell/blob/master/README.md
-- that helped me in understanding things and inspired sometimes strongly the code here.
-- All credit should go to the authors of the above references (& Idris)
-- as my work was only in the porting.
--
-- differences wrt Idris:
--    missing: a lot, see TODOs
--    different: fromList, fromList', replicate
--    added: empty, replicate', sort (& insert)
--    syntactic: [] is Nil, :: is :>
--
module Data.Vect
where

import           Data.Kind         (Type)
import           Data.Type.Natural
import           Prelude           hiding (concat, head, init, last, length,
                                    map, replicate, tail, unzip, zipWith,
                                    zipWith3, (++))

data Vect :: Nat -> Type -> Type where
  Nil :: Vect Z a
  (:>) :: a -> Vect n a -> Vect (S n) a
infixr 7 :>

--
-- typeclasses
--

instance Eq a => Eq (Vect n a) where
  Nil == Nil = True
  (a :> v1) == (b :> v2) = a == b && v1 == v2

instance Ord a => Ord (Vect n a) where
  compare Nil Nil = EQ
  compare (a :> v1) (b :> v2) =
    case compare a b of
      EQ -> compare v1 v2
      x  -> x

instance Show a => Show (Vect n a) where
  showsPrec p = showsPrec p . toList

instance Functor (Vect n) where
  fmap = map

instance Foldable (Vect n) where
  foldr f e = foldrImpl f e id

--
-- length
--

length :: Vect n a -> Int
length Nil      = 0
length (_ :> v) = 1 + length v

--
-- indexing
--

tail :: Vect (S n) a -> Vect n a
tail (a :> v) = v

head :: Vect (S n) a -> a
head (a :> v) = a

last :: Vect (S n) a -> a
last (a :> Nil)    = a
last (a :> b :> v) = last $ b :> v

init :: Vect (S n) a -> Vect n a
init (a :> Nil)    = Nil
init (a :> b :> v) = a :> init (b :> v)

-- TODO: index, insertAt, deleteAt, replaceAt, updateAt

--
-- subvectors
--

-- TODO: take, drop, takeWhile, dropWhile

--
-- transformations
--

-- TODO: reverse, intersperse

--
-- conversion from and to lists
--

-- TODO: work on fromList and fromList' to get closer to Idris and avoid Maybe

-- different from fromList' in Idris
fromList' :: SingI n => [a] -> Maybe (Vect n a)
fromList' = fromList sing

-- different from fromList in Idris
fromList :: SNat n -> [a] -> Maybe (Vect n a)
fromList SZ     []       = Just Nil
fromList (SS n) (x : xs) = (x :>) <$> fromList n xs
fromList _      _        = Nothing

toList :: Vect n a -> [a]
toList Nil      = []
toList (a :> v) = a : toList v

--
-- building vectors
--

empty :: Vect Z a
empty = Nil

(++) :: Vect n a -> Vect m a -> Vect (n + m) a
Nil ++ v = v
(a :> v1) ++ v2 = a :> (v1 ++ v2)
infixr 6 ++

-- TODO: define a version that can be called usning replicate 3 0 instead of replicate sN3 0

replicate :: SNat n -> a -> Vect n a
replicate SZ     _ = Nil
replicate (SS n) a = a :> replicate n a

replicate' :: SingI n => a -> Vect n a
replicate' = replicate sing

-- TODO: define merge and mergeBy

--
-- zipping
--

zipWith :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f Nil       Nil       = Nil
zipWith f (a :> v1) (b :> v2) = f a b :> zipWith f v1 v2

zipWith3 :: (a -> b -> c -> d) -> Vect n a -> Vect n b -> Vect n c -> Vect n d
zipWith3 f Nil       Nil       Nil       = Nil
zipWith3 f (a :> v1) (b :> v2) (c :> v3) = f a b c :> zipWith3 f v1 v2 v3

zip :: Vect n a -> Vect n b -> Vect n (a, b)
zip = zipWith (,)

zip3 :: Vect n a -> Vect n b -> Vect n c -> Vect n (a, b, c)
zip3 = zipWith3 (,,)

unzip :: Vect n (a, b) -> (Vect n a, Vect n b)
unzip Nil = (Nil, Nil)
unzip ((l, r) :>  v) = let (lefts, rights) = unzip v in (l :> lefts, r :> rights)

--
-- mapping
--

map :: (a -> b) -> Vect n a -> Vect n b
map _ Nil      = Nil
map f (a :> v) = f a :> map f v

-- TODO: mapMaybe

--
-- folds
--

foldrImpl :: (t -> acc -> acc) -> acc -> (acc -> acc) -> Vect n t -> acc
foldrImpl f e go Nil      = go e
foldrImpl f e go (a :> v) = foldrImpl f e (go . f a) v

--
-- special folds
--

-- TODO: define concat

foldr1 :: (t -> t -> t) -> Vect (S n) t -> t
foldr1 f (a :> v) = foldr f a v

foldl1 :: (t -> t -> t) -> Vect (S n) t -> t
foldl1 f (a :> v) = foldl f a v

--
-- membership tests
--

elemBy :: (a -> a -> Bool) -> a -> Vect n a -> Bool
elemBy _ _ Nil = False
elemBy p e (a :> v) = p e a || elemBy p e v

elem :: Eq a => a -> Vect n a -> Bool
elem = elemBy (==)

lookupBy :: (key -> key -> Bool) -> key -> Vect n (key, val) -> Maybe val
lookupBy _ _ Nil = Nothing
lookupBy p e ((l, r) :> v) = if p e l then Just r else lookupBy p e v

lookup :: Eq key => key -> Vect n (key, val) -> Maybe val
lookup = lookupBy (==)

hasAnyBy :: (a -> a -> Bool) -> Vect n a -> Vect m a -> Bool
hasAnyBy _ _ Nil = False
hasAnyBy p elems (a :> v) = elemBy p a elems || hasAnyBy p elems v

hasAny :: Eq a => Vect n a -> Vect m a -> Bool
hasAny = hasAnyBy (==)

--
-- searching with a predicate
--



--
-- sorting
--

sort :: Ord a => Vect n a -> Vect n a
sort Nil      = Nil
sort (a :> v) = let sortedv = sort v in insert a sortedv

insert :: Ord a => a -> Vect n a -> Vect (S n) a
insert a Nil      = a :> Nil
insert a (b :> v) = if a <= b then a :> b :> v else b :> insert a v
