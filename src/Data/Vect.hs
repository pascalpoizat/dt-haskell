-- {-# LANGUAGE ConstraintKinds           #-}
-- {-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DefaultSignatures         #-}
-- {-# LANGUAGE EmptyCase                 #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs              #-}
-- {-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
-- {-# LANGUAGE PolyKinds                 #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TemplateHaskell           #-}
-- {-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}
-- {-# LANGUAGE UndecidableInstances      #-}

module Data.Vect
where

import           Data.Type.Natural
import           Prelude           hiding (head, length, map, replicate, tail,
                                    zipWith)

-- TODO: a should better be (a :: *)
data Vect (size :: Nat) a where
  Nil :: Vect Z a
  (:-) :: a -> Vect n a -> Vect (S n) a
infixr 5 :-

--
-- typeclasses
--

deriving instance Eq a => Eq (Vect n a)

instance Show a => Show (Vect n a) where
  showsPrec p = showsPrec p . toList

instance Functor (Vect n) where
  fmap = map

--
-- constructors
--

empty :: Vect Z a
empty = Nil

append :: Vect n a -> Vect m a -> Vect (n + m) a
append Nil       v  = v
append (a :- v1) v2 = a :- append v1 v2

replicate :: SNat n -> a -> Vect n a
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a

replicate' :: SingI n => a -> Vect n a
replicate' = replicate sing

--
-- destructors
--

head :: Vect (S n) a -> a
head (a :- v) = a

tail :: Vect (S n) a -> a
tail (a :- v) = a

--
-- basics
--

length :: Vect n a -> Int
length Nil      = 0
length (_ :- v) = 1 + length v

--
-- mapping
--

map :: (a -> b) -> Vect n a -> Vect n b
map _ Nil      = Nil
map f (a :- v) = f a :- map f v

--
-- zipping
--

zipWithSame :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWithSame f Nil       Nil       = Nil
zipWithSame f (a :- v1) (b :- v2) = f a b :- zipWithSame f v1 v2

zipWith :: (a -> b -> c) -> Vect n a -> Vect m b -> Vect (Min n m) c
zipWith f Nil       Nil       = Nil
zipWith f Nil       (b :- v)  = Nil
zipWith f (a :- v ) Nil       = Nil
zipWith f (a :- v1) (b :- v2) = f a b :- zipWith f v1 v2

zipSame :: Vect n a -> Vect n b -> Vect n (a, b)
zipSame = zipWithSame (,)

zip :: Vect n a -> Vect m b -> Vect (Min n m) (a, b)
zip = zipWith (,)

--
-- sorting
--

{-| insertion sort (not efficient on large vectors)
-}
sort :: Ord a => Vect n a -> Vect n a
sort Nil      = Nil
sort (a :- v) = let sortedv = sort v in insert a sortedv


{-| insert an element in a sorted vector
-}
insert :: Ord a => a -> Vect n a -> Vect (S n) a
insert a Nil      = a :- Nil
insert a (b :- v) = if a <= b then a :- b :- v else b :- insert a v

--
-- relation to other structures
--

toList :: Vect n a -> [a]
toList Nil      = []
toList (a :- v) = a : toList v

fromList :: SNat n -> [a] -> Maybe (Vect n a)
fromList SZ     _        = Just Nil
fromList (SS n) (x : xs) = (x :-) <$> fromList n xs
fromList _      _        = Nothing

fromList' :: SingI n => [a] -> Maybe (Vect n a)
fromList' = fromList sing
