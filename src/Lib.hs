{-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DefaultSignatures         #-}
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
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-- some experiments with dependent types / type-level programming in Haskell
-- references:
-- http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html
-- https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell
-- https://hackage.haskell.org/package/singletons
-- https://github.com/konn/sized-vector
-- https://github.com/konn/sized
--
-- questions:
-- [x] are Nat, Z, S, +, *, min etc already defined somewhere in singletons (possibly with useful proofs)?
--    -> yes, in Data.Type.Natural (package type-natural)
-- [x] we define Vector and Matrix for fun, but are there already package for such kinds of structures?
--    -> yes, in https://github.com/konn/sized-vector (deprecated) and https://github.com/konn/sized (replacement)
--    -> and several others too (e.g., https://hackage.haskell.org/package/vec or https://hackage.haskell.org/package/vector-sized)

module Lib
where

import           Data.Singletons.TH
import           Data.Type.Natural
import           Prelude            hiding (head, replicate, tail, zipWith)

{-
singletons
   [d|

  data Nat = Z
           | S Nat
               deriving (Show, Eq, Ord)

  (+) :: Nat -> Nat -> Nat
  Z + n = n
  S m + n = S (m + n)

  (*) :: Nat -> Nat -> Nat
  Z * n = Z
  S m * n = n * m + m

  min :: Nat -> Nat -> Nat
  min Z Z         = Z
  min Z (S _)     = Z
  min (S _) Z     = Z
  min (S m) (S n) = S (min m n)
  |]
-- types: Z and S n
-- values (singletons): SZ and SS n
-}

--
-- from Idris book, chapter 4
-- see https://github.com/pascalpoizat/idris-book/blob/master/chapter4/Vehicle.idr
--

data PowerSource = Petrol | Pedal | Electric

data Vehicle (powersource :: PowerSource) where
  Unicycle :: Vehicle Pedal
  Bicycle :: Vehicle Pedal
  Motorcycle :: Nat -> Vehicle Petrol
  Car :: Nat -> Vehicle Petrol
  Bus :: Nat -> Vehicle Petrol
  Tram :: Vehicle Electric
  ElectricCar :: Vehicle Electric

wheels :: Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Tram = 10
wheels ElectricCar = 4

refuel :: Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

-- TODO: DataStore.idr

--
-- ongoing work
--

-- TODO: find why (a :: *) does not work
data Vector (size :: Nat) a where
  Nil :: Vector Z a
  (:-) :: a -> Vector n a -> Vector (S n) a
infixr 5 :-

deriving instance Eq a => Eq (Vector n a)

toList :: Vector n a -> [a]
toList Nil      = []
toList (a :- v) = a : toList v

--fromList :: SNat n -> [a] -> Maybe (Vector n a)
--fromList SZ _ = Just Nil
--fromList (SS n) (x :: xs) = (x :-) <$> fromList n xs
--fromList _ _  = Nothing

--fromList' :: SingI n => [a] -> Vector n a
--fromList' = fromList sing

instance Show a => Show (Vector n a) where
  showsPrec p = showsPrec p . toList

append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil v        = v
append (a :- v1) v2 = a :- append v1 v2

head :: Vector (S n) a -> a
head (a :- v) = a

tail :: Vector (S n) a -> a
tail (a :- v) = a

replicate :: SNat n -> a -> Vector n a
replicate SZ _     = Nil
replicate (SS n) a = a :- replicate n a

zipWithSame :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWithSame f Nil Nil             = Nil
zipWithSame f (a :- v1) (b :- v2) = f a b :- zipWithSame f v1 v2

zipWith :: (a -> b -> c) -> Vector n a -> Vector m b -> Vector (Min n m) c
zipWith f Nil Nil             = Nil
zipWith f Nil (b :- v)        = Nil
zipWith f (a :- v) Nil        = Nil
zipWith f (a :- v1) (b :- v2) = f a b :- zipWith f v1 v2

zipSame :: Vector n a -> Vector n b -> Vector n (a, b)
zipSame = zipWithSame (,)

zip :: Vector n a -> Vector m b -> Vector (Min n m) (a, b)
zip = zipWith (,)

type Matrix n m a = Vector n (Vector m a)

type IVect3 = Vector N3 Integer

aV1 :: IVect3
aV1 = 1 :- 2 :- 3 :- Nil

aV2 :: IVect3
aV2 = 4 :- 5 :- 6 :- Nil

{-
testMatrix :: Matrix n2 n3 Double
testMatrix = ((1) :- (2) :- (3) :- Nil) :-
             ((4) :- (5) :- (6) :- Nil) :-
             Nil
-}
