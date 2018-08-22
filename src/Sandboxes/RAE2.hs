-- {-# LANGUAGE ConstraintKinds           #-}
-- {-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DefaultSignatures         #-}
-- {-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExplicitForAll            #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
-- {-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- {-# LANGUAGE NoImplicitPrelude         #-}
-- {-# LANGUAGE PolyKinds                 #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
-- {-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Sandboxes.RAE2
where

import Data.Kind (Type)

--
-- **VERY ONGOING WORK**
-- since I am reading a lot for find the good starting point
-- for the time being, rather look at Sandboxes/RAE1
--

--
-- notes from RE: https://www.reddit.com/r/haskell/comments/8lkv6l/richard_eisenberg_speaks_on_dependent_types/
--
-- RE requires:
-- - defines naturals, ie Nat, Zero, and Succ
--    (TODO: could it be imported from somewhere, eg Data.Type.Natural?)
-- - import Type (from Data.Kind)
--    Data.Kind defines basic kinds
--    (Type and Constraint, * used for pre-GHC8 compatibility)
-- - ExplicitForAll
-- - FlexibleInstances (hence TypeSynonymInstances)
-- - GADTs
-- - MultiParamTypeClasses
-- - StandaloneDeriving
-- - TypeFamilies
-- - TypeInType
-- - TypeOperators
-- - UndecidableInstances

--
-- the following code is from Stitch
-- https://cs.brynmawr.edu/~rae/pubs.html
-- (c) Richard A. Eisenberg
-- I type it here line by line to understand how things work
--

data Nat = Zero | Succ Nat
  deriving Show

type family n + m where
  Zero   + m = m
  Succ n + m = Succ (n + m)

data Fin :: Nat -> Type where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

deriving instance Show (Fin n)

finToInt :: Fin n -> Int
finToInt FZ = 0
finToInt (FS n) = 1 + finToInt n
  
-- we can use one of the two (we will cope to the first one, as in RE)
data Vec :: Type -> Nat -> Type where
-- data Vec (elem :: Type) (len :: Nat) where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)
infixr 5 :>

deriving instance Show a => Show (Vec a n)

elemIndex :: Eq a => a -> Vec a n -> Maybe (Fin n)
elemIndex _ VNil = Nothing
elemIndex x (y :> ys)
  | x == y = Just FZ
  | otherwise = FS <$> elemIndex x ys

--
-- personal sandbox
--

append :: Vec a n -> Vec a m -> Vec a (n + m)
append VNil ys = ys
append (x :> xs) ys = x :> append xs ys

type Nat3 = (Succ (Succ (Succ Zero)))
type Nat6 = (Succ (Succ (Succ Nat3)))
type VecInt n = Vec Int n
type VectInt3 = VecInt Nat3
type VectInt6 = VecInt Nat6

aVI3 :: VectInt3
-- aVI3 = 1 :> 2 :> VNil
-- Couldn't match type ‘'Zero’ with ‘'Succ 'Zero’
aVI3 = 1 :> 2 :> 3 :> VNil

-- bVI6 :: VectInt3
-- Couldn't match type ‘'Succ ('Succ ('Succ 'Zero))’ with ‘'Zero’
bVI6 :: VectInt6
bVI6 = append aVI3 aVI3

