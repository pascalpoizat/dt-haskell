{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
-- {-# LANGUAGE DefaultSignatures         #-}
-- {-# LANGUAGE EmptyCase                 #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ExplicitForAll            #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE FlexibleInstances         #-}
-- {-# LANGUAGE FunctionalDependencies    #-}
-- {-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE GADTs               #-}
-- {-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
-- {-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies        #-}
-- {-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators       #-}
-- {-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module Sandboxes.RAE1.Chapter2
where

import           Data.Kind                (Constraint)
import           Data.List                (intercalate)
import           Data.Primitive.ByteArray (ByteArray)
import           Data.Set                 as Set (Set, singleton)
import           Data.Type.Equality       ((:~:) (Refl), castWith)
import           Data.Vector              (Vector)
import           Prelude                  hiding ((+))

--
-- examples from reference:
-- Dependent Types in Haskell: Theory and Practice.
-- R. A. Eisenberg, PhD thesis, University of Pennsylvania, 2016.
-- https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs
--

-- Possible required things, by order in the thesis:
-- - TypeFamilies
-- - DataKinds
-- - TypeOperators
-- - PolyKinds
-- - GADTs (or GADTSyntax + ExistentialQuantification)
-- - ConstraintKinds
-- - import Data.Kind (for Constraint)
-- - import Data.Type.Equality (for propositional equality, or write own defs)
-- - RankNTypes
-- - ScopedTypeVariables
-- - (FunctionalDependencies presented but not required in the approach)
-- - (MultiParamTypeClasses and FlexibleInstances not required in the examples without FD)
-- - (UndecidableInstances not required in the examples without FD)

-- writing this requires TypeFamilies
type family F1 a where
  F1 Int = Bool
  F1 Char = Double

useF1 :: F1 Int -> F1 Char
useF1 True  = 1.0
useF1 False = (- 1.0)

type family Element c
class Collection c where
  singleton :: Element c -> c
type instance Element [a] = a
instance Collection [a] where
  singleton x = [x]
type instance Element (Set a) = a
instance Collection (Set a) where
  singleton = Set.singleton

type family F2 a
type instance F2 Int = Bool

data family Array a
data instance Array Bool = MkArrayBool ByteArray
data instance Array Int = MkArrayInt (Vector Int)

data Nat = Zero | Succ Nat

-- this is + on type Nat
(+) :: Nat -> Nat -> Nat
Zero   + b = b
Succ a + b = Succ (a + b)

-- writing this requires
-- - DataKinds to get kinds for data
--    from the definition of type Nat (with Zero and Succ) we get kind Nat with types 'Zero, and 'Succ)
-- - TypeOperators (+ between a and b)
-- it enable one to write type 'Succ 'Zero + 'Succ 'Zero (type "1" + type "1")
-- and Haskell simplifies it to type 'Succ ('Succ Zero)  (type "2")
-- note that ticks (eg in 'Zero) are optional if there is no ambiguity

-- this is + on kind Nat (kind left unspecified)
-- type family a + b where
--   'Zero   + b = b
--   'Succ a + b = 'Succ (a + b)

-- this is + on kind Nat (kind being specified)
type family (a :: Nat) + (b :: Nat) where
  'Zero   + b = b
  'Succ a + b = 'Succ (a + b)

-- writing this requires PolyKinds
-- -fprint-explicit-kinds is used to make GHC show invisible parameters corresponding to kind arguments
type family Length (list :: [k]) :: Nat where
  Length '[] = 'Zero
  Length (x ': xs) = 'Succ (Length xs)

-- writing this requires
-- - GADTs or GADTSyntax + ExistentialQuantification
-- - ConstraintKinds
-- - import Data.Kind (for Constraint)
data Some :: (* -> Constraint) -> * where
  Some :: c a => a -> Some c

showSomething :: Some Show -> String
showSomething (Some thing) = show thing

heteroList :: [Some Show]
heteroList = [Some True, Some (5 :: Int), Some (Just ())]

printList :: [Some Show] -> String
printList things = "[" ++ intercalate ", " (map showSomething things) ++ "]"

-- to use :~:, Refl, and castWith, without defining them
-- - import Data.Type.Equality
{-
data (a :: k) :~: (b :: k) where
  Refl :: a :~: a
castWith :: (a :~: b) -> a -> b
castWith Refl x = x
-}

{-
data (a :: k) :~: (b :: k) where
  Refl :: (a ~ b) => a :~:b

castWith :: (a :~:b) -> a -> b
castWith Refl x = x
-}

x :: Int
x = 1

-- OK
y :: Int
y = castWith Refl x

-- KO: Couldn't match type ‘Int’ with ‘Integer’
-- z :: Integer
-- z = castWith Refl x

-- higher rank types cannot be inferred and require a signature
-- requires RankNTypes
higherRank :: (forall a . a -> a) -> (Bool, Char)
higherRank f = (f True, f 'x')

-- requires ScopedTypeVariables
foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
 where
  lgo :: b -> [a] -> b
  lgo z []       = z
  lgo z (x : xs) = lgo (f z x) xs

-- requires MultiParamTypeClasses (for the example)
-- requires FlexibleInstances (for the example)
-- requires FunctionalDependencies (in general)
{-
class Pred (a :: Nat) (b :: Nat) | a -> b
instance Pred 'Zero 'Zero
instance Pred ('Succ n) n
-}

-- requires UndecidableInstances (for the example)
{-
class Plus (a :: Nat) (b :: Nat) (r :: Nat) | a b -> r, r a -> b
instance Plus 'Zero b b
instance Plus a b r => Plus ('Succ a) b ('Succ r)
-}

--
-- personal sandbox
--

-- in showSomething
-- no Show a constraint in signature
-- use of pattern matching
-- compare with:
showSomething' :: Show a => a -> String
showSomething' = show

heteroNumList :: [Some Num]
heteroNumList = [Some (1 :: Int), Some (2 :: Integer), Some 3.0]

-- alternative to type dependency example 1 (and we know that a -> b)
type family Pred' (a :: Nat) :: Nat
type instance Pred' 'Zero = 'Zero
type instance Pred' ('Succ n) = n

-- alternative to type dependency example 2 (and we know that a b -> r, but not r a -> b)
type family Plus' (a :: Nat) (b :: Nat) :: Nat
type instance Plus' 'Zero n = n
type instance Plus' ('Succ n) m = 'Succ (Plus' n m)

testcase :: IO ()
testcase =
  print "RAE1 passed"
