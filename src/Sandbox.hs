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

module Sandbox
where

import Data.Kind (Type)

--
-- questions:
-- - there seem to have a difference in doing Vect a n (ok) vs Vect n a. Why?
--

--
-- notes from https://downloads.haskell.org/%7Eghc/8.4.3/docs/html/users_guide/glasgow_exts.html#extension-StandaloneDeriving
-- mostly things taken from the doc, hence (c) GHC Team
-- plus some things from experiments or other references
--
-- - ConstrainedClassMethods
--      Allows the definition of further constraints on individual class methods.
--
-- - ExplicitForAll
--      Enable explicit universal quantification.
--      May bring type variables into scope.
--      Use with -Wunused-foralls
--
-- - FlexibleInstances
--      Implies TypeSynonymInstances.
--      Allow definition of type class instances with arbitrary nested types
--        in the instance head.
--      Still have to take care of the rules for instance termination.
--
-- - GADTs
--      Generalised Algebraic Data Types (GADTs) generalise ordinary algebraic data types
--        by allowing constructors to have richer return types.
--      Pattern matching causes type refinement, based on user supplied type annotations.
--      Lots of obscure error messages can happen in some cases (see doc).
--      Implies MonoLocalBinds, GADTSyntax
--
-- - GADTSyntax
--      Allow the use of GADT syntax in data type definitions (but not GADTs themselves).
--      GADT-style syntax generalises existential types.
--      Wrt types declared in the H98 syntax, GADT-style declarations
--        treat class constraints on the data constructors differently.
--      Specifically, if the constructor is given a type-class context,
--        that context is made available by pattern matching.
--      The type signature of each constructor is independent.
--      The variables in the first line have no scope.
--        One can use * instead (or Type with Data.Kinds).
--        Once can also kind them explicitly.
--        ex:
--          data Set a where ...
--          data Set :: * -> * where ...
--          data Set a (b :: * -> *) where ...
--      One can use strictness annotations.
--      One can use deriving clauses (but not for a GADT)
--      What makes a GADT into a GADT is not the syntax,
--        but rather the presence of data constructors
--        whose result type is not just T a b.
--      A constructor signature may mention type class constraints,
--        which can differ for different constructors
--      ... and much more things (see the doc)
--
-- - MonoLocalBinds
--      Infer less polymorphic types for local bindings by default.
--      Related to type inference predictability.
--
-- - MultiParamTypeClasses
--      Implies ConstrainedClassMethods.
--      Enable multi parameter type classes.
--
-- - StandaloneDeriving
--      Allow the use of stand-alone deriving declarations.
--      Can be in other modules wrt the datatype.
--      With FlexibleInstances, the instance can be more specific than the datatype.
--      Can be used to derive instances to exotic types.
--      Errors can come from boilerplate code that has been generated (not written).
--
-- - TypeFamilies TODO:
--
-- - TypeInType TODO:
--
-- - TypeSynonymInstances
--      Allow definition of type class instances for type synonyms.
--
-- - TypeOperators TODO:
--
-- - UndecidableInstances
--      Permit definition of instances which may lead to type-checker non-termination.
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
-- demonstration (personal code)
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

