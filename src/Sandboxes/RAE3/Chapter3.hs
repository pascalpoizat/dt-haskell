-- {-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds            #-}
-- {-# LANGUAGE DefaultSignatures         #-}
-- {-# LANGUAGE EmptyCase                 #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                #-}
-- {-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
-- {-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies         #-}
-- {-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fprint-explicit-kinds           #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}
-- {-# OPTIONS_GHC -Wunused-type-patterns           #-}

module Sandboxes.RAE3.Chapter3 where

import           Data.Kind          (Type)
import           Data.Singletons.TH
import           Prelude            hiding ((+), replicate)

--
-- examples from reference:
-- Dependent Types in Haskell: Theory and Practice.
-- R. A. Eisenberg, PhD thesis, University of Pennsylvania, 2016.
-- https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs
--

--
-- We define Nat because Nat in GHC use efficient representation and cannot be pattern matched against
-- Use of Type instead of * (Data.Kind)
-- To have + on vector sizes:
-- - either we can define a type family for :+: and use :+: instead of '+
-- - or we need Data.Singletons to derive '+ from +
--
--  -Wunused-type-patterns is not practical with singletons
--
-- SOLUTIONS OF THE THESIS (YELLOW EXAMPLES) = ??
--
-- stopped at the end of 3.1.1 (not enough info to continue -> skip to chapter 7)
--

{-
-- version of the thesis for Nat and (+) on values
data Nat = Zero | Succ Nat

(+) :: Nat -> Nat -> Nat
Zero + m = m
Succ n + m = Succ (n + m)

-- for (+) on types we try first with a type family
-- see personal sandbox
-- the second try uses singletons
-- we define it from the thesis and Data.Type.Natural
-}

singletons [d|

  data Nat = Zero | Succ Nat

  (+) :: Nat -> Nat -> Nat
  Zero + m = m
  Succ n + m = Succ (n + m)

  -- we define some naturals
  -- so we get (for example)
  -- - n1 : the natural value 1
  -- - N1 : the type 1
  -- - sN1 : the singleton value inhabiting N1
  n0 = Zero
  n1 = Succ Zero
  n2 = Succ n1
  n3 = Succ n2

  |]

data Vec :: Type -> Nat -> Type where
  Nil :: Vec a 'Zero
  (:>) :: a -> Vec a n -> Vec a ('Succ n)
infixr 5 :>

-- using our type family
-- append :: Vec a n -> Vec a m -> Vec a (n :+: m)
-- using singletons (we use +, not '+ as in the thesis)
-- this is my solution from reading other references
-- SOLUTION OF THE THESIS = ??
append :: Vec a n -> Vec a m -> Vec a (n + m)
append Nil      w = w
append (a :> v) w = a :> append v w

-- this is my solution from reading other references
-- SOLUTION OF THE THESIS = ??
--   F a . P (n :: Nat) -> a -> Vec a n
replicate :: SNat n -> a -> Vec a n
replicate SZero _ = Nil
replicate (SSucc n) x = x :> replicate n x

-- this is my solution from reading other references
-- SOLUTION OF THE THESIS = ??
--           P (n :: Nat) . F a . a -> Vec a n
replicateInvis :: SingI (n :: Nat) => a -> Vec a n
replicateInvis = replicate sing

-- SOLUTION OF THE THESIS = ??
-- P (n) . F a . Vec a n -> Nat
-- lengthRel :: ???

-- this is my solution from reading other references
-- SOLUTION OF THE THESIS = ??
-- F n a  . Vec a n -> Nat
lengthIrrel :: Vec a n -> Nat
lengthIrrel Nil = Zero
lengthIrrel (_ :> v) = Succ Zero + lengthIrrel v

--
-- personal sandbox
--

-- we use StandaloneDeriving for these
deriving instance Show Nat
deriving instance Show a => Show (Vec a n)

{-
-- first a try with a type family
type family (n :: Nat) :+: (m :: Nat) :: Nat
type instance 'Zero :+: m = m
type instance 'Succ n :+: m = 'Succ (n :+: m)
-- OK but error message if we do errors in append are not clear at all
-}

{-
type N0 = 'Zero
type N1 = 'Succ 'Zero
type N2 = 'Succ N1
type N3 = 'Succ N2
n0 :: Nat
n0 = Zero
n1 :: Nat
n1 = Succ n0
n2 :: Nat
n2 = Succ n1
n3 :: Nat
n3 = Succ n2
-}

aV1 :: Vec Integer N3
aV1 = 1 :> 2 :> 3 :> Nil

aV2 :: Vec Integer N2
aV2 = 4 :> 5 :> Nil

aV3 :: Vec Integer N3
aV3 = replicate sN3 0

aV4 :: Vec Integer N3
aV4 = replicateInvis 1

testcase :: IO ()
testcase = do
  putStrLn "\n*** chapter 3\n"
  print $ n3 + n2
  print aV1
  print aV2
  print $ append aV1 aV2
  print aV3
  print aV4
  print $ lengthIrrel aV4
  putStrLn "\nchapter 3 failed"

