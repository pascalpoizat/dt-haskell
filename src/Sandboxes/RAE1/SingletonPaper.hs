{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
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
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fprint-explicit-kinds           #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}
-- {-# OPTIONS_GHC -Wunused-type-patterns           #-}
{-# OPTIONS_GHC -Wno-redundant-constraints       #-}

module Sandboxes.RAE1.SingletonPaper where

import           Data.Kind                    (Type)
import           Data.Singletons.Prelude.Bool
import           Data.Singletons.TH           hiding (type (<), (%<), (<))
import           Prelude                      (Bool (..), IO (..), Int (..), Eq, Read, String,
                                               Show (..), error, print, pure,
                                               putStrLn, ($), (.))
import qualified Prelude                      as P ((+))

--
-- Dependently Typed Programming with Singletons.
-- R. A. Eisenberg and S. Weirich
-- Haskell Symposium (2012)
--

--
-- Haskell programmers have been trying to do DT in Haskell for at least a decade
-- clever encodings but code looks different than DT code
-- led to extensions to Haskell type system such as GADTs, type families, datatype promotion
-- but there remains difference with DT code
-- singletons library generates boilerplate code
--

--
-- there are some errors
-- retry importing singletons (Nat, Bool, etc) instead of defining them
--

singletons [d|

  {-
  -- we reuse Bool from singletons
  data Bool where
    True :: Bool
    False :: Bool
  -}

  data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
      deriving (Eq, Read, Show)

  (<) :: Nat -> Nat -> Bool
  m < Zero = False
  Zero < Succ n = True
  Succ m < Succ n = m < n

  (+) :: Nat -> Nat -> Nat
  Zero + n = n
  (Succ m) + n = Succ (m + n)

  isEven :: Nat -> Bool
  isEven Zero            = True
  isEven (Succ Zero)     = False
  isEven (Succ (Succ n)) = isEven n

  nextEven :: Nat -> Nat
  nextEven n = if isEven n then n else Succ n

  |]

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

{-
-- we use singletons
type family (m :: Nat) :< (n :: Nat) :: Bool
type instance m :< Zero = 'False
type instance 'Zero :< 'Succ n = 'True
type instance 'Succ m :< 'Succ n = m :< n
-}

nth :: (m < n) ~ 'True => SNat m -> Vec a n -> a
nth SZero       (VCons a _ ) = a
nth (SSucc sm') (VCons _ as) = nth sm' as
-- nth m VNil = error "index out of bounds"

head :: Vec a ( 'Succ n) -> a
head (VCons x _) = x

append :: Vec a n -> Vec a m -> Vec a (n + m)
append VNil         v2 = v2
append (VCons x v1) v2 = VCons x (append v1 v2)

makeEven :: SNat n -> Vec a n -> Vec a (NextEven n)
makeEven n vec = case sIsEven n of
  STrue  -> vec
  SFalse -> case vec of
    VCons h t -> VCons h (VCons h t)

vTake :: Nat -> Vec a n -> [a]
vTake Zero     _           = []
vTake (Succ k) (VCons x v) = x : vTake k v

-- here we use < instead of <= to avoid defining it or compare
vTake' :: (m < n) ~ 'True => SNat m -> Vec a n -> [a]
vTake' m vec = vTake (fromSing m) vec

replicate1 :: SNat n -> a -> Vec a n
replicate1 SZero     _ = VNil
replicate1 (SSucc n) a = VCons a (replicate1 n a)

-- PROBLEM does not compile
-- replicate2 :: forall n a . SingI n => a -> Vec a n
-- replicate2 a = case (sing :: Sing n) of
--   SZero   -> VNil
--   SSucc _ -> VCons a (replicate2 a)

-- however, this works
replicate3 :: forall n a . SingI n => a -> Vec a n
replicate3 a = replicate1 sing a

-- PROBLEM does not compile
-- mkTrueList :: SNat n -> Vec Bool n
-- mkTrueList n = case singInstance n of
--   SingInstance -> replicate3 True

--
-- case study not done
--

--
-- personal sandbox
--

-- instance Show Nat where
--   show = show . natToInt

natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ n) = 1 P.+ natToInt n

instance (Show a) => Show (Vec a n) where
  show = show . toList

toList :: Vec a n -> [a]
toList VNil        = []
toList (VCons x v) = x : toList v

-- value of type Nat
v0 :: Nat
v0 = Zero
v1 :: Nat
v1 = Succ v0
v2 :: Nat
v2 = Succ v1
v3 :: Nat
v3 = Succ v2

-- types of the promoted Nat kind
type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2

-- singleton types for Nat
type SN0 = Sing N0
type SN1 = Sing N1
type SN2 = Sing N2
type SN3 = Sing N3

-- values of the singleton types
sN0 :: SN0
sN0 = SZero
sN1 :: SN1
sN1 = SSucc sN0
sN2 :: SN2
sN2 = SSucc sN1
sN3 :: SN3
sN3 = SSucc sN2

vec0 :: Vec Int N0
vec0 = VNil

vec1 :: Vec Int N2
vec1 = VCons 77 (VCons 88 VNil)

vec2 :: Vec Int N3
vec2 = VCons 44 (VCons 55 (VCons 66 VNil))

vec3 :: Vec Int N3
vec3 = replicate1 sN3 99

val1 :: Int
-- KO
-- val1 = nth sN2 vec1
-- OK
-- val1 = nth sN1 vec1
-- OK
val1 = nth sN0 vec1

testcase :: IO ()
testcase = do
  putStrLn "\n*** singleton paper\n"
  if v1 < v2 then putStrLn "1 < 2 (Nat)" else print "error"
  print vec1
  print val1
  print $ head vec1
  -- print $ head vec0
  print $ v1 + v2
  print $ append vec1 vec2
  print $ makeEven sN3 vec2
  print $ vTake v2 vec2
  print $ vTake' sN2 vec2
  print vec3
  putStrLn "\nsingleton paper passed (some errors possibly due to the version?, case study not tried)"
