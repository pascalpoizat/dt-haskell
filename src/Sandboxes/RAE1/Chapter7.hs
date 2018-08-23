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
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fprint-explicit-kinds           #-}
{-# OPTIONS_GHC -Wunticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wunused-type-patterns           #-}

module Sandboxes.RAE1.Chapter7 where

import           Data.Kind                      ( Type )
import           Prelude                 hiding ( (+) )

--
-- examples from reference:
-- Dependent Types in Haskell: Theory and Practice.
-- R. A. Eisenberg, PhD thesis, University of Pennsylvania, 2016.
-- https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs
--

--
-- foralls sometime cause parsing errors (hlint), see Proxy
-- inconclusive (my solutions for missing things are they ok?) -> skip to RAE2
--

--
-- things already in GHC 8
--

data Proxy :: forall k . k -> Type where
-- data Proxy :: k -> Type where
    Proxy :: forall k (a :: k) . Proxy a

data T where
  MkT :: forall k (a :: k) . k -> Proxy a -> T

data G :: forall k . k -> Type where
  GInt :: G Int
  GMaybe :: G Maybe

class HTestEquality (f :: forall k . k -> Type) where
  hTestEquality :: forall k1 k2 (a :: k1) (b :: k2) . f a -> f b -> Maybe (a :≈: b)
instance HTestEquality TypeRep where
  hTestEquality = eqT

--
-- from section 3.2 and ref [75]
-- not clear if ok because things were missing there
--

data Dynamic where
  Dyn :: TypeRep a -> a -> Dynamic

toDynamic :: Typeable a => a -> Dynamic
toDynamic x = Dyn typeRep x

fromDynamic :: forall d . Typeable d => Dynamic -> Maybe d
fromDynamic (Dyn ra x) = do
  HRefl <- eqT ra (typeRep :: TypeRep d)
  return x

cast :: forall a b . (Typeable a, Typeable b) => a -> Maybe b
cast x = do
  HRefl <- eqT (typeRep :: TypeRep a) (typeRep :: TypeRep b)
  return x

data TypeRep (a :: k) where
  TrApp :: TypeRep a -> TypeRep b -> TypeRep (a b)
  TrTyCon :: TyCon a -> TypeRep (a :: k)

deriving instance Show (TyCon a)
deriving instance Show (TypeRep a)

class Typeable a where
  typeRep :: TypeRep a

data AppResult (t :: k) where
  App :: forall k1 k (a :: k1 -> k) (b :: k1) .
    TypeRep a -> TypeRep b -> AppResult (a b)

eqT :: forall k1 k2 (a :: k1) (b :: k2)
     . TypeRep a
    -> TypeRep b
    -> Maybe (a :≈: b)
eqT (TrTyCon t1) (TrTyCon t2) = eqTyCon t1 t2
eqT (TrApp a1 b1) (TrApp a2 b2)
  | Just HRefl <- eqT a1 a2, Just HRefl <- eqT b1 b2 = Just HRefl
eqT _ _ = Nothing

data (a :: k1) :≈: (b :: k2) where
  HRefl :: forall k (a :: k) . a :≈: a

--
-- Nat and "generated" things
--

data Nat = Zero | Succ Nat

deriving instance Eq Nat
deriving instance Show Nat

data TyCon (a :: k) where
  TNat :: TyCon Nat

eqTyCon :: forall (a :: k1) (b :: k2) . TyCon a -> TyCon b -> Maybe (a :≈: b)
eqTyCon TNat TNat = Just HRefl

instance Typeable Nat where
  typeRep = TrTyCon TNat

-- usage?

--
-- personal sandbox
--

testcase :: IO ()
testcase = do
  print "RAE1, chapter 7 inconclusive"

