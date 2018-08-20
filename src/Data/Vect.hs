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

module Data.Vect
where

import           Data.Singletons.TH
import           Data.Type.Natural
import           Prelude            hiding (head, replicate, tail, zipWith)

-- TODO: find why (a :: *) does not work
data Vect (size :: Nat) a where
  Nil :: Vect Z a
  (:-) :: a -> Vect n a -> Vect (S n) a
infixr 5 :-

deriving instance Eq a => Eq (Vect n a)

toList :: Vect n a -> [a]
toList Nil      = []
toList (a :- v) = a : toList v

--fromList :: SNat n -> [a] -> Maybe (Vector n a)
--fromList SZ _ = Just Nil
--fromList (SS n) (x :: xs) = (x :-) <$> fromList n xs
--fromList _ _  = Nothing

--fromList' :: SingI n => [a] -> Vector n a
--fromList' = fromList sing

instance Show a => Show (Vect n a) where
  showsPrec p = showsPrec p . toList

append :: Vect n a -> Vect m a -> Vect (n + m) a
append Nil v        = v
append (a :- v1) v2 = a :- append v1 v2

head :: Vect (S n) a -> a
head (a :- v) = a

tail :: Vect (S n) a -> a
tail (a :- v) = a

replicate :: SNat n -> a -> Vect n a
replicate SZ _     = Nil
replicate (SS n) a = a :- replicate n a

zipWithSame :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWithSame f Nil Nil             = Nil
zipWithSame f (a :- v1) (b :- v2) = f a b :- zipWithSame f v1 v2

zipWith :: (a -> b -> c) -> Vect n a -> Vect m b -> Vect (Min n m) c
zipWith f Nil Nil             = Nil
zipWith f Nil (b :- v)        = Nil
zipWith f (a :- v) Nil        = Nil
zipWith f (a :- v1) (b :- v2) = f a b :- zipWith f v1 v2

zipSame :: Vect n a -> Vect n b -> Vect n (a, b)
zipSame = zipWithSame (,)

zip :: Vect n a -> Vect m b -> Vect (Min n m) (a, b)
zip = zipWith (,)
