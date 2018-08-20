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
{-# LANGUAGE ViewPatterns              #-}

module Data.Matrix
where

import           Data.Vect
import           Prelude           (Num, (+))

type Matrix n m a = Vect n (Vect m a)

addVector :: Num a => Vect n a -> Vect n a -> Vect n a
addVector = zipWith (+)

add :: Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
add = zipWith addVector

{-
testMatrix :: Matrix n2 n3 Double
testMatrix = ((1) :- (2) :- (3) :- Nil) :-
             ((4) :- (5) :- (6) :- Nil) :-
             Nil
-}
