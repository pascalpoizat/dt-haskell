{-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE StandaloneDeriving        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Matrix
where

import           Data.Vect

type Matrix n m a = Vect n (Vect m a)

{-
testMatrix :: Matrix n2 n3 Double
testMatrix = ((1) :- (2) :- (3) :- Nil) :-
             ((4) :- (5) :- (6) :- Nil) :-
             Nil
-}
