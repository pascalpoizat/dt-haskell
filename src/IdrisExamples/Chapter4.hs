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

module IdrisExamples.Chapter4
where

import           Data.Type.Natural -- re-exports Data.Singletons

--
--
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
wheels Unicycle          = 1
wheels Bicycle           = 2
wheels (Motorcycle fuel) = 2
wheels (Car        fuel) = 4
wheels (Bus        fuel) = 4
wheels Tram              = 10
wheels ElectricCar       = 4

refuel :: Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car        fuel) = Car 100
refuel (Bus        fuel) = Bus 200

-- TODO: DataStore.idr
