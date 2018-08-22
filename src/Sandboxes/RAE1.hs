-- {-# LANGUAGE ConstraintKinds           #-}
-- {-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DefaultSignatures         #-}
-- {-# LANGUAGE EmptyCase                 #-}
-- {-# LANGUAGE ExplicitForAll            #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE FlexibleInstances         #-}
-- {-# LANGUAGE GADTs                     #-}
-- {-# LANGUAGE InstanceSigs              #-}
-- {-# LANGUAGE KindSignatures            #-}
-- {-# LANGUAGE MultiParamTypeClasses     #-}
-- {-# LANGUAGE NoImplicitPrelude         #-}
-- {-# LANGUAGE PolyKinds                 #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE StandaloneDeriving        #-}
-- {-# LANGUAGE TemplateHaskell           #-}
-- {-# LANGUAGE TypeApplications          #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE TypeInType                #-}
-- {-# LANGUAGE TypeOperators             #-}
-- {-# LANGUAGE UndecidableInstances      #-}

module Sandboxes.RAE1
where

--
-- examples from reference:
-- Dependent Types in Haskell: Theory and Practice.
-- R. A. Eisenberg, PhD thesis, University of Pennsylvania, 2016. 
-- https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs
--

--
-- personal sandbox
--
