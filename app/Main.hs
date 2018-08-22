{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Prelude                 (IO)
-- import           TestCase1
import qualified Sandboxes.RAE1.Chapter2 as RAE1_2 (testcase)

main :: IO ()
main = RAE1_2.testcase

