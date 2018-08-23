{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Prelude                 (IO)
-- import           TestCase1
import qualified Sandboxes.RAE1.Chapter2 as RAE1_2 (testcase)
import qualified Sandboxes.RAE1.Chapter3 as RAE1_3 (testcase)
import qualified Sandboxes.RAE1.Chapter7 as RAE1_7 (testcase)

main :: IO ()
main = do
  RAE1_2.testcase
  RAE1_3.testcase
  RAE1_7.testcase

