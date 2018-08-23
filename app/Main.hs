{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Prelude                        ( IO )
-- import           TestCase1
import qualified Sandboxes.RAE1.SingletonPaper as RAE1_1
                                                ( testcase )
import qualified Sandboxes.RAE3.Chapter2       as RAE3_2
                                                ( testcase )
import qualified Sandboxes.RAE3.Chapter3       as RAE3_3
                                                ( testcase )
import qualified Sandboxes.RAE3.Chapter7       as RAE3_7
                                                ( testcase )

main :: IO ()
main = do
  RAE1_1.testcase
  RAE3_2.testcase
  RAE3_3.testcase
  RAE3_7.testcase

