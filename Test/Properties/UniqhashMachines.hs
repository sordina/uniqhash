module Test.Properties.UniqhashMachines where

import Data.Machine
import Text.UniqhashMachines

prop_detectChanges :: Bool
prop_detectChanges = expected == result
  where
  expected = [1,1,2]
  result   = run $ source [(1 :: Int,'a'),(1,'a'),(1,'b'),(1,'b'),(2,'a'),(1,'b'),(2,'a')] ~> emitChanges
