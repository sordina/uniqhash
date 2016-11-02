module Test.Properties.AutoMealyM where

import Data.Machine
import Data.Machine.MealyM
import Control.Arrow
import Control.Monad.Identity
import qualified Control.Category as C

prop_arr_1 :: Bool
prop_arr_1 = expected == result
  where
  result   = run $ source [1..10] ~> autoMealyM ((arrPure id &&& arr succ) >>> (arr succ *** arr pred))
  expected = [(2,1),(3,2),(4,3),(5,4),(6,5),(7,6),(8,7),(9,8),(10,9),(11,10 :: Int)]

-- Should this output the last value?
prop_scan_1 :: Bool
prop_scan_1 = expected == result
  where
  result   = run $ source items ~> autoMealyM (scanMealy (+) 1) ~> taking 9
  expected = take 9 $ scanl (+) (1 :: Int) items
  items    = [1..10]

prop_length_id :: [()] -> Bool
prop_length_id xs = expected == result
  where
  expected = length xs
  result   = length $ run $ source xs ~> autoMealyM (C.id :: MealyM Identity a a)
