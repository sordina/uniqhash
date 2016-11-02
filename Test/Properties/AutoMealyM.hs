module Test.Properties.AutoMealyM where

import Data.Machine
import Data.Machine.MealyM
import Control.Arrow

prop_arr_1 :: Bool
prop_arr_1 = expected == result
  where
  result   = run $ source [1..10] ~> autoMealyM ((arrPure id &&& arr succ) >>> (arr succ *** arr pred))
  expected = [(2,1),(3,2),(4,3),(5,4),(6,5),(7,6),(8,7),(9,8),(10,9),(11,10 :: Int)]

prop_scan_1 :: Bool
prop_scan_1 = expected == result
  where
  result = run $ source items ~> autoMealyM (scanMealy (+) 1) ~> taking 9
  expected = take 9 $ scanl (+) 1 items
  items = [1..10]
