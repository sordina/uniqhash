
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Needle where

import Control.Arrow.Needle
import Data.Machine.MealyM
import Text.UniqhashMachines

import qualified Data.Map as M

{-|

Like Magic:

*Needle> embedMealyM n (words "a b c a a a")
[Just "a",Just "b",Just "c",Nothing,Nothing,Nothing]

-}

n :: MealyM IO FilePath (Maybe FilePath)
n = [nd|

  }===\================\
      \                { tuple }==\=============\
      \=={ hashPipe }==/          \             { get }==>
                                  \=={ cache }==/
|]

tuple :: Arrow a => a (a1, b) (a1, b)
tuple = arr (uncurry (,))

get :: (Arrow a, Ord k, Eq v) => a ((k, v), M.Map k v) (Maybe k)
get = arr (uncurry retrieve)
