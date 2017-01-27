
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Needle where

import Control.Arrow.Needle
import Data.Machine.MealyT
import Text.UniqhashMachines
import Data.Machine
import System.IO
import System.IO.Machine
import qualified Data.Map as M

{-|

Like Magic:

*Needle> embedMealyT n (words "a b c a a a")
[Just "a",Just "b",Just "c",Nothing,Nothing,Nothing]

-}

n :: MealyT IO FilePath (Maybe FilePath)
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

example :: IO ()
example = runT_ $ sourceHandle byLine stdin ~> autoT n ~> sinkIO print
