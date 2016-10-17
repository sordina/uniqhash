module Main (main) where

import qualified Text.Uniqhash as U
import qualified Conduit       as C
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  C.runConduit U.process
