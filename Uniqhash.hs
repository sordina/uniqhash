module Main (main) where

import qualified Text.Uniqhash as U
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  U.main
