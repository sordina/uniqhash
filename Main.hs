{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Text.Uniqhash (uniqmd5)
import System.IO

main :: IO ()
main = hSetBuffering stdin  LineBuffering
   >>  hSetBuffering stdout LineBuffering
   >>  getContents
   >>= uniqmd5 . lines
   >>= mapM_ putStrLn
