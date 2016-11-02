{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | A library for checking that the contents of files have changed.

module Text.Uniqhash (changedFiles, process, main) where

import qualified Text.UniqhashMachines as UM
import qualified Data.Machine.Process  as M

main :: IO ()
main = UM.main

changedFiles :: M.ProcessT IO FilePath FilePath
changedFiles = UM.changedFiles

process :: forall a. M.ProcessT IO a ()
process = UM.pipeline
