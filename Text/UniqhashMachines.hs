{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | A library for checking that the contents of files have changed.

-- module Text.UniqhashMachines (changedFiles, pipeline, main) where

-- TODO: Switch from Crypto.Hash to Cryptonite https://hackage.haskell.org/package/cryptonite-0.20/docs/Crypto-Hash-Algorithms.html
--
-- https://github.com/acowley/concurrent-machines/issues/3

module Text.UniqhashMachines where

import System.IO
import Data.Machine
import System.IO.Machine
import System.Directory (doesFileExist)
import Data.Machine.MealyM
import Control.Arrow

import qualified Crypto.Hash         as CH
import qualified Data.ByteString     as BS
import qualified Data.Map            as M

type HASH = CH.Digest CH.SHA256

main :: IO ()
main = runT_ pipeline

pipeline :: ProcessT IO a ()
pipeline = sourceHandle byLine stdin
        ~> changedFiles
        ~> sinkIO putStrLn

changedFiles :: ProcessT IO FilePath FilePath
changedFiles = autoMealyM (fingerprint >>> detectChanges) ~> filterJust

fingerprint :: MealyM IO FilePath (FilePath, Maybe HASH)
fingerprint = arr id &&& hashPipe

detectChanges :: (Ord a, Eq b, Monad m) => MealyM m (a, b) (Maybe a)
detectChanges = (arr id &&& cache) >>> arr (uncurry retrieve)

-- Note: Unused in main
emitChanges :: (Ord a, Eq b) => Process (a, b) a
emitChanges = autoMealyM detectChanges ~> filterJust

filterJust :: Process (Maybe a) a
filterJust = repeatedly $ do
  m <- await
  case m of Just v  -> yield v
            Nothing -> return ()

hashPipe :: MealyM IO FilePath (Maybe HASH)
hashPipe = arrM hash

hash :: FilePath -> IO (Maybe HASH)
hash f = do
  e <- doesFileExist f
  if e then fmap (Just . CH.hash) (BS.readFile f)
       else return Nothing

cache :: (Monad m, Ord k) => MealyM m (k, v) (M.Map k v)
cache = scanMealy (flip (uncurry M.insert)) M.empty

retrieve :: (Ord k, Eq v) => (k, v) -> M.Map k v -> Maybe k
retrieve (k,v) m | M.lookup k m == Just v = Nothing
retrieve (k,_) _                          = Just k
