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
import Debug.Trace
import Data.Machine.MealyM

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
changedFiles = splitProdPair echo hashPipe
            ~> splitProdPair echo cache
            ~> interest
            ~> filterMaybe

splitProdPair :: Monad m => ProcessT m a b -> ProcessT m a c -> ProcessT m a (b,c)
splitProdPair = mergeProd echo

-- TODO: Cross-Product of misaligned yields and awaits
--
mergeProd :: Monad m => MachineT m k a -> ProcessT m a b -> ProcessT m a c -> MachineT m k (b,c)
mergeProd ma pb pc = MachineT $ do
  bv <- runMachineT pb
  cv <- runMachineT pc
  case (bv, cv) of
       (Stop, _) -> return Stop
       (_, Stop) -> return Stop
       (Yield bo bk, Yield co ck) -> return $ Yield (bo,co) (mergeProd ma bk ck)
       (Await bf Refl bff, Await cf Refl cff) -> runMachineT ma >>= \u -> case u of
          Stop          -> runMachineT $ mergeProd stopped bff cff
          Yield o k     -> runMachineT $ mergeProd k (bf o) (cf o)
          Await g kg fg -> return $ Await (\a -> mergeProd (g a) (MachineT (return bv)) (MachineT (return cv)))
                                          kg
                                          (mergeProd fg (MachineT (return bv)) (MachineT (return cv)))
       (Await _bf Refl _bff, Yield _co _ck) -> trace "hybrid case 1" (return Stop)
       (Yield _bo _bk, Await _cf Refl _cff) -> trace "hybrid case 2" (return Stop)

filterMaybe :: Process (Maybe a) a
filterMaybe = repeatedly $ do
  m <- await
  case m of Just v  -> yield v
            Nothing -> return ()

hashPipe :: ProcessT IO FilePath (Maybe HASH)
hashPipe = autoM hash

hash :: FilePath -> IO (Maybe HASH)
hash f = do
  e <- doesFileExist f
  if e then fmap (Just . CH.hash) (BS.readFile f)
       else return Nothing

cache :: (Ord k) => Process (k, v) (M.Map k v)
cache = scan (flip (uncurry M.insert)) M.empty

interest :: Eq v => Process ((FilePath, v), M.Map FilePath v) (Maybe FilePath)
interest = mapping (uncurry retrieve)

retrieve :: Eq v => (FilePath, v) -> M.Map FilePath v -> Maybe FilePath
retrieve (k,v) m | M.lookup k m == Just v = Nothing
retrieve (k,_) _                          = Just k
