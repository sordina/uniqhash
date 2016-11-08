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

module Text.UniqhashStreaming where

import Prelude hiding (map, mapM)

import System.IO
import Streaming
import Streaming.Prelude
import System.Directory (doesFileExist)

import qualified Crypto.Hash         as CH
import qualified Data.ByteString     as BS
import qualified Data.Map            as M

type HASH = CH.Digest CH.SHA256

main :: IO ()
main = pipeline

pipeline :: IO ()
pipeline = stdoutLn $ changedFiles $ stdinLn

changedFiles :: Stream (Of String) IO r -> Stream (Of String) IO r
changedFiles = catMaybes . detectChanges . fingerprint

fingerprint :: Stream (Of FilePath) IO r -> Stream (Of (FilePath, Maybe HASH)) IO r
fingerprint = rezip . hashPipe . copy

detectChanges :: (Ord a, Eq b, Monad m) => Stream (Of (a, b)) m r -> Stream (Of (Maybe a)) m r
detectChanges = map (uncurry retrieve) . rezip . cache . copy

hashPipe :: (MonadIO m) => Stream (Of FilePath) m r -> Stream (Of (Maybe HASH)) m r
hashPipe = mapM (liftIO . hash)

hash :: FilePath -> IO (Maybe HASH)
hash f = do
  e <- doesFileExist f
  if e then fmap (Just . CH.hash) (BS.readFile f)
       else return Nothing

cache :: (Monad m, Ord k) => Stream (Of (k, v)) m r -> Stream (Of (M.Map k v)) m  r
cache = scan (flip (uncurry M.insert)) M.empty id

retrieve :: (Ord k, Eq v) => (k, v) -> M.Map k v -> Maybe k
retrieve (k,v) m | M.lookup k m == Just v = Nothing
retrieve (k,_) _                          = Just k

rezip :: Monad m => Stream (Of b) (Stream (Of a) m) r -> Stream (Of (a, b)) m r
rezip = unfold (\s -> do
  x <- inspect (inspect s)
  case x of
    Left (Left r) -> return (Left r)
    Left (Right (_ :> sb)) -> do
      r <- effects (effects sb)
      return (Left r)
    Right (a :> sa) -> do
      y <- effects sa
      case y of
        Left r -> return (Left r)
        Right (b :> s') -> return (Right ((a, b) :> s')))

