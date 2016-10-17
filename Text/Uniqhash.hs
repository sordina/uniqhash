{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | A library for checking that the contents of files have changed.

module Text.Uniqhash (changedFiles, process, main) where

import Conduit
import Control.Exception
import System.IO

import qualified Crypto.Hash         as CH
import qualified Crypto.Hash.Conduit as CHC
import qualified Data.Map            as M

type HASH = CH.Digest CH.SHA256

main :: IO ()
main = runConduit process

process :: forall a c. ConduitM a c IO ()
process = stdinC
       .| linesUnboundedC
       .| filterC (not . null)
       .| changedFiles
       .| stdoutC'

-- Test

prop_expected_list_example :: Bool
prop_expected_list_example = result == expected
  where
    cond     = mapM_ yield input .| roll .| interest .| emission .| sinkList
    one      = 1 :: Int
    input    = (zip [one .. 10] [one .. 10] ++ zip [8..10] [8..10] ++ zip [8..14] [7..13])
    result   = runConduitPure cond
    expected = [1,2,3,4,5,6,7,8,9,10,8,9,10,11,12,13,14]

-- | Conduit for filenames.
-- Checks if the contents of that file have changed, and if so, yields the filename.
--
changedFiles :: Conduit FilePath IO FilePath
changedFiles = logExceptions (mapMC digestion) .| roll .| interest .| emission

digestion :: FilePath -> IO (FilePath, HASH)
digestion fn = (fn,) <$> CHC.hashFile fn

roll :: (Monad m, Ord k) => Conduit (k, v) m ((k, v), M.Map k v)
roll = pipeline .| pairs
  where
    pipeline = getZipConduit $ ZipConduit (mapC Left) <* ZipConduit (cache .| mapC Right)

interest :: (Ord k, Eq v, Monad m) => Conduit ((k, v), M.Map k v) m (Maybe k)
interest = mapC (uncurry retrieve)

emission :: Monad m => Conduit (Maybe k) m k
emission = do
  x <- await
  case x of Just (Just f) -> yield f >> emission
            Just Nothing  -> emission
            Nothing       -> return ()

-- Helpers

pairs :: Monad m => Conduit (Either l r) m (l,r)
pairs = do
  x <- await
  y <- await
  case (x,y) of (Just (Left x'), Just (Right y')) -> yield (x',y') >> pairs
                _                                 -> return ()

cache :: (Ord k, Monad m) => Conduit (k, v) m (M.Map k v)
cache = scanlC (flip (uncurry M.insert)) M.empty

retrieve :: (Ord k, Eq v) => (k, v) -> (M.Map k v) -> Maybe k
retrieve (f,h) m | M.lookup f m == Just h = Nothing
retrieve (f,_) _                          = Just f

-- Utilities

logExceptions :: Conduit a IO b -> Conduit a IO b
logExceptions c = catchC c (logExceptionThen (logExceptions c))

logExceptionThen :: Conduit a IO b -> Control.Exception.SomeException -> Conduit a IO b
logExceptionThen c e = liftIO (hPutStrLn stderr (show e)) >> c

stdoutC' :: ConduitM String o IO ()
stdoutC' = do
  ml <- await
  case ml of Just l  -> liftIO (putStrLn l) >> stdoutC'
             Nothing -> return ()
