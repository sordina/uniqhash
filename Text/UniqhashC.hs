{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A library for checking that the contents of files have changed.

module Text.UniqhashC (uniqmd5C) where

import Control.Exception
import GHC.IO.Exception
import Control.Monad
import Control.Arrow
import Control.Applicative
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Digest.Pure.MD5

import qualified Data.ByteString as B
import qualified Data.Map        as M

-- | Takes a lazy-list of filenames and for each filename,
-- checks if the contents of that file have changed, and if so,
-- outputs the filename.
uniqmd5C :: [String] -> IO [String]
uniqmd5C = fmap (dedup . (id &&& maps)) . mapML check

hashBStoString :: B.ByteString -> MD5Digest
hashBStoString = hash'

-- TODO: Show might be less efficient here than another function...
check :: String -> IO (String, String)
check s = (s,) . show . hashBStoString <$> readIfExists s

dedup :: (Ord a, Eq b) => ([(a,b)], [M.Map a b]) -> [a]
dedup = concat . uncurry (zipWith look)

look :: (Ord a, Eq b) => (a,b) -> M.Map a b -> [a]
look (k,v) m | M.lookup k m == Just v = []
             | otherwise              = [k]

maps :: Ord a => [(a,b)] -> [M.Map a b]
maps = scanl (flip (uncurry M.insert)) M.empty

mapML :: (a -> IO b) -> [a] -> IO [b]
mapML _ [] = return []
mapML f (a:as) = liftM2 (:) (f a) (unsafeInterleaveIO (mapML f as))

readIfExists :: FilePath -> IO B.ByteString
readIfExists fileName = if blank fileName then return ""
                                          else B.readFile fileName `catch` handler

handler :: IOException -> IO B.ByteString
handler e = case ioe_type e of InappropriateType -> hPutStrLn stderr ("Error: " ++ show e) >> return ""
                               NoSuchThing       -> hPutStrLn stderr ("Error: " ++ show e) >> return ""
                               _                 -> throwIO e

blank :: String -> Bool
blank ""       = True
blank (' ':xs) = blank xs
blank _        = False
