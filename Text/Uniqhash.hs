{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Uniqhash (uniqmd5) where

import Control.Exception
import GHC.IO.Exception
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Digest.OpenSSL.MD5
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString as B
import qualified Data.Map        as M

uniqmd5 :: [String] -> IO [String]
uniqmd5 = fmap (dedup . (id &&& maps)) . mapML check

check :: String -> IO (String, String)
check s = (s,) . md5sum <$> readIfExists s

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
readIfExists fileName = if null fileName then return ""
                                         else B.readFile fileName `catch` handler

handler :: IOException -> IO B.ByteString
handler e = case ioe_type e of InappropriateType -> hPutStrLn stderr ("Error: " ++ show e) >> return ""
                               NoSuchThing       -> hPutStrLn stderr ("Error: " ++ show e) >> return ""
                               _                 -> throwIO e
