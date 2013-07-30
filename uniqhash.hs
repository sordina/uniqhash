{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Exception
import System.IO.Error
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Digest.OpenSSL.MD5
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString as B
import qualified Data.Map        as M

main :: IO ()
main = hSetBuffering stdin  LineBuffering
   >>  hSetBuffering stdout LineBuffering
   >>  getContents
   >>= uniqmd5 . lines
   >>= mapM_ putStrLn

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
readIfExists fileName = B.readFile fileName `catch` handler
  where handler e
          | isDoesNotExistError e = return ""
          | otherwise             = throwIO e
