{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | A library for checking that the contents of files have changed.

-- module Text.UniqhashMachines (changedFiles, pipeline, main) where

-- TODO: Switch from Crypto.Hash to Cryptonite https://hackage.haskell.org/package/cryptonite-0.20/docs/Crypto-Hash-Algorithms.html
--
-- https://github.com/acowley/concurrent-machines/issues/3

module Data.Machine.MealyM where

import Data.Machine
import Control.Arrow
import Data.Pointed
import Data.Machine.Mealy
import qualified Control.Category as C
import Control.Monad.Trans

newtype MealyM m a b = MealyM { runMealyM :: a -> m (b, MealyM m a b) }

instance Functor m => Functor (MealyM m a) where
  {-# INLINE fmap #-}
  fmap f (MealyM m) = MealyM $ \a ->
    fmap (\(x,y) -> (f x, fmap f y)) (m a)

instance Pointed m => Pointed (MealyM m a) where
  {-# INLINE point #-}
  point b = r where r = MealyM (const (point (b, r)))

instance Applicative m => Applicative (MealyM m a) where
  {-# INLINE pure #-}
  pure b = r where r = MealyM (const (pure (b, r))) -- Stolen from Pointed
  MealyM m <*> MealyM n = MealyM $ \a ->
    do let (ma, na) = (m a, n a)
       (mb, mm) <- ma
       (nb, nm) <- na
       return (mb nb, mm <*> nm)

instance Monad m => Monad (MealyM m a) where
  {-# INLINE return #-}
  return = pure
  (>>=) :: MealyM m a b -> (b -> MealyM m a c) -> MealyM m a c
  (MealyM g :: MealyM m a b) >>= (f :: b -> MealyM m a c) = MealyM $ \(a :: a) ->
    do (b, MealyM h) <- g a
       runMealyM (f b) a

instance Monad m => C.Category (MealyM m) where
  {-# INLINE id #-}
  id = MealyM $ \a -> return (a, C.id)
  MealyM bc . MealyM ab = MealyM $ \a ->
    do (b, nab) <- ab a
       (c, nbc) <- bc b
       return (c, nbc C.. nab)

instance Monad m => Arrow (MealyM m) where
  {-# INLINE arr #-}
  arr f = r where r = MealyM (\a -> return (f a, r))

{-
newtype MealyM m a b = MealyM { runMealyM :: a -> m (b, MealyM m a b) }

instance Arrow Mealy where
  arr f = r where r = Mealy (\a -> (f a, r))
  {-# INLINE arr #-}
  first (Mealy m) = Mealy $ \(a,c) -> case m a of
    (b, n) -> ((b, c), first n)

class C.Category a => Arrow (a :: * -> * -> *) where
    arr :: (b -> c) -> a b c
-}

autoMealyM :: Monad m => MealyM m a b -> ProcessT m a b
autoMealyM = construct . go
  where
  go (MealyM f) = do
    a      <- await
    (b, m) <- lift $ f a
    yield b
    go m

class AutomatonM x where
  autoM :: Monad m => x m a b -> ProcessT m a b

instance AutomatonM MealyM where
  autoM = autoMealyM
