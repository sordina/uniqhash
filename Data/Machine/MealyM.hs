{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- https://github.com/acowley/concurrent-machines/issues/3

module Data.Machine.MealyM where

import Data.Machine
import Control.Arrow
import Data.Pointed
import qualified Control.Category as C
import Control.Monad.Trans
import Control.Monad.Identity

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
    do (b, MealyM _h) <- g a
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
  first (MealyM m) = MealyM $ \(a,c) ->
    do (b, n) <- m a
       return ((b, c), (first n))

arrPure :: (a -> b) -> MealyM Identity a b
arrPure = arr

arrM :: Functor m => (a -> m b) -> MealyM m a b
arrM f = r where r = MealyM $ \a -> fmap (,r) (f a)

-- TODO: Mealy -> MealyM

scanMealy :: Monad m => (a -> b -> a) -> a -> MealyM m b a
scanMealy f a = MealyM (\b -> return (a, scanMealy f (f a b)))

scanMealyM :: Functor m => (a -> b -> m a) -> a -> MealyM m b a
scanMealyM (f :: a -> b -> m a) a = MealyM $ \b ->
  do x <- f a b
     return (a, scanMealyM f x)

autoMealyMImpl :: Monad m => MealyM m a b -> ProcessT m a b
autoMealyMImpl = construct . go
  where
  go (MealyM f) = do
    a      <- await
    (b, m) <- lift $ f a
    yield b
    go m

class AutomatonM x where
  autoMealyM :: Monad m => x m a b -> ProcessT m a b

instance AutomatonM MealyM where
  autoMealyM = autoMealyMImpl
