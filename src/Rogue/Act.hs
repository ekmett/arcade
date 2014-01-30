{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Rogue.Act
  ( Act(..)
  , runAct, execAct, evalAct
  , here
  , globally
  , locally
  , root
  , roots
  , focus
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State
import Data.Profunctor.Unsafe

newtype Act s t m a = Act { enact :: Lens' s t -> s -> t -> m (a, s, t) }

-- | A limited, but legal form of `zoom`
focus :: Monad m => LensLike' (Focusing m a) t u -> StateT u m a -> Act s t m a
focus l (StateT m) = Act $ \_ s t -> do
  (c, t') <- unfocusing $ l (Focusing #. m) t
  return (c, s, t')

runAct :: Monad m => Act s s m a -> s -> m (a, s)
runAct (Act m) s = do
  (a, _, t) <- m id s s
  return (a, t)

evalAct :: Monad m => Act s s m a -> s -> m a
evalAct m s = fst `liftM` runAct m s

execAct :: Monad m => Act s s m a -> s -> m s
execAct m s = snd `liftM` runAct m s

instance Monad m => Functor (Act s t m) where
  fmap f (Act m) = Act $ \ l s t -> do
    (a, s', t') <- m l s t
    return (f a, s', t')

instance Monad m => Applicative (Act s t m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Act s t m) where
  return a = Act $ \ _ s t -> return (a, s, t)
  Act m >>= k = Act $ \l s t -> do
    (a, s', t') <- m l s t
    enact (k a) l s' t'

instance Monad m => MonadState t (Act s t m) where
  get = Act $ \_ s t -> return (t, s, t)
  put t = Act $ \_ s _ -> return ((), s, t)
  state f = Act $ \_ s t -> case f t of
    (a, t') -> return (a, s, t')

instance MonadTrans (Act s t) where
  lift m = Act $ \_ s t -> do
    a <- m
    return (a, s, t)

instance MonadIO m => MonadIO (Act s t m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (Act s t m) where
  ask = lift ask
  local f (Act m) = Act $ \l s t -> local f $ m l s t

root :: Monad m => Act s t m s
root = Act $ \l s t -> do
  let s' = s & l .~ t
  return (s', s', t)

roots :: Monad m => (s -> a) -> Act s t m a
roots f = Act $ \l s t -> do
  let s' = s & l .~ t
  return (f s', s', t)

here :: Monad m => Act s t m (ReifiedLens' s t)
here = Act $ \ l s t -> return (Lens l, s, t)

globally :: Monad m => ALens' s t -> Act s t m a -> Act s u m a
globally l2 (Act m) = Act $ \l1 s1 t1 -> do
  let s2 = s1 & l1 #~ t1
  (a, s3, t3) <- m (cloneLens l2) s2 (s2 ^# l2)
  let s4 = s3 & l2 #~ t3
  return (a, s4, s4 ^# l1)

locally :: Monad m => ALens' t u -> Act s u m a -> Act s t m a
locally l' m = do
  Lens l <- here
  globally (l.cloneLens l') m
