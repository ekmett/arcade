module Rogue.Classes where

import Control.Monad.Trans
import Data.Random
import Control.Monad.State

import Rogue.Events

class Rollable a where
  roll :: MonadIO m => RVarT m a

class MobLike a where
--  onTick :: a -> a
  -- Mobs don't actually get to depend on 's', the overall game state.
  -- Just their own definition.
  -- Monad 'm' will need to be enhanced to access random numbers and such
  onTick :: (MonadRandom m, Monad m) => StateT a m ()
  applyEvent :: (MonadRandom m, Monad m) => MobEvent -> StateT a m [Occurence]
  postEvents :: (MonadRandom m, Monad m) => StateT a m [Occurence]
  