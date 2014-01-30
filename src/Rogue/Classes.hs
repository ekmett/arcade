module Rogue.Classes where

import Control.Monad.Trans
import Data.Random

class Rollable a where
  roll :: MonadIO m => RVarT m a

class OnTick a where
  onTick :: a -> a
