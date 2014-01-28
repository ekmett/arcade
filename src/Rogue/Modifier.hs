module Rogue.Modifier
  ( Modifier(..)
  , modified
  ) where

import Data.Monoid

data Modifier = Modifier { _modDelta, _modBonus, _modPenalty :: {-# UNPACK #-} !Double }

instance Monoid Modifier where
  Modifier a b c `mappend` Modifier d e f = Modifier (a + d) (b + e) (c + f)
  mempty = Modifier 0 0 0

modified :: Modifier -> Double -> Double
modified (Modifier a b c) r = (r + a) * (1 + b) / (1 + c)


