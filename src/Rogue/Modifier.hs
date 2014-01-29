{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Rogue.Modifier
  ( Modifier(..)
  , HasModifier(..)
  , modified
  ) where

import Control.Lens
import Data.Default
import Data.Hashable
import Data.Semigroup
import Data.Typeable
import GHC.Generics

data Modifier = Modifier { _modDelta, _modBonus, _modPenalty :: {-# UNPACK #-} !Double }
  deriving (Eq,Ord,Show,Read,Generic,Typeable)

instance Hashable Modifier

makeClassy ''Modifier

instance Default Modifier where
  def = 0

instance Semigroup Modifier where
  (<>) = (+)

instance Monoid Modifier where
  mappend = (+)
  mempty = 0

instance Num Modifier where
  Modifier a b c + Modifier d e f = Modifier (a + d) (b + e) (c + f)
  Modifier a b c - Modifier d e f = Modifier (a - d) (b + f) (c + e)
  Modifier a b c * Modifier d e f = Modifier (a * d) (max b e) (max c f)
  negate (Modifier a b c) = Modifier (-a) c b
  abs (Modifier a b c)    = Modifier (abs a) b c
  signum (Modifier a b c) = Modifier (signum a) b c
  fromInteger a           = Modifier (fromInteger a) 0 0

modified :: Modifier -> Double -> Double
modified (Modifier a b c) r = (r + a) * (1 + b) / (1 + c)
