{-# LANGUAGE RankNTypes #-}
module Rogue.Stat
  ( Stat
  , Stats
  , HasStats(..)
  , stat
  ) where

import Control.Lens
import Data.Int
import Data.Map
import Data.Text

type Stat = Text

type Stats = Map Stat Int64

class HasStats t where
  stats :: Simple Lens t Stats

stat :: HasStats s => Stat -> Lens' s Int64
stat n = stats.at n.non 0
