{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Rogue.Stat
  ( Stat(..)
  , Stats(..)
  , HasStats(..)
  , stat
  ) where

import Control.Lens
import Data.Hashable
import Data.Map
import Data.Text
import Data.Typeable
import GHC.Generics

data Stat
  = MaxHealth
  | MaxEndurance
  | MaxStun
  | Stat !Text deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance Hashable Stat

data Stats a = Stats
  { _maxHealth, _maxEndurance, _maxStun :: a
  , _otherStats :: Map Text a
  } deriving (Functor,Typeable)

makeClassy ''Stats

stat :: (Num a, Eq a) => Stat -> Lens' (Stats a) a
stat MaxHealth    = maxHealth
stat MaxEndurance = maxEndurance
stat MaxStun      = maxStun
stat (Stat n)     = otherStats. at n . non 0
