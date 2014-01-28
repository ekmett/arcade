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
  = Health
  | Endurance
  | Stun
  | Stat !Text deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance Hashable Stat

data Stats a = Stats
  { _health, _endurance, _stun :: a
  , _otherStats :: Map Text a
  } deriving (Functor,Typeable)

makeClassy ''Stats

stat :: (Num a, Eq a) => Stat -> Lens' (Stats a) a
stat Health    = health
stat Endurance = endurance
stat Stun      = stun
stat (Stat n)  = otherStats. at n . non 0
