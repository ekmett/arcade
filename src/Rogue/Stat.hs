{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Rogue.Stat
  ( Stat(..)
  , Stats(..)
  , HasStats(..)
  , stat
  ) where

import Control.Lens
import Data.Default
import Data.Hashable
import Data.Int
import Data.Map
import Data.Monoid
import Data.Text
import Data.Typeable
import GHC.Generics
import Data.Aeson

data Stat
  = Health
  | MaxHealth
  | Endurance
  | MaxEndurance
  | Stun
  | MaxStun
  | Stat !Text
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance Hashable Stat
instance FromJSON Stat
instance ToJSON Stat

data Stats = Stats
  { _health,    _maxHealth
  , _endurance, _maxEndurance
  , _stun,      _maxStun :: Int64
  , _otherStats :: Map Text Int64
  } deriving (Eq,Ord,Read,Show,Typeable,Generic)

instance FromJSON Stats
instance ToJSON Stats

instance Default Stats where
  def = Stats def def
              def def
              def def
              mempty

makeClassy ''Stats

stat :: HasStats s => Stat -> Lens' s Int64
stat Health       = health
stat Endurance    = endurance
stat Stun         = stun
stat MaxHealth    = maxHealth
stat MaxEndurance = maxEndurance
stat MaxStun      = maxStun
stat (Stat n)     = otherStats.at n.non 0
