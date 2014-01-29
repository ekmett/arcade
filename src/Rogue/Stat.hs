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
import Data.Map
import Data.Monoid
import Data.Text
import Data.Typeable
import GHC.Generics
import Data.Aeson

data Stat
  = Health
  | Endurance
  | Stun
  | Stat !Text
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance Hashable Stat
instance FromJSON Stat
instance ToJSON Stat

data Stats a = Stats
  { _health, _endurance, _stun :: a
  , _otherStats :: Map Text a
  } deriving (Functor,Typeable,Generic)

deriving instance Show a => Show (Stats a)
deriving instance Read a => Read (Stats a)
instance FromJSON a => FromJSON (Stats a)
instance ToJSON a => ToJSON (Stats a)

instance Default a => Default (Stats a) where
  def = Stats def def def mempty

makeClassy ''Stats

stat :: (HasStats s a, Default a) => Stat -> Lens' s a
stat Health    = health
stat Endurance = endurance
stat Stun      = stun
stat (Stat n)  = otherStats.at n.anon def (const False)
