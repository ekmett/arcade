{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Mob
  ( MobId
  , Mob(..)
  , HasMob(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Function (on)
import Rogue.Body
import Rogue.Location
import Rogue.Stat

type MobId = Int

data Mob a = Mob
  { _mobId :: {-# UNPACK #-} !Int
  , _mobBody :: !Body
  , _mind :: a
  } deriving (Show,Read)

makeClassy ''Mob

instance Eq (Mob a) where
  (==) = (==) `on` _mobId

instance Ord (Mob a) where
  compare = compare `on` _mobId

instance Comonad Mob where
  extract (Mob _ _ a) = a
  extend f w@(Mob i b _) = Mob i b (f w)

instance Functor Mob where
  fmap f (Mob i b a) = Mob i b (f a)

instance Foldable Mob where
  foldMap f (Mob _ _ a) = f a

instance Traversable Mob where
  traverse f (Mob i b a) = Mob i b <$> f a

instance HasLocation (Mob a) where
  location = body.location

instance HasStats (Mob a) where
  stats = body.stats

instance FromJSON a => FromJSON (Mob a) where
  parseJSON (Object v) = Mob <$>
    v .: "id" <*>
    v .: "body" <*>
    v .: "mind"
  parseJSON _ = mzero

instance ToJSON a => ToJSON (Mob a) where
  toJSON (Mob i b m) = object ["id" .= i, "body" .= b, "mind" .= m]

instance HasBody (Mob a) where
  body = mobBody

instance Default a => Default (Mob a) where
  def = Mob def def def
