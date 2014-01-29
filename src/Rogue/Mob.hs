{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Rogue.Mob
  ( MobId
  , Mob(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Function (on)
import Data.Table

import Rogue.Location
import Rogue.Stat
import Rogue.Identifiers

import Rogue.Mob.Player

data Mob = 
    MobPlayer { _player :: Player }
  deriving (Show,Read)
  
makeLenses ''Mob

class Mobify a where  
  mobify :: a -> Mob

instance Mobify Player where
  mobify = MobPlayer

getMobId :: Mob -> MobId
getMobId (MobPlayer p) = view mobId p

setMobId :: Mob -> MobId -> Mob
setMobId (MobPlayer p) i = MobPlayer $ set mobId i p

getMobLocation :: Mob -> Location
getMobLocation (MobPlayer p) = view location p

setMobLocation :: Mob -> Location -> Mob
setMobLocation (MobPlayer p) i = MobPlayer $ set location i p

instance HasMobId Mob where
  mobId = lens getMobId setMobId

instance HasLocation Mob where
  location = lens getMobLocation setMobLocation

instance Eq Mob where
  (==) = (==) `on` (view mobId)

instance Ord Mob where
  compare = compare `on` (view mobId)

instance Tabular Mob where
  type PKT Mob = MobId

  data Key k Mob b where
    MobId :: Key Primary Mob MobId
    MobLocation :: Key Supplemental Mob Location

  data Tab Mob i = MobTab (i Primary MobId) (i Supplemental Location)

  fetch MobId = view mobId
  fetch MobLocation = view location

  primary = MobId
  primarily MobId r = r

  mkTab f = MobTab <$> f MobId <*> f MobLocation
  forTab (MobTab x y) f = MobTab <$> f MobId x <*> f MobLocation y
  ixTab (MobTab x _) MobId = x
  ixTab (MobTab _ x) MobLocation = x
