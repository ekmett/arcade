module Rogue.Events (
    DamageType(..)
  , MobEvent(..)
  , Occurence(..)
  , Action(..)
  ) where

import Data.Int
import Data.Text

import Rogue.Location
import Rogue.Mob.Id

data DamageType =
    Smash
  | Slash
  deriving (Read,Show,Eq)

data MobEvent =
    MEDamage DamageType Int64
  deriving (Read,Show,Eq)

-- Things mobs do and tell the GameEngine to impliment
data Occurence =
    Die
    -- At a location, with a volume
  | Noise Location Int Text
    -- Send an attach the target's way
  | Attack MobId DamageType Int64
  deriving (Read,Show,Eq)

-- Things the mob is told to do by an Inteligence (Artificial or Biological)
data Action =
    Meditate
  | LoseConcentration
  deriving (Read,Show,Eq)
  