module Rogue.Events where

import Data.Int
import Data.Text

import Rogue.Location

data DamageType =
    Smash
  | Slash
  deriving (Read,Show,Eq)

data MobEvent =
    MEDamage DamageType Int64
  deriving (Read,Show,Eq)

data Occurence =
    -- At a location, with a volume
    Noise Location Int Text
  deriving (Read,Show,Eq)

{-
data Occurence =
    Noise { _where :: Location, _what :: Text }
  | 
-}