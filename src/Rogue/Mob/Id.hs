module Rogue.Mob.Id
  ( MobId
  , HasMobId(..)
  ) where

import Control.Lens
import Data.UUID (UUID)

type MobId = UUID

class HasMobId t where
  mobId :: Lens' t MobId

instance HasMobId UUID where
  mobId = id
