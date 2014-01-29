module Rogue.Identifiers where

import Control.Lens
import Data.UUID (UUID)

type MobId = UUID

class HasMobId t where
  mobId :: Simple Lens t MobId

