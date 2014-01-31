module Rogue.Mob.Basic (
    BasicMob
  ) where

import Control.Monad.Trans
import Control.Lens
import Data.Default
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.UUID.V1 as V1

import Rogue.Ascii
import Rogue.Utils
import Rogue.Classes
import Rogue.Description
import Rogue.Location
import Rogue.Stat
import Rogue.Bucket
import Rogue.Events
import Rogue.Mob.Id

data BasicMob =
    BasicMob { _bmId :: MobId, _bmLocation :: Location, _bmStats :: Stats, _bmBuckets :: Buckets, _bmAscii :: Char }
  deriving (Read,Show)

makeLenses ''BasicMob

health, actionPoints :: BucketName
health = "hp"
actionPoints = "ap"

instance HasMobId BasicMob where
    mobId = bmId
    
instance HasLocation BasicMob where
  location = bmLocation

instance HasStats BasicMob where
  stats = bmStats
  
instance HasBuckets BasicMob where
  buckets = bmBuckets

instance ASCII BasicMob where
  ascii b = b ^. bmAscii

instance Rollable BasicMob where
  roll = do
      Just u <- liftIO V1.nextUUID
      return $ (withBuckets sizeBuckets fillBuckets $ BasicMob u def s Map.empty)
    where
      s = Map.fromList [(constitution, 10)]
