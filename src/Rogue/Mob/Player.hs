{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Mob.Player( 
    Player
  ) where

import Control.Monad.Trans
import Control.Lens
import Data.Default
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.UUID.V1 as V1

import Rogue.ASCII
import Rogue.Utils
import Rogue.Classes
import Rogue.Description
import Rogue.Location
import Rogue.Stat
import Rogue.Bucket
import Rogue.Events
import Rogue.Mob.Id

data Player =
    Player { _playerId :: MobId, _playerLocation :: Location, _playerStats :: Stats, _playerBuckets :: Buckets }
  deriving (Read,Show)

makeLenses ''Player

constitution :: Stat
constitution = "constitution"

health, stamina, actionPoints :: BucketName
health = "hp"
stamina = "stamina"
actionPoints = "ap"

instance HasMobId Player where
  mobId = playerId

instance HasLocation Player where
  location = playerLocation

instance HasStats Player where
  stats = playerStats

instance HasBuckets Player where
  buckets = playerBuckets

instance ASCII Player where
  ascii _ = '웃'

instance Rollable Player where
  roll = do
      Just u <- liftIO V1.nextUUID
      return $ (withBuckets sizeBuckets fillBuckets $ Player u def s Map.empty)
    where
      s = Map.fromList [(constitution, 10)]

sizeBuckets :: Player -> Player
sizeBuckets p = 
  setBucketMax actionPoints (isqrt (10 * p ^. stat constitution)) .
  setBucketMax stamina (10 * (p ^. stat constitution)) .
  setBucketMax health (p ^. stat constitution) $
  p

instance HasDescription Player where
  description p =
    Description
    (T.concat ["Player ", T.pack . show $ p ^. mobId, " is a common adventurer and not long for this world."])
    (p ^. stats)
    (p ^. buckets)
    (ascii p)

instance MobLike Player where
  onTick = do
    modify . withBuckets sizeBuckets $ \p ->
      addBucket (bucket health) (isqrt (p ^. stat constitution)) .
      pour (bucket stamina) (bucket actionPoints) (min 1 $ sqrt ((p ^. bucket health.current) / 10)) .
      -- Based on our old health
      addBucket (bucket stamina) (sqrt (p ^. bucket health.current)) $
      p
  postEvents = do
    h <- use (bucket health.current)
    if h <= 0
      then return [Die]
      else return []
  applyEvent (MEDamage Smash d) = do
    -- Smashing damage hurts AP and then health
    bucket actionPoints.current -= realToFrac d
    o <- use $ bucket actionPoints.current
    bucket actionPoints.current .= max 0 o
    bucket health.current -= abs (min 0 o)
    return []
  applyEvent (MEDamage Slash d) = do
    -- Slashing just hurts HP
    bucket health.current -= realToFrac d
    return []
