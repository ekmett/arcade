{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Mob.Player
  ( Player(..)
  , HasPlayer(..)
  ) where

import Control.Monad.Trans
import Control.Lens
import Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.UUID.V1 as V1
import Rogue.Classes
import Rogue.Description
import Rogue.Location
import Rogue.Mob.Id

data Player =
    Player { _playerId :: MobId, _playerLocation :: Location }
  deriving (Read,Show)

makeClassy ''Player

instance HasMobId Player where
  mobId = playerId

instance HasLocation Player where
  location = playerLocation

instance Rollable Player where
  roll = do
    Just u <- liftIO V1.nextUUID
    return $ Player u def

instance OnTick Player where
  onTick = id

instance HasDescription Player where
  description p =
    Description
    (T.concat ["Player ", T.pack . show $ p ^. mobId, " is a common adventurer and not long for this world."])
    Map.empty
    Map.empty
