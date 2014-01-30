{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Mob.Player (
    Player
  ) where

import Control.Monad.Trans
import Control.Lens
import qualified Data.UUID.V1 as V1
import Data.Default
import qualified Data.Text as T
import qualified Data.Map as Map

import Rogue.Classes
import Rogue.Identifiers
import Rogue.Location
import Rogue.Description

data Player =
    Player { _pId :: MobId, _pLocation :: Location }
  deriving (Read,Show)

makeLenses ''Player

instance HasMobId Player where
  mobId = pId

instance HasLocation Player where
  location = pLocation

instance Rollable Player where
  roll = do
    Just u <- liftIO $ V1.nextUUID
    return $ Player u def

instance OnTick Player where
  onTick = id

instance HasDescription Player where
  description p = 
    Description
    (T.concat ["Player ", T.pack . show $ p ^. mobId, " is a common adventurer and not long for this world."])
    Map.empty
    Map.empty
