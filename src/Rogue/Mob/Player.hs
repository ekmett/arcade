{-# LANGUAGE TemplateHaskell #-}
module Rogue.Mob.Player (
    Player
  ) where

import Control.Monad.Trans
import Control.Lens
import qualified Data.UUID.V1 as V1
import Data.Default

import Rogue.Classes
import Rogue.Identifiers
import Rogue.Location

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
