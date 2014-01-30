{-# LANGUAGE TemplateHaskell #-}
module Rogue.Engine
  ( GameEngine
  , startGame
  , joinPlayer
  , describeMob
  ) where

import Control.Lens
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.Time
import Data.IORef
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
import Data.Table (Table)
import Data.Table as Table
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Random
import qualified Data.Random.Distribution.Categorical as Categorical

import Rogue.Act
import Rogue.Utils
import Rogue.Classes
import Rogue.Time
import Rogue.Events
import Rogue.Description
import Rogue.Mob
import Rogue.Mob.Id
import Rogue.Mob.Player

data GameState = GameState
  { _mobs        :: Table Mob
  , _updateQueue :: MinPQueue UTCTime MobId
  , _randSrc     :: PureMT
  , _mobQueue    :: Map MobId (Set MobEvent)
  }

makeLenses ''GameState

data GameEngine = GameEngine
  { _gameRef :: IORef GameState
  }

makeLenses ''GameEngine

startGame :: IO GameEngine
startGame = do
  mt <- newPureMT
  ge <- GameEngine <$> newIORef (GameState Table.empty PQ.empty mt Map.empty)
  forkIO $ gameLoop ge
  return ge

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  now <- getCurrentTime
  let mid = p ^. mobId
  atomicModifyIORef' (ge ^. gameRef) $ \gs ->
    case gs ^. mobs.at mid of
      Just _ -> 
        -- TODO LoseConcentration here.
        (gs, ())
      Nothing ->
        ((gs & mobs %~ Table.insert (mobify p)) 
         & updateQueue %~ PQ.insert (delayTillTick gs mid `addUTCTime` now) mid, ())

describeMob :: GameEngine -> MobId -> IO (Maybe Description)
describeMob ge mid = do
  gs <- readIORef (ge ^. gameRef)
  return . fmap description $ gs ^. mobs.at mid

delayTillTick :: GameState -> MobId -> NominalDiffTime
delayTillTick _ _ = 1

-- Tick a single Mob
mobTick :: GameEngine -> IO UTCTime
mobTick ge = do
  now <- getCurrentTime
  es <- sample $ Categorical.fromWeightedList [(1::Float, [MEDamage Smash 3]), (1, [MEDamage Slash 2]), (3, [])]
  atomicModifyIORef' (ge ^. gameRef) $ \gs ->
    case PQ.minViewWithKey (gs ^. updateQueue) of
      -- we never want to more then a second without checking.
      Nothing -> (gs, 1 `addUTCTime` now)
      -- Not time yet! To soon! To soon!
      Just ((t, _), _) | t > now -> (gs, t)
      -- Just right (or late ... fashionably so ...)
      -- We ignore t because even if we're late, we don't want to schedule them earlier again
      -- since if we're lagged and catch up we don't want a flurry of actions.
      Just ((_, mid), qr) -> 
          (ugs, maybe (1 `addUTCTime` now) (^. _1) $ PQ.getMin (gs ^. updateQueue))
        where ugs = runIdentity $ execAct ?? gs $ do
                focus (mobs.at mid.traverse) $ do
                  onTick
                  concatMapM applyEvent es
                -- Reschedule this Mob
                updateQueue .= PQ.insert (delayTillTick gs mid `addUTCTime` now) mid qr

gameLoop :: GameEngine -> IO ()
gameLoop ge = forever $ do
  t <- mobTick ge
  delayTill t
