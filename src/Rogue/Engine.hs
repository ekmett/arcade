{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Engine
  ( GameEngine
  , startGame
  , joinPlayer
  , describeMob
  , gameEngineG
  ) where

import Control.Lens
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.Time
import Data.IORef
import System.Mem.Weak
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
import Rogue.Monitor
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

type GameEngine = IORef GameState

gameEngineG = "Game Engines"

startGame :: Monitor -> IO GameEngine
startGame mon = do
  tG <- gauge gameEngineG mon
  inc tG
  mt <- newPureMT
  ge <- newIORef (GameState Table.empty PQ.empty mt Map.empty)
  wge <- mkWeakIORef ge (return ())
  forkIO $ gameLoop tG wge
  return ge

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  now <- getCurrentTime
  let mid = p ^. mobId
  atomicModifyIORef' ge $ \gs ->
    case gs ^. mobs.at mid of
      Just _ -> 
        -- TODO LoseConcentration here.
        (gs, ())
      Nothing ->
        ((gs & mobs %~ Table.insert (mobify p)) 
         & updateQueue %~ PQ.insert (delayTillTick gs mid `addUTCTime` now) mid, ())

describeMob :: GameEngine -> MobId -> IO (Maybe Description)
describeMob ge mid = do
  gs <- readIORef ge
  return . fmap description $ gs ^. mobs.at mid

delayTillTick :: GameState -> MobId -> NominalDiffTime
delayTillTick _ _ = 1

-- Tick a single Mob
mobTick :: GameEngine -> IO UTCTime
mobTick ge = do
  now <- getCurrentTime
  es <- sample $ Categorical.fromWeightedList [(1::Float, [MEDamage Smash 11]), (1, [MEDamage Slash 6]), (1, [])]
  atomicModifyIORef' ge $ \gs ->
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

gameLoop :: Gauge -> Weak GameEngine -> IO ()
gameLoop tG wge = go
  where
    go = do
      mge <- deRefWeak wge
      case mge of
        Nothing -> do
          dec tG
        Just ge -> do
          t <- mobTick ge
          delayTill t
          go
