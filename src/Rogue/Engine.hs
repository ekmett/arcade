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
import Control.Concurrent
import Data.Time
import Data.IORef
import System.Mem.Weak
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import System.Random.Mersenne.Pure64 (PureMT, newPureMT)
import Data.Table as Table
import Control.Monad.State
import Data.Text (Text)
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
  , _mobEvents   :: Map MobId (Set MobEvent)
  , _morge      :: Set Mob
  }

makeLenses ''GameState

type GameEngine = IORef GameState

gameEngineG :: Text
gameEngineG = "Game Engines"

startGame :: Monitor -> IO GameEngine
startGame mon = do
  tG <- gauge gameEngineG mon
  inc tG
  mt <- newPureMT
  ge <- newIORef (GameState Table.empty PQ.empty mt Map.empty Set.empty)
  wge <- mkWeakIORef ge (return ())
  void . forkIO $ gameLoop tG wge
  return ge

inGame :: GameEngine -> Act GameState GameState Identity a -> IO a
inGame ge m = atomicModifyIORef' ge ((\(a, b) -> (b, a)) . runIdentity . runAct m)

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  now <- getCurrentTime
  inGame ge $ gameAddMob now (mobify p)    

gameAddMob :: Monad m => UTCTime -> Mob -> Act GameState GameState m ()
gameAddMob now m = do
  let mid = m ^. mobId
  mm <- use (mobs.at mid)
  case mm of
    Just _ -> do
      -- TODO LoseConcentration here.
      return ()
    Nothing -> do
      mobs %= Table.insert m
      gs <- get
      let d = delayTillTick gs mid
      updateQueue %= PQ.insert (d `addUTCTime` now) mid

gameRemoveMob :: Monad m => MobId -> Act GameState GameState m ()
gameRemoveMob mid = do
  mm <- use (mobs.at mid)
  case mm of
    Nothing -> return ()
    Just m -> do
      mobs %= Table.delete m
      updateQueue %= PQ.filter (== mid)
      mobEvents %= Map.delete mid
      morge %= Set.insert m

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
          (ugs & randSrc .~ nr, maybe (1 `addUTCTime` now) (^. _1) $ PQ.getMin (gs ^. updateQueue))
        where (ugs, nr) = runState ?? (gs ^. randSrc) $ execAct ?? gs $ do
                os <- focus (mobs.at mid.traverse) $ do
                  onTick
                  er <- concatMapM applyEvent es
                  pe <- postEvents
                  return $ pe++er
                -- Apply other occurences, then
                case Die `elem` os of
                  True -> gameRemoveMob mid
                  False ->
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
