{-# LANGUAGE TemplateHaskell #-}
module Rogue.Engine (
    GameEngine
  , startGame
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

import Rogue.Identifiers
import Rogue.Mob
import Rogue.Mob.Player

data GameState =
    GS {
      _mobs        :: Table Mob
    , _updateQueue :: MinPQueue UTCTime MobId
    , _randSrc     :: PureMT
    }

makeLenses ''GameState

data GameEngine =
  Engine {
      _gameState :: IORef GameState
    }

makeLenses ''GameEngine

startGame :: IO GameEngine
startGame = do
  mt <- newPureMT
  ge <- Engine <$> newIORef (GS Table.empty PQ.empty mt)
  forkIO $ gameLoop ge
  return ge

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  -- Queue the player for insertion into the game after the next tick.
  -- Don't return untill they're in the game?
  -- We don't want connections hanging though.
  error "We don't want no sticking players"

-- Tick a single Mob
mobTick :: GameEngine -> IO UTCTime
mobTick ge = do
  now <- getCurrentTime
  atomicModifyIORef' (ge ^. gameState) $ \gs ->
    case PQ.minView (gs ^. updateQueue) of
      Nothing -> (gs, 1 `addUTCTime` now)

gameLoop :: GameEngine -> IO ()
gameLoop ge = do
  forever $ do
    n <- mobTick ge
    undefined