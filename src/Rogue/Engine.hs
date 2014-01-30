{-# LANGUAGE TemplateHaskell #-}
module Rogue.Engine
  ( GameEngine
  , HasGameEngine(gameEngine)
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
import Control.Monad.Identity

import Rogue.Act
import Rogue.Time
import Rogue.Mob
import Rogue.Mob.Id
import Rogue.Mob.Player

data GameState = GameState
  { _mobs        :: Table Mob
  , _updateQueue :: MinPQueue UTCTime MobId
  , _randSrc     :: PureMT
  }

makeClassy ''GameState

data GameEngine = GameEngine
  { _gameRef :: IORef GameState
  }

makeClassy ''GameEngine

startGame :: IO GameEngine
startGame = do
  mt <- newPureMT
  ge <- GameEngine <$> newIORef (GameState Table.empty PQ.empty mt)
  forkIO $ gameLoop ge
  return ge

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  -- Queue the player for insertion into the game after the next tick.
  -- Don't return until they are in the game?
  -- We don't want connections hanging though.
  error "We don't want no sticking players"

delayTillTick :: GameState -> MobId -> NominalDiffTime
delayTillTick _ _ = 1

-- Tick a single Mob
mobTick :: GameEngine -> IO UTCTime
mobTick ge = do
  now <- getCurrentTime
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
        let ugs = runIdentity $ execAct ?? gs $ do
              updateQueue .= PQ.insert (delayTillTick gs mid `addUTCTime` now) mid qr
        in (ugs, maybe (1 `addUTCTime` now) (^. _1) $ PQ.getMin (gs ^. updateQueue))

gameLoop :: GameEngine -> IO ()
gameLoop ge = forever $ do
  t <- mobTick ge
  delayTill t
