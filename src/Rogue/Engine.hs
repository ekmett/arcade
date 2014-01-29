module Rogue.Engine (
    GameEngine
  , startGame
  ) where

data GameEngine =
  Engine {
      _gameState :: IORef GameState
    , _
    }

startGame :: IO GameEngine
startGame = do
  ???

joinPlayer :: Player -> GameEngine -> IO ()
joinPlayer p ge = do
  -- Queue the player for insertion into the game after the next tick.
  -- Don't return untill they're in the game?
  -- We don't want connections hanging though.

gameLoop :: GameEngine -> IO ()
gameLoop ge = do
  