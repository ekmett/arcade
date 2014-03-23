{-# LANGUAGE DeriveDataTypeable #-}
module Arcade.Engine
  ( Engine(..)
  , startEngine
  ) where

import Control.Concurrent
import Data.Typeable
import Arcade.Connection

data Engine = Engine
  { engineThreadId :: ThreadId
  , engineRequest :: Request -> IO ()
  } deriving Typeable

instance AcceptsRequests Engine where
  request = engineRequest

game :: Game ()
game = do
  delayTime 1000
  --  first things first. always address requests
  {-
  requestChan <- ask envRequestChan
  untilM (isEmptyChan requestChan) $ do
    req <- readChan requests
    handleRequest req
  -}
  -- read from channels, skipping to last verson of ephemera
  -- track exponential moving average of the number of messages and throttle a client?
  -- or dilate system time
  -- run a time tick
  -- notify everyone of results
  game

startEngine :: IO Engine
startEngine = do
  requestChan <- newChan
  tid <- forkIO $ runGame game requestChan newWorld
  return $ Engine tid (sendChan requestChan)
