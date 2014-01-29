{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import Control.Monad
import Control.Concurrent
import Data.Monoid
import Options.Applicative
import Network.WebSockets as WS

import Rogue.Monitor

main :: IO ()
main = do
  options <- execParser $ info parseMonitorOptions $
    fullDesc
    <> progDesc "rogue.console"
    <> header "A game console"

  withMonitor options $ \_mon -> do
    WS.runClient "127.0.0.1" 8080 "/" $ \conn -> do
      void . forkIO . forever $ do
        putStrLn "Press enter to continue."
        l <- TIO.getLine
        WS.sendTextData conn l
      void . forever $ do
        f <- WS.receiveData conn
        TIO.putStrLn f
        TIO.putStrLn ""
    return ()
