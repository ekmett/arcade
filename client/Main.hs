{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import qualified Data.Text.IO as TIO
import Control.Lens
import Control.Monad
import Control.Concurrent
import Data.Monoid
import Data.Text
import Data.Text.Strict.Lens
import Network.WebSockets as WS
import Options.Applicative
import Rogue.Curses
import Rogue.Monitor
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper as Helper

main :: IO ()
main = do
  options <- execParser $ info parseMonitorOptions $
    fullDesc
    <> progDesc "rogue.console"
    <> header "A game console"

  withMonitor options $ \_mon -> do
   withCurses $ \ ui -> do
    _ <- WS.runClient "127.0.0.1" 8080 "/" $ \conn -> do
      msgs <- newChan
      (_h,w) <- scrSize
      _scrollback <- newWin 5 (w-2) 1 1
      void . forkIO . forever $ do
        k <- readChan $ ui^.cursesKeys
        WS.sendTextData conn $ Helper.displayKey k ^.packed
      forever $ do
        f <- WS.receiveData conn
        writeChan msgs (f :: Text)

    return ()
