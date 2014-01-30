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
import Rogue.Client.Options
import Rogue.Curses
import Rogue.Monitor
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper as Helper

main :: IO ()
main = do
  options <- execParser $ info parseClientOptions $
    fullDesc
    <> progDesc "rogue.client"
    <> header "A game client"

  withMonitor options $ \ _mon -> do
   withCurses $ \ ui -> do
    _ <- WS.runClient (options^.clientHost) (options^.clientPort) "/" $ \conn -> do
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
