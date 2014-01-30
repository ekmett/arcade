{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Curses
  ( Curses(..)
  , HasCurses(..)
  , withCurses
  ) where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad (forever)
import Control.Monad.Catch as Catch
import Control.Monad.Trans (MonadIO(..))
import Data.Typeable
import UI.HSCurses.Curses as Curses
import UI.HSCurses.CursesHelper as Helper

data Curses = Curses
  { _cursesKeys :: Chan Key
  }

makeClassy ''Curses

data ShutdownCurses = ShutdownCurses deriving (Show,Typeable)
instance Exception ShutdownCurses

withCurses :: (MonadCatch m, MonadIO m) => (Curses -> m a) -> m a
withCurses k = do
  ch <- liftIO $ do
    Helper.start
    newChan
  tid <- liftIO $ forkIO $ forever $ do
    a <- getCh
    writeChan ch a
  Catch.finally (k $ Curses ch) $ liftIO $ do
    throwTo tid ShutdownCurses
    Helper.end
