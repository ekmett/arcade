{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Rogue.Client.Model
  ( Model(..)
  , HasModel(..)
  ) where

import Control.Lens
import Data.Text
import Data.Typeable
import Data.Sequence
import Rogue.Client.Options
import Rogue.Location
import Rogue.Monitor

-- | What the client knows about the world

data Model = Model
  { _chatLog        :: Seq Text
  , _commandHistory :: Seq Text
  , _modelLocation  :: Location
  , _modelMonitor   :: Monitor
  , _modelOptions   :: ClientOptions
  } deriving Typeable

makeClassy ''Model

instance HasLocation Model where
  location = modelLocation

instance HasClientOptions Model where
  clientOptions = modelOptions

instance HasMonitor Model where
  monitor = modelMonitor

instance HasMonitorOptions Model where
  monitorOptions = monitor.monitorOptions
