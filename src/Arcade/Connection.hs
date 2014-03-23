{-# LANGUAGE DeriveDataTypeable #-}
module Arcade.Connection
  ( Connection(..)
  , socketConnection
  ) where

import Control.Monad (unless)
import Data.ByteString as B
import Data.Typeable
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketBS

data Connection = Connection
  { recv         :: IO ByteString
  , send         :: ByteString -> IO ()
  , close        :: IO ()
  } deriving Typeable

_MTU :: Int
_MTU = 4096

socketConnection :: Socket.Socket -> Connection
socketConnection s = Connection
  { recv = SocketBS.recv s _MTU
  , send = \msg -> do
      len <- SocketBS.send s msg
      unless (B.length msg == len) $ fail "socketConnection: partial send"
  , close = Socket.close s
  }
