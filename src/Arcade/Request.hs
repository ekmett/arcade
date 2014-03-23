{-# LANGUAGE DeriveDataTypeable #-}
module Arcade.Request
  ( Request(..)
  , AcceptsRequests(..)
  , accept
  , shutdown
  ) where

import Data.Typeable
import Arcade.Connection

data Request
  = Shutdown
  | Accept Connection
  deriving Typeable

class AcceptsRequests t where
  request  :: t -> Request -> IO ()

accept :: AcceptsRequests t => t -> Connection -> IO ()
accept t con = request t $ Accept con

shutdown :: AcceptsRequests t => t -> IO ()
shutdown t = request t Shutdown
