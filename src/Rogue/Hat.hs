{-# LANGUAGE DeriveGeneric #-}
module Rogue.Hat
  ( Hat(..)
  ) where

import GHC.Generics
import Data.Aeson

data Hat =
    Hat
  deriving (Read,Show,Generic)

instance FromJSON Hat
instance ToJSON Hat
