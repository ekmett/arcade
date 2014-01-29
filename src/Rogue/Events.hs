module Rogue.Events where

import Data.Text (Text)

import Rogue.Location

data Occurence =
    Noise { _where :: Location, _what :: Text }
  | 