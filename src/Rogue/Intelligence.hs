module Rogue.Intelligence (
    Command(..)
  , Intelligence
  ) where

data Command =
    NOP
  | Meditate
  | LoseConcentration

type Intelligence = World -> Mob -> IO Command
