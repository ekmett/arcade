{-# LANGUAGE DeriveGeneric #-}
module Rogue.Damage where

-- A basic damage message type.

data PDType =
    Slash
  | Smash

data Damage =
    Physical PDType Int64
  -- Emotional, Heat, Cold, whatever
  | deriving (Read,Show,Generic)

