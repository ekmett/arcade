{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Rogue.Affect where

import Rogue.Modifier

data Affect =
    TempAffect { _remaining :: Int -- How many ticks it will last
               , _affect :: Affect
               }
    | AffectModifier { _target   :: (Stat, Bool) -- The stat and if it changes capacity or delta
                     , _modifier :: Modifier }
    