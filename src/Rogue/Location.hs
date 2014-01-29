{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Location
  ( Floor
  , Location(..)
  ) where

import Control.Lens
import Data.Hashable
import Data.Text
import Data.Typeable
import GHC.Generics

type Floor = Text

data Location = Location { _floor :: !Floor, __x, __y :: {-# UNPACK #-} !Int }
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

makeClassy ''Location

instance Hashable Location
