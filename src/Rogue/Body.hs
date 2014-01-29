{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
module Rogue.Body
  ( Slot
  , Body(..)
  , HasBody(..)
  )
  where

import Control.Lens
import Data.Aeson
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Rogue.ASCII
import Rogue.Hat
import Rogue.Location
import Rogue.Stat

type Slot = Text

data Body = Body
  { _bodyLocation  :: !Location
  , _inventory     :: Map Slot Hat
  , _bodyStats     :: Stats
  , _bodyASCII     :: !Char
  } deriving (Read,Show,Generic)

makeClassy ''Body

instance ASCII Body where
  ascii = _bodyASCII

instance Default Body where
  def = Body def def def '?'
      & stat MaxHealth .~ 10
      & stat Health    .~ 10

instance HasStats Body where
  stats = bodyStats

instance HasLocation Body where
  location = bodyLocation

instance FromJSON Body
instance ToJSON Body
