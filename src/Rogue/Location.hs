{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Location
  ( Floor
  , Location(..)
  , HasLocation(..)
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Hashable
import Data.String
import Data.Text
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics

newtype Floor = Floor Text deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance FromJSON Floor
instance ToJSON Floor
instance Hashable Floor

instance IsString Floor where
  fromString = Floor . fromString

instance Default Floor where
  def = "Limbo"

data Location = Location { _floor :: !Floor, __x, __y :: {-# UNPACK #-} !Int }
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

instance Default Location where
  def = Location "Limbo" 0 0

makeClassy ''Location

instance FromJSON Location where
  parseJSON = withArray "location" $ \v -> case toList v of
    f:x:y:_ -> Location <$> parseJSON f <*> parseJSON x <*> parseJSON y
    _       -> mzero

instance ToJSON Location where
  toJSON (Location f x y) = Array $ fromList $ [toJSON f,toJSON x,toJSON y]

instance Hashable Location
