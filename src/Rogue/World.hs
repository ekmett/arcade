{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rogue.World
  (
    X, Y
  , Point
  , Grid

  , Stair(..)
  , up
  , down

  , Tile(..)
  , Level(..)
  , levelId
  , stairs

  , World(..)
  , worldId
  , levels
  , portals
  ) where

import           Rogue.ASCII

import           Control.Lens   hiding (Level, levels)
import           Data.Array     (Array, listArray)
import           Data.Default
import           Data.Function  (on)
import           Data.Text      (Text)
import           Data.Text.Lens
import           Data.UUID      (UUID)
import qualified Data.UUID      as UUID

type X = Int
type Y = Int
type Point = (X,Y)
type Grid a = Array Point a

type WorldId = UUID
type LevelId = UUID

data Stair a = Stair
  { _up   :: a
  , _down :: a
  } deriving (Show)

makeLenses ''Stair

data Tile = Floor | Wall
  deriving (Show, Eq, Ord)

instance ASCII Tile where
  ascii Floor = ' '
  ascii Wall  = '#'

data Level = Level
  { _levelId   :: {-# UNPACK #-} !LevelId
  , _levelName :: !Text
  , _stairs    :: [Stair Level]
  , _tiles     :: Grid Tile
  }

makeClassy ''Level

instance Eq Level where
  (==) = (==) `on` _levelId

instance Show Level where
  show l = "<Level " ++ show (l^.levelId) ++ " " ++ l^.levelName.unpacked ++ ">"

instance Default Level where
  def = Level UUID.nil "A Dungeon" [] (listArray ((0,0), (79,23)) (repeat Floor))

data World = World
  { _worldId :: {-# UNPACK #-} !WorldId
  , _portals :: [Stair World]
  , _levels  :: [Level]
  }

makeClassy ''World

instance Eq World where
  (==) = (==) `on` _worldId

instance Show World where
  show w = "<World " ++ show (w^.worldId) ++ ">"

instance Default World where
  def = World UUID.nil [] [def]
