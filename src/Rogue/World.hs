{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rogue.World
  ( X, Y
  , Point
  , Grid
  , Stair(..)

  , Tile(..)
  , Level(..)
  , levelId
  , stairs

  , World(..)
  , worldId
  , levels
  , portals

  , showGrid
  , printGrid
  , showLevel
  , printLevel
  ) where

import           Rogue.ASCII

import           Control.Lens    hiding (Level, indices, levels)
import           Data.Array      (Array, bounds, elems, listArray)
import           Data.Array.ST   (runSTArray, thaw, writeArray)
import           Data.Default
import           Data.Function   (on)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Data.Text.Lens
import           Data.UUID       (UUID)
import qualified Data.UUID       as UUID

type X = Int
type Y = Int
type Point = (X,Y)

_x, _y :: Simple Lens Point Int
_x = _1
_y = _2

type Grid a = Array Point a
type PointMap a = Map.Map Point a

type WorldId = UUID
type LevelId = UUID

data Stair a = Up a | Down a
  deriving (Show)

makeLenses ''Stair

instance ASCII (Stair a) where
  ascii (Up _)   = '<'
  ascii (Down _) = '>'

data Tile = Floor | Wall
  deriving (Show, Eq, Ord)

instance ASCII Tile where
  ascii Floor = ' '
  ascii Wall  = '#'

data Level = Level
  { _levelId   :: {-# UNPACK #-} !LevelId
  , _levelName :: !Text
  , _stairs    :: PointMap (Stair Level)
  , _tiles     :: Grid Tile
  }

makeClassy ''Level

instance Eq Level where
  (==) = (==) `on` _levelId

instance Show Level where
  show l = "<Level " ++ show (l^.levelId) ++ " " ++ l^.levelName.unpacked ++ ">"

instance Default Level where
  def = Level UUID.nil
              "A Dungeon"
              (Map.fromList [((0,0), Up def) ,((79,23), Down def)])
              (listArray ((0,0), (79,23)) (repeat Floor))

data World = World
  { _worldId :: {-# UNPACK #-} !WorldId
  , _portals :: PointMap (Stair World)
  , _levels  :: [Level]
  }

makeClassy ''World

instance Eq World where
  (==) = (==) `on` _worldId

instance Show World where
  show w = "<World " ++ show (w^.worldId) ++ ">"

instance Default World where
  def = World UUID.nil Map.empty [def]

-- Convenience methods for displaying grids and levels

showGrid :: ASCII a => Grid a -> String
showGrid g = (unlines . chunksOf width . map ascii . elems) g
  where width = bounds g ^. _2 . _x + 1

printGrid :: ASCII a => Grid a -> IO ()
printGrid = putStrLn . showGrid

levelChars :: Level -> Grid Char
levelChars l = runSTArray $ thaw chars >>= blit stairs
  where
    chars = fmap ascii (l^.tiles)
    blit lns arr = itraverse write (l^.lns) >> return arr
      where write i = writeArray arr i . ascii

showLevel :: Level -> String
showLevel = showGrid . levelChars

printLevel :: Level -> IO ()
printLevel = putStrLn . showLevel

