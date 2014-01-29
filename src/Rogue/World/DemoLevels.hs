{-# LANGUAGE OverloadedStrings #-}
module Rogue.World.DemoLevels
  ( testLevel1
  , module Rogue.World
  ) where

import           Rogue.World

import qualified Data.Array   as A
import           Data.Default (def)
import qualified Data.Map     as Map
import           Data.UUID    (nil)

readTile :: Char -> Tile
readTile ' ' = Floor
readTile '<' = Floor
readTile '>' = Floor
readTile _   = Wall

buildLevel :: [String] -> Level
buildLevel chars = Level nil "A test letel" stairs' tiles'
  where
    chars'  = A.listArray ((0,0), (xBound, yBound)) (concat chars)
    tiles' = fmap readTile chars'
    xBound = length (head chars) - 1
    yBound = length chars - 1
    stairs' = (Map.fromList . foldr addStair [] . A.assocs) chars'
      where
        addStair (ix, '<') = ((ix, Up def) :)
        addStair (ix, '>') = ((ix, Down def) :)
        addStair _ = id

testLevel1 :: Level
testLevel1 = buildLevel [ "######"
                        , "#<   #"
                        , "#    #"
                        , "#   >#"
                        , "######"
                        ]
