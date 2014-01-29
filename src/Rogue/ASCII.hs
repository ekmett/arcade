module Rogue.ASCII
  ( ASCII(..)
  ) where

class ASCII t where
  ascii :: t -> Char

instance ASCII Char where
  ascii = id
