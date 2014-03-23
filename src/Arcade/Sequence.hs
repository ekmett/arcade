module Arcade.Sequence
  ( Sequence(..)
  ) where

import Data.Word

-- wrapping 16 bit sequence numbers
newtype Sequence = Sequence Word16

instance Eq Sequence where
  Sequence a == Sequence b = a == b

instance Ord Sequence where
  compare (Sequence s1) (Sequence s2) = case compare s1 s2 of
    LT | s2 - s1 <= 32768 -> GT
    GT | s1 - s2 <= 32768 -> LT
    x -> x
