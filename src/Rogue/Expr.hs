module Rogue.Expr
  ( Expr(..)
  , eval
  ) where

import Data.Int
import Rogue.Stat

data Expr
  = Sqrt Expr
  | Fixed Int64
  | Min   Expr Expr
  | Max   Expr Expr
  | Plus  Expr Expr
  | Times Expr Expr
  | Capacity Stat
  | Current  Stat
  | Negate Expr
  | Abs Expr
  | Signum Expr
  deriving (Show, Read)

instance Eq Expr where
  (==) = error "Expr.=="

instance Ord Expr where
  compare = error "Expr.compare"
  min = Min
  max = Max

instance Num Expr where
  (+) = Plus
  (*) = Times
  negate = Negate
  abs = Abs
  signum = Signum
  fromInteger = Fixed . fromInteger

eval :: (Stat -> Int64) -> (Stat -> Int64) -> Expr -> Int64
eval current capacity = go where
  go (Sqrt x)     = round $ sqrt (fromIntegral (go x) :: Double)
  go (Fixed n)    = n
  go (Negate x)   = negate $ go x
  go (Min x y)    = go x `min` go y
  go (Max x y)    = go x `max` go y
  go (Plus x y)   = go x + go y
  go (Times x y)  = go x * go y
  go (Abs x)      = abs $ go x
  go (Signum x)   = signum $ go x
  go (Capacity x) = capacity x
  go (Current x)  = current x
