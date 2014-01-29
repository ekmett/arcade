{-# LANGUAGE TemplateHaskell #-}
module Rogue.Expr
  ( Expr(..)
  , eval
  , Env(..)
  , HasEnv(..)
  ) where

import Control.Lens
import Data.Int
import Rogue.Stat

data Env = Env { _statsVals, _envCurrent, _envCapacity :: Stat -> Int64 }

makeClassy ''Env

data Expr
  = Sqrt Expr
  | Given Int64
  | Min   Expr Expr
  | Max   Expr Expr
  | Plus  Expr Expr
  | Times Expr Expr
  | Capacity Stat
  | Current  Stat
  | StatVal Stat
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
  fromInteger = Given . fromInteger

eval :: Env -> Expr -> Int64
eval (Env sv current capacity) = go where
  go (Sqrt x)     = round $ sqrt (fromIntegral (go x) :: Double)
  go (Given n)    = n
  go (Negate x)   = negate $ go x
  go (Min x y)    = go x `min` go y
  go (Max x y)    = go x `max` go y
  go (Plus x y)   = go x + go y
  go (Times x y)  = go x * go y
  go (Abs x)      = abs $ go x
  go (Signum x)   = signum $ go x
  go (Capacity x) = capacity x
  go (Current x)  = current x
  go (StatVal x)     = sv x
