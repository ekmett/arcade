{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Rogue.Bucket
  ( Bucket(..)
  , HasBucket(..)
  , update
  , leak
  , context
  ) where

import Control.Lens
import Data.Default
import Data.Int
import Data.Typeable
import Rogue.Expr
import Rogue.Stat

data Bucket = Bucket
  { _capacity, _delta :: Expr
  , _current          :: Int64
  } deriving (Read,Show,Typeable)

instance Default Bucket where
  def = Bucket 0 0 0

makeClassy ''Bucket

update :: Env -> Bucket -> Bucket
update e (Bucket l d n) = Bucket l d $ max 0 $ min (n + eval e d) (eval e l)

leak :: Lens' t Bucket -> Lens' t Bucket -> Expr -> t -> t
leak hither yon rate t =
  t & hither.delta -~ rate
    & yon.delta +~ rate

-- mutually recursive definitions
context :: (HasStats t b, HasBucket b, Default b) => t -> Env
context t = e where
  e = Env (\s -> t^.stat s.current)
          (\s -> eval e $ t^.stat s.capacity)
