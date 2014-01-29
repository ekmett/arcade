{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
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
import GHC.Generics
import Data.Aeson

import Rogue.Expr
import Rogue.Stat

-- Has some crazy behavior, see IRC 2014-01-28 19:00 EST5EDT

data Bucket = Bucket
  { _capacity, _delta :: Expr
  , _current          :: Int64
  } deriving (Read,Show,Typeable,Generic)

instance FromJSON Bucket
instance ToJSON Bucket

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
context :: (HasStats t Bucket) => t -> Env
context t = e where
  e = Env (\s -> t^.stat s.current)
          (\s -> eval e $ t^.stat s.capacity)
