{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Rogue.Bucket
  ( Bucket(..)
  , HasBucket(..)
  , update
  , leak
  ) where

import Control.Lens
import Data.Int
import Data.Typeable
import Rogue.Stat
import Rogue.Expr

data Bucket = Bucket
  { _limit, _delta :: Expr
  , _current       :: Int64
  } deriving (Read,Show,Typeable)

makeClassy ''Bucket

update :: (Stat -> Int64) -> (Stat -> Int64) -> Bucket -> Bucket
update cur cap (Bucket l d n) =
  Bucket l d $ max 0 $ min (n + eval cur cap d) (eval cur cap l)

leak :: Lens' t Bucket -> Lens' t Bucket -> Expr -> t -> t
leak hither yon rate t =
  t & hither.delta -~ rate
    & yon.delta +~ rate
