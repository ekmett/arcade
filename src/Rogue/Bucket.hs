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
import Rogue.Expr

-- Has some crazy behavior, see IRC 2014-01-28 19:00 EST5EDT

data Bucket = Bucket
  { _limit, _delta :: Expr
  , _current       :: Int64
  } deriving (Read,Show,Typeable)

makeClassy ''Bucket

update :: Env -> Bucket -> Bucket
update e (Bucket l d n) = Bucket l d $ max 0 $ min (n + eval e d) (eval e l)

leak :: Lens' t Bucket -> Lens' t Bucket -> Expr -> t -> t
leak hither yon rate t =
  t & hither.delta -~ rate
    & yon.delta +~ rate
