{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Rogue.Bucket
  ( BucketName
  , Bucket
  , Buckets
  , capacity
  , current
  , HasBuckets(..)
  , bucket
  ) where

import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Map
import Data.Text
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as JS

type BucketName = Text

type Buckets = Map BucketName Bucket

data Bucket =
    Bucket {
       _capacity :: Int64
     , _current :: Int64
     }
  deriving (Eq,Ord,Read,Show)

makeLenses ''Bucket

class HasBuckets t where
  buckets :: Simple Lens t Buckets

bucket :: HasBuckets s => BucketName -> Lens' s Bucket
bucket n = buckets.at n.non (Bucket 0 0)

instance JS.ToJSON Bucket where
  toJSON (Bucket cap cur) =
    JS.object ["capacity" .= cap, "current" .= cur]

instance JS.FromJSON Bucket where
  parseJSON (JS.Object v) =
    Bucket
    <$> v .: "capacity"
    <*> v .: "current"
  parseJSON _ = mzero
