{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Rogue.Bucket
  ( BucketName
  , Bucket
  , Buckets
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
     , _contense :: Int64
     }
  deriving (Read,Show,Eq)

class HasBuckets t where
  buckets :: Simple Lens t Buckets

bucket :: HasBuckets s => BucketName -> Lens' s Bucket
bucket n = buckets.at n.non (Bucket 0 0)

instance JS.ToJSON Bucket where
  toJSON (Bucket cap con) =
    JS.object ["capacity" .= cap, "contense" .= con]

instance JS.FromJSON Bucket where
  parseJSON (JS.Object v) =
    Bucket
    <$> v .: "capacity"
    <*> v .: "contense"
  parseJSON _ = mzero
