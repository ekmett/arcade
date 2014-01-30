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
  , setBucketMax
  , limitBucket, limitBuckets
  , fillBucket, fillBuckets
  , pour
  , withBuckets
  , addBucket
  ) where

import Control.Lens hiding ((.=))
import Control.Applicative
import Control.Monad
import Data.Map
import Data.Text (Text)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as JS

type BucketName = Text

type Buckets = Map BucketName Bucket

data Bucket =
    Bucket {
       _capacity :: Double
     , _current :: Double
     }
  deriving (Eq,Ord,Read,Show)

makeLenses ''Bucket

class HasBuckets t where
  buckets :: Simple Lens t Buckets

bucket :: HasBuckets s => BucketName -> Lens' s Bucket
bucket n = buckets.at n.non (Bucket 0 0)

-- Sets the max of a bucket, if it doesn't exist create it empty.
setBucketMax :: (Real r, HasBuckets s) => BucketName -> r -> s -> s
setBucketMax b m bs =
  bs & bucket b .~ ((bs ^. bucket b) & capacity .~ (realToFrac m))

limitBucket :: Bucket -> Bucket
limitBucket b = b & current .~ min (b ^. capacity) (b ^. current)

limitBuckets :: HasBuckets b => b -> b
limitBuckets b = b & buckets %~ (fmap limitBucket)

fillBucket :: Bucket -> Bucket
fillBucket b = b & current .~ (b ^. capacity)

fillBuckets :: HasBuckets b => b -> b
fillBuckets b = b & buckets %~ (fmap fillBucket)

addBucket :: Real r => Lens' s Bucket -> r -> s -> s
addBucket l a s = s & l.current +~ (min (remaningSpace l s) (realToFrac a))

remaningSpace :: Lens' s Bucket -> s -> Double
remaningSpace l s = (s ^. l.capacity) - (s ^. l.current)

-- Transfers up to 'a' from b1 to b2,  limited by the amount in b1 and how much b2 can fit.
pour :: Real r => Lens' s Bucket -> Lens' s Bucket -> r -> s -> s
pour b1 b2 a s =
    (s & b1 . current -~ t) & b2.current +~ t
  where
    t = minimum [s ^. b1.current, realToFrac a, remaningSpace b2 s]

-- Sizes the buckets, updates them, and always limits them
withBuckets :: HasBuckets b => (b -> b) -> (b -> b) -> b -> b 
withBuckets sz up hbs =
  limitBuckets $ up $ sz hbs

instance JS.ToJSON Bucket where
  toJSON (Bucket cap cur) =
    JS.object ["capacity" .= cap, "current" .= cur]

instance JS.FromJSON Bucket where
  parseJSON (JS.Object v) =
    Bucket
    <$> v .: "capacity"
    <*> v .: "current"
  parseJSON _ = mzero
