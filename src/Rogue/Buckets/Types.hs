module Rogue.Buckets.Types (
  ) where

data BucketFunction =
    BFSqrt
  | BFAll
  | BFLimit Int64

data BucketSource =
    OtherBucket BucketName
  | Stat StatName
  | BSFixed Int64

data BucketUpdate = 
    -- Moves some of the contents of the specified bucket into this bucket (based on BucketFunction).
    Transfer BucketName BucketFunction
    -- Provides some into the bucket based on the value of the source. Sources are read only.
  | Provide BucketSource BucketFunction

data Bucket = 
  Bucket {
      -- Word64 because I trust the community to find loopholes.
      _current :: Int64
      -- How we refill this bucket
    , _bucketSource :: BucketUpdate
    , _bucketLimit :: (BucketSource, BucketFunction)
    }
