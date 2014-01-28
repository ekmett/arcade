module Rogue.Buckets (
  ) where

bFunc :: BucketFunction -> Int64 -> Int64
bFunc BFSqrt i = squareRoot i
bFunc BFAll  i = i
bFunc (BFLimit l) i = if l > i then i else l 

bCalc :: Level -> Mob -> BucketSource -> BucketFunction -> Int64
bCalc _ m (OtherBucket bn) bf = bFunc bf (???)
bCalc _ m (Stat sn) bf = bFunc bf (???)
bCalc _ _ (BSFixed i) bf = bFunc bf i

-- I'm lazy and the normal case is a single step, so we'll just make multiple steps a fold.
-- Then we can just add functions without worrying about discrete integration.
updateBuckets :: Level -> Steps -> Mob -> Mob
updateBuckets w s m =
    m & buckets .~ execState (m ^. buckets) $ do
      let bo = filter (Map.member ?? (m ^. buckets)) m ^. bucketOrder 
      let be = filter (Set.member ?? (Set.fromList bo)) . Map.keys $ m ^. buckets
      let bs = bo ++ be
      replicateM s $ do 
        forM_ bs $ \bn -> do
          
  where
    up m _ = fmap updateBucket (m ^. buckets)
    updateBucket b = b & current +~ (bucketStep (b ^. bucketSource))
    bucketStep (
      
-- From wiki (http://www.haskell.org/haskellwiki/Generic_number_type)
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Int64 -> Int64
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
  let twopows = iterate (^!2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
      isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters
