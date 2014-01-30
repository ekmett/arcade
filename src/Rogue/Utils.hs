module Rogue.Utils (
    isqrt
  , concatMapM
  ) where

import Data.Int
import Control.Monad


-- https://ghc.haskell.org/trac/ghc/ticket/2042
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- From wiki (http://www.haskell.org/haskellwiki/Generic_number_type)
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

isqrt :: Int64 -> Int64
isqrt 0 = 0
isqrt 1 = 1
isqrt n =
  let twopows = iterate (^!2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (isqrt (div n lowerN) * lowerRoot)
      isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters
