module Rogue.Time
  ( delayTime
  , delayTill
  , frameDuration
  ) where

import Data.Time
import Control.Concurrent.Thread.Delay

delayTime :: NominalDiffTime -> IO ()
delayTime d = delay $ ceiling $ d*1e6

-- | duration of a frame at a given fps rate
frameDuration :: Integer -> DiffTime
frameDuration fps = picosecondsToDiffTime $ fromInteger (div (10^(12::Int)) fps)

delayTill :: UTCTime -> IO ()
delayTill w = do
  n <- getCurrentTime
  delayTime $ w `diffUTCTime` n
