module Rogue.Time
  ( delayTime
  , delayTill
  ) where

import Data.Time
import Control.Concurrent.Thread.Delay

delayTime :: NominalDiffTime -> IO ()
delayTime d = delay (ceiling $ d*(10^(6::Int)))

delayTill :: UTCTime -> IO ()
delayTill w = do
  n <- getCurrentTime
  delayTime (w `diffUTCTime` n)
