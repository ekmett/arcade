module Main where

import Data.Monoid
import Options.Applicative
import Rogue.Monitor

main :: IO ()
main = do
  options <- execParser $ info parseMonitorOptions $
    fullDesc
    <> progDesc "rogue.console"
    <> header "A game console"

  withMonitor options $ \_mon -> do
    putStrLn "Press enter to continue."
    _ <- getLine
    return ()
