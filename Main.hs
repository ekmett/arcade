module Main
  ( main
  ) where

import Options.Applicative
import Arcade.Monitor
import Arcade.Server
import Arcade.Options

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOptions)
     $ fullDesc
    <> progDesc "arcade"
    <> header "A physics playground"

  withMonitor opts $ serverMain opts
