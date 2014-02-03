module Main
  ( main
  ) where

import Options.Applicative
import Rogue.Monitor
import Rogue.Server
import Rogue.Options

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOptions)
     $ fullDesc
    <> progDesc "roguekcd"
    <> header "A game of hats and stacks and style"

  withMonitor opts $ serverMain opts
