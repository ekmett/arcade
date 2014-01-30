{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Options.Applicative
import Rogue.Client
import Rogue.Client.Options
import Rogue.Monitor
import Rogue.Server
import Rogue.Server.Options

data Command
  = Server ServerOptions
  | Client ClientOptions

instance HasMonitorOptions Command where
  monitorOptions f (Server s) = Server <$> monitorOptions f s
  monitorOptions f (Client c) = Client <$> monitorOptions f c

parseOptions :: Parser Command
parseOptions = subparser
  ( command "server"  (info (Server <$> parseServerOptions) $ progDesc "start a game server")
 <> command "client"  (info (Client <$> parseClientOptions) $ progDesc "start a game client")
  )

main :: IO ()
main = do
  options <- execParser $ info (helper <*> parseOptions) $
    fullDesc
    <> progDesc "roguish"
    <> header "A game of hats and stacks and style"

  withMonitor options $ case options of
    Client c -> clientMain c
    Server s -> serverMain s
