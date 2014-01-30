{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Server.Options
  ( ServerOptions(..)
  , HasServerOptions(..)
  , parseServerOptions
  , serverUri
  ) where

import Control.Lens hiding (Setting)
import Data.Data
import Options.Applicative
import Rogue.Monitor

data ServerOptions = ServerOptions
  { _serverMonitorOptions :: MonitorOptions
  , _serverHost           :: String
  , _serverPort           :: Int
  , _serverOpen           :: Bool
  , _serverTimeout        :: Int
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''ServerOptions

serverUri :: HasServerOptions t => t -> String
serverUri t = "http://" ++ t^.serverHost ++ ":" ++ show (t^.serverPort) ++ "/"

-- | Parse EKG configuration
parseServerOptions :: Parser ServerOptions
parseServerOptions = ServerOptions
  <$> parseMonitorOptions
  <*> strOption (long "host" <> short 'h' <> help "hostname for the server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "port" <> short 'p' <> help "port for the server" <> metavar "PORT" <> value 8080)
  <*> switch (long "open" <> short 'o' <> help "open on launch")
  <*> option (long "timeout" <> short 't' <> help "server timeout" <> metavar "SECONDS" <> value 30)

instance HasMonitorOptions ServerOptions where
  monitorOptions = serverMonitorOptions
