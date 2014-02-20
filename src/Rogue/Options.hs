{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Options
  ( Options(..)
  , HasOptions(..)
  , parseOptions
  , serverUri
  ) where

import Control.Lens hiding (Setting)
import Data.Data
import Options.Applicative
import Rogue.Monitor

data Options = Options
  { _serverMonitorOptions :: MonitorOptions
  , _serverHost           :: String
  , _serverPort           :: Int
  , _serverOpen           :: Bool
  , _serverFile           :: Bool
  , _serverTimeout        :: Int
  , _serverStaticPath     :: Maybe String
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''Options

serverUri :: HasOptions t => t -> String
serverUri t
  | t^.serverFile = "static/index.html" -- TODO: install in data-dir and use installed path
  | otherwise = "http://" ++ t^.serverHost ++ ":" ++ show (t^.serverPort) ++ "/"

-- | Parse EKG configuration
parseOptions :: Parser Options
parseOptions = Options
  <$> parseMonitorOptions
  <*> strOption (long "host" <> short 'h' <> help "hostname for the server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "port" <> short 'p' <> help "port for the server" <> metavar "PORT" <> value 8080)
  <*> switch (long "open" <> short 'o' <> help "open on launch")
  <*> switch (long "file" <> short 'f' <> help "load static content via file:// on -o")
  <*> option (long "timeout" <> short 't' <> help "server timeout" <> metavar "SECONDS" <> value 30)
  <*> optional (strOption (long "static" <> short 's' <> metavar "DIR" <> help "serve content dynamically from static directory instead of from the embedded copy"))

instance HasMonitorOptions Options where
  monitorOptions = serverMonitorOptions
