{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Client.Options
  ( ClientOptions(..)
  , HasClientOptions(..)
  , parseClientOptions
  , clientUri
  ) where

import Control.Lens hiding (Setting)
import Data.Data
import Options.Applicative
import Rogue.Monitor

data ClientOptions = ClientOptions
  { _clientMonitorOptions :: MonitorOptions
  , _clientHost           :: String
  , _clientPort           :: Int
  , _clientTimeout        :: Int
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

makeClassy ''ClientOptions

clientUri :: HasClientOptions t => t -> String
clientUri t = "http://" ++ t^.clientHost ++ ":" ++ show (t^.clientPort) ++ "/"

parseClientOptions :: Parser ClientOptions
parseClientOptions = ClientOptions
  <$> parseMonitorOptions
  <*> strOption (long "host" <> short 'h' <> help "hostname for the server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "port" <> short 'p' <> help "port for the server" <> metavar "PORT" <> value 8080)
  <*> option (long "timeout" <> short 't' <> help "server timeout" <> metavar "SECONDS" <> value 30)

instance HasMonitorOptions ClientOptions where
  monitorOptions = clientMonitorOptions
