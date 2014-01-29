{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Monitor
  ( MonitorOptions(..)
  , HasMonitorOptions(..)
  , parseMonitorOptions
  , withMonitor
  -- * The Monitor
  , Monitor(..)
  , HasMonitor(..)
  -- * Gauges
  , Gauge(..)
  , gauge
  -- * Counters
  , Counter(..)
  , counter
  -- * Labels
  , Label(..)
  , label
  -- * Modifiers
  , Setting(..)
  , Incremental(..)
  , dec
  , sub
  -- * Compatibilty with EKG
  , withServer
  ) where

import Control.Exception
import Control.Lens hiding (Setting)
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Data.ByteString.Lens
import Data.Data
import Data.Text
import Options.Applicative
import System.Process
import System.Remote.Monitoring
import qualified System.Remote.Gauge as G
import qualified System.Remote.Counter as C
import qualified System.Remote.Label as L

-- | Enable/disable EKG
data MonitorOptions = MonitorOptions
  { _monitorHost    :: String
  , _monitorPort    :: Int
  , _monitorEnabled :: Bool
  } deriving (Eq,Ord,Show,Read,Data,Typeable)

-- | Parse EKG configuration
parseMonitorOptions :: Parser MonitorOptions
parseMonitorOptions = MonitorOptions
  <$> strOption (long "ekg-host" <> short 'h' <> help "host for the EKG server" <> metavar "HOST" <> action "hostname" <> value "localhost")
  <*> option (long "ekg-port" <> short 'p' <> help "port for the EKG server" <> metavar "PORT" <> value 5616)
  <*> (not <$> switch (long "no-ekg" <> help "do not start the EKG server" <> value False))

makeClassy ''MonitorOptions

data ShutdownMonitor = ShutdownMonitor deriving (Typeable, Show)

instance Exception ShutdownMonitor

data Monitor = Monitor
  { __monitorOptions :: MonitorOptions
  , _monitorServer   :: Maybe Server
  }

makeClassy ''Monitor

withServer :: HasMonitor t => t -> (Server -> IO ()) -> IO ()
withServer t k = case t^.monitorServer of
  Nothing -> return ()
  Just s  -> k s

newtype Gauge = Gauge { runGauge :: Maybe G.Gauge }
newtype Label = Label { runLabel :: Maybe L.Label }
newtype Counter = Counter { runCounter :: Maybe C.Counter }

class Setting t a | t -> a where
  assign :: MonadIO m => t -> a -> m ()        -- set
  update :: MonadIO m => t -> (a -> a) -> m () -- modify

instance Setting Label Text where
  assign (Label t) a = liftIO $ maybe (return ()) (L.set ?? a) t
  update (Label t) f = liftIO $ maybe (return ()) (L.modify f) t

instance Setting Gauge Int where
  assign (Gauge t) a = liftIO $ maybe (return ()) (G.set ?? a) t
  update (Gauge t) f = liftIO $ maybe (return ()) (G.modify f) t

dec :: (MonadIO m, Setting t a, Num a) => t -> m ()
dec t = update t (subtract 1)

sub :: (MonadIO m, Setting t a, Num a) => t -> a -> m ()
sub t n = update t (subtract n)

class Incremental t where
  inc :: MonadIO m => t -> m ()
  add :: MonadIO m => t -> Int -> m ()

instance Incremental Gauge where
  inc (Gauge t)   = liftIO $ maybe (return ()) G.inc t
  add (Gauge t) i = liftIO $ maybe (return ()) (G.add ?? i) t

instance Incremental Counter where
  inc (Counter t)   = liftIO $ maybe (return ()) C.inc t
  add (Counter t) i = liftIO $ maybe (return ()) (C.add ?? i) t

-- | create a gauge
gauge :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Gauge
gauge t = view monitorServer >>= maybe (return $ Gauge Nothing) (liftIO . fmap (Gauge . Just) . getGauge t)

-- | create a counter
counter :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Counter
counter t = view monitorServer >>= maybe (return $ Counter Nothing) (liftIO . fmap (Counter . Just) . getCounter t)

-- | create a label
label :: (MonadIO m, MonadReader t m, HasMonitor t) => Text -> m Label
label t = view monitorServer >>= maybe (return $ Label Nothing) (liftIO . fmap (Label . Just) . getLabel t)

instance HasMonitorOptions Monitor where
  monitorOptions = _monitorOptions

withMonitor :: HasMonitorOptions t => t -> (Monitor -> IO a) -> IO a
withMonitor t k
  | t^.monitorEnabled = do
    server <- forkServer (t^.monitorHost.packedChars) (t^.monitorPort)
    let uri = "http://" ++ t^.monitorHost ++ ":" ++ show (t^.monitorPort) ++ "/"
    putStrLn $ "Monitoring enabled at " ++ uri
    _ <- system $ "/usr/bin/open " ++ uri
    k (Monitor (t^.monitorOptions) $ Just server) `finally` throwTo (serverThreadId server) ShutdownMonitor
  | otherwise = k $ Monitor (t^.monitorOptions) Nothing
