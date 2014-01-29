{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Exception (finally)
import Control.Concurrent
import Control.Lens
import Data.Default
import Data.FileEmbed
import Data.Monoid
import Data.Text as T
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
import System.Process
import Options.Applicative
import Rogue.Mob
import Rogue.Monitor
import Rogue.Server.Options

main :: IO ()
main = do
  options <- execParser $ info parseServerOptions $
    fullDesc
    <> progDesc "rogue.server"
    <> header "A game server"

  withMonitor options $ \mon -> do
    let uri = serverUri options
    putStrLn $ "Serving " ++ uri
    when (options^.serverOpen) $ do
      _ <- system $ "/usr/bin/open " ++ uri
      return ()
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = options^.serverPort
      , Warp.settingsTimeout = options^.serverTimeout
      , Warp.settingsIntercept = WaiWS.intercept (app mon)
      } $ staticApp $ embeddedSettings $(embedDir "static")

app :: Monitor -> ServerApp
app _mon pending = do
  let p = def :: Mob ()
  conn <- WS.acceptRequest pending
  void . forkIO . forever $ do
    msg <- WS.receiveData conn
    print (msg :: Text)
  void . forever $ do
    WS.sendTextData conn $ T.concat ["alert('", T.replace "'" "\\'" . T.pack $ show p, "');"]
    threadDelay $ 10^(6::Int)
  finally ?? disconnect $ return ()
 where disconnect = return ()
