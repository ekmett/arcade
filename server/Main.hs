{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception (finally)
import Control.Lens
import Data.Default
import Data.FileEmbed
import Data.Monoid
import Data.Text as T
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Data.Aeson as JS
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import Rogue.Mob
import Rogue.Monitor

main :: IO ()
main = do
  options <- execParser $ info parseMonitorOptions $
    fullDesc
    <> progDesc "rogue.server"
    <> header "A game server"

  withMonitor options $ \mon -> do
    putStrLn "Serving http://localhost:8080/index.html"
    Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 8080
      , Warp.settingsIntercept = WaiWS.intercept (app mon)
      } $ staticApp $ embeddedSettings $(embedDir "static")

app :: Monitor -> ServerApp
app _mon pending = do
  let p = def :: Mob ()
  conn <- WS.acceptRequest pending
  WS.sendTextData conn $ T.concat ["alert('", T.replace "'" "\\'" . T.pack $ show p, "');"]
  msg <- WS.receiveData conn
  print (msg :: Text)
  finally ?? disconnect $ return ()
 where disconnect = return ()
