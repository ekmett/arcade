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
import Data.FileEmbed
import Data.Text
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
import System.Process

main :: IO ()
main = do
  _ <- system "/usr/bin/open http://localhost:8080/index.html"
  Warp.runSettings Warp.defaultSettings
    { Warp.settingsPort = 8080
    , Warp.settingsIntercept = WaiWS.intercept app
    } $ staticApp $ embeddedSettings $(embedDir "static")

app :: ServerApp
app pending = do
  conn <- WS.acceptRequest pending
  WS.sendTextData conn ("alert('test');" :: Text)
  msg <- WS.receiveData conn
  print (msg :: Text)
  finally ?? disconnect $ return ()
 where disconnect = return ()
