{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Rogue.Server
  ( serverMain
  ) where

import Control.Monad
import Control.Exception as E
import Control.Exception.Lens
import Control.Concurrent
import Control.Lens
import Data.FileEmbed
import Data.Random
import Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Aeson as JS
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import System.Process

import Rogue.Classes
import Rogue.Description
import Rogue.Mob
import Rogue.Mob.Player
import Rogue.Monitor
import Rogue.Server.Options
import Rogue.Time

threadCountG :: Text
threadCountG = "thread count"

serverMain :: ServerOptions -> Monitor -> IO ()
serverMain options mon = do
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
app _mon pending = isThread _mon "websocket" $ do
  p <- sample (roll::RVarT IO Player) >>= return . mobify
  conn <- WS.acceptRequest pending
  void . forkR _mon "reciever" . forever $ do 
      msg <- WS.receiveData conn
      print (msg :: Text)
  void . forever $ do
    WS.sendTextData conn . TE.decodeUtf8 . BSL.toStrict . JS.encode . description $ p
    delayTime 1
  finally ?? disconnect $ return ()
 where disconnect = return ()

isThread :: Monitor -> Text -> IO a -> IO a
isThread mon nm a = do
  tG <- gauge threadCountG mon
  whereException nm . E.bracket_ (inc tG) (dec tG) $ a

forkR :: Monitor -> Text -> IO () -> IO ThreadId
forkR mon nm a = do
  tG <- gauge threadCountG mon
  forkIO . whereException nm . E.bracket_ (inc tG) (dec tG) $ a

whereException :: Text -> IO a -> IO a
whereException w = handling id $ \e -> do
  TIO.putStrLn (T.concat ["Exception in ", w, ": ", T.pack $ show e])
  E.throw e
