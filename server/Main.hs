{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Exception (finally)
import Control.Concurrent
import Control.Lens
import Data.FileEmbed
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Data.Aeson as JS
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString.Lazy as BSL
import System.Process
import Options.Applicative
import qualified Control.Exception as E
import Data.Random

import Rogue.Classes
import Rogue.Mob
import Rogue.Mob.Player
import Rogue.Monitor
import Rogue.Server.Options
import Rogue.Time

import Control.Monad.Reader

threadCountG :: Text
threadCountG = "thread count"

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
app _mon pending = isThread _mon "websocket" $ do
  p <- sample (roll::RVarT IO Player) >>= return . mobify
  conn <- WS.acceptRequest pending
  void . forkR _mon "reciever" . forever $ do 
      msg <- WS.receiveData conn
      print (msg :: Text)
  void . forever $ do
    WS.sendTextData conn . T.pack . show $ p
    delayTime 1
  finally ?? disconnect $ return ()
 where disconnect = return ()

isThread :: Monitor -> Text -> IO a -> IO a
isThread _mon nm a = do
  tG <- runReaderT (gauge threadCountG) _mon
  whereException nm . E.bracket_ (inc tG) (dec tG) $ a

forkR :: Monitor -> Text -> IO () -> IO ThreadId
forkR _mon nm a = do
  tG <- runReaderT (gauge threadCountG) _mon
  forkIO . whereException nm . E.bracket_ (inc tG) (dec tG) $ a

whereException :: Text -> IO a -> IO a
whereException w = 
  E.handle (\(e::E.SomeException) -> TIO.putStrLn (T.concat ["Exception in ", w, ": ", T.pack $ show e]) >> E.throw e)
