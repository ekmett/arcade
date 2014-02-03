{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rogue.Server
  ( serverMain
  ) where

import Control.Monad
import Control.Exception as E
import Control.Exception.Lens
import Control.Concurrent
import Control.Lens
import Data.FileEmbed
import Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import Network.Wai.Application.Static
import Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Handler.Warp as Warp
import System.Process

import Rogue.Connection as Rogue
import Rogue.Monitor
import Rogue.Options

registerConnection :: Rogue.Connection -> IO ()
registerConnection r = do
  return ()

threadCountG :: Text
threadCountG = "thread count"

serverMain :: Options -> Monitor -> IO ()
serverMain opts mon = do
  let uri = serverUri opts
  putStrLn $ "Serving " ++ uri
  when (opts^.serverOpen) $ do
    _ <- system $ "/usr/bin/open " ++ uri
    return ()
  Warp.runSettings Warp.defaultSettings
    { Warp.settingsPort = opts^.serverPort
    , Warp.settingsTimeout = opts^.serverTimeout
    , Warp.settingsIntercept = WaiWS.intercept (app mon)
    } $ staticApp $ embeddedSettings $(embedDir "static")

data Hangup = Hangup deriving (Show,Typeable)
instance Exception Hangup

app :: Monitor -> ServerApp
app mon pending = countThread mon "websocket" $ do

  let rhead = pendingRequest pending
      rpath = requestPath rhead

  putStrLn $ "connection via " ++ show rpath


  conn <- WS.acceptRequest pending

  -- TODO: coin a UUID for this connecton, or take it from the URL
  WS.sendTextData conn ("0"::Text)

  input <- newChan
  output <- newChan

  receiverId <- forkR mon "websocket.read" $
    handle (\Hangup -> return ()) $
    forever $ do
      msg <- WS.receiveData conn
      writeChan input msg
      print msg

  senderId <- myThreadId

  registerConnection $ Connection
    { Rogue.send  = writeChan output
    , Rogue.recv  = readChan input
    , Rogue.close = throwTo senderId Hangup
    }

  whereException "websocket.write" $
    finally ?? throwTo receiverId Hangup $ do
      forever $ do
        msg <- readChan output
        WS.sendBinaryData conn msg

countThread :: Monitor -> Text -> IO a -> IO a
countThread mon nm a = do
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
