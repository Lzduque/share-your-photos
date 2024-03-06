{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Data.Text (Text)
import Network.Wai qualified as Wai
import Network.Wai.Application.Static qualified as Static
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWS
import Network.WebSockets qualified as WS

app :: Wai.Application
app = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp backupApp
  where
    -- Corrected backupApp definition
    backupApp = Static.staticApp (Static.defaultFileServerSettings "static")

wsApp :: WS.ServerApp
wsApp pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.forkPingThread conn 30 -- Keep the connection alive
  putStrLn "Client connected"
  -- Echo messages back to the client
  let loop = do
        msg <- WS.receiveData conn
        WS.sendTextData conn (msg :: Text)
        loop
  loop

main :: IO ()
main = do
  let port = 3001
  putStrLn $ "Starting server on port " ++ show port
  Warp.run port app
