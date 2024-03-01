{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors
import Web.Scotty

data Info = Info {url :: String} deriving (Show, Generic)

instance FromJSON Info

instance ToJSON Info

main :: IO ()
main = scotty 3000 $ do
  middleware $
    cors
      ( const $
          Just
            simpleCorsResourcePolicy
              { corsRequestHeaders = ["Content-Type"],
                corsOrigins = Just (["chrome-extension://hiopepkbgdgkinjhhhenahmepgfaoacc"], True)
              }
      )
  get "/" $ do
    liftIO $ putStrLn "Received GET request at /"
    text "Hello, world!"

  post "/data" $ do
    info <- jsonData :: ActionM Info
    liftIO $ putStrLn "Received POST request at /data with the following content:"
    liftIO $ print info -- Print the content of the Info data type
    json info -- Echo back the received JSON
