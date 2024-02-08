{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Network.HTTP.Types (status200)
import Web.Scotty

shareyourphotos :: IO ()
shareyourphotos = scotty 3000 $ do
  post "/webhook" $ do
    status status200
    text "Webhook received!"
