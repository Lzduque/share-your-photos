{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Web.Scotty

shareyourphotos :: IO ()
shareyourphotos = do
  -- Fetch the token from the environment variable
  token <- lookupEnv "TOKEN"
  let tokenValue = fromMaybe "" token -- Use a default value if TOKEN is not found
  scotty 3000 $ do
    get "/webhook" $ do
      mode <- param "hub.mode" `rescue` (\_ -> return "")
      verifyToken <- param "hub.verify_token" `rescue` (\_ -> return "")
      challenge <- param "hub.challenge" `rescue` (\_ -> return "")

      -- Check if the mode is 'subscribe' and the token matches
      if mode == "subscribe" && verifyToken == tokenValue
        then text challenge -- Respond with the challenge if the verification is successful
        else status 400 -- Send a 400 status code if verification fails
