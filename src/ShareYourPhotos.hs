{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Data.Maybe qualified as Maybe
import Network.HTTP.Types qualified as HTTP
import System.Environment qualified as Env
import Web.Scotty qualified as S

shareyourphotos :: IO ()
shareyourphotos = do
  -- Fetch the token from the environment variable
  token <- Env.lookupEnv "TOKEN"
  let tokenValue = Maybe.fromMaybe "" token -- Use a default value if TOKEN is not found
  S.scotty 3000 $ do
    S.get "/webhook" $ do
      mode <- S.param "hub.mode" `S.rescue` (\_ -> return "")
      verifyToken <- S.param "hub.verify_token" `S.rescue` (\_ -> return "")
      challenge <- S.param "hub.challenge" `S.rescue` (\_ -> return "")
      -- Check if the mode is 'subscribe' and the token matches
      if mode == ("subscribe" :: String) && verifyToken == tokenValue
        then S.text challenge -- Respond with the challenge if the verification is successful
        else S.status HTTP.status400 -- Send a 400 status code if verification fails
