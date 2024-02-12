{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Data.Maybe qualified as Maybe
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy.IO qualified as TIO
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
    S.post "/webhook" $ do
      requestParams <- S.params
      media <- S.param "Media" `S.rescue` (\_ -> return ("" :: String))
      requestBody <- S.body -- Get the raw request body
      let requestBodyText = TLE.decodeUtf8 requestBody -- Decode it as UTF-8 text
      S.liftAndCatchIO $ TIO.putStrLn requestBodyText -- Print the request body
      S.liftAndCatchIO $ print requestParams -- Print the request body
      S.liftAndCatchIO $ print media -- Print the request body
      S.text "Received"

-- curl -X GET "https://api.twilio.com/2010-04-01/Accounts/AC48feacc222bc35649aa57d5463165c1a/Messages/IM3c54aedf24144037836fa7ae65a9754f/Media.json?PageSize=20" \
-- -u AC48feacc222bc35649aa57d5463165c1a:7e5dded2c1b7fe444baca89b2b453233
