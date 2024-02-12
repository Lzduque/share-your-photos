{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Data.Maybe qualified as Maybe
-- import Data.Text.Lazy.Encoding qualified as TLE
-- import Data.Text.Lazy.IO qualified as TIO
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
      mediaUrl0 <- S.param "MediaUrl0" `S.rescue` (\_ -> return ("" :: String))
      mediaContentType0 <- S.param "MediaContentType0" `S.rescue` (\_ -> return ("" :: String))

      if mediaContentType0 == "image/jpeg" -- we have to handle is there is no "mediaUrl0" field (message is not an image) AND if message is not a photo (sticker) OR video -- types: '''' , video/mp4 ... ONLY ACCEPTABLE TYPE: image/jpeg
        then do
          S.text "Photo recieved! You should see it soon on the screen!" -- Respond with the challenge if the verification is successful
          S.liftAndCatchIO $ print mediaUrl0 -- Print the request body
          S.liftAndCatchIO $ print mediaContentType0 -- Print the request body
        else do
          S.text "Please, only share photos!" -- Send a 400 status code if verification fails
          -- S.status HTTP.status400 -- if I do this withou a timer, the message is not sent, cause it sends only the 400 first.

-- curl -X GET "https://api.twilio.com/2010-04-01/Accounts/AC48feacc222bc35649aa57d5463165c1a/Messages/IM3c54aedf24144037836fa7ae65a9754f/Media.json?PageSize=20" \
-- -u AC48feacc222bc35649aa57d5463165c1a:7e5dded2c1b7fe444baca89b2b453233

-- curl -u AC48feacc222bc35649aa57d5463165c1a:7e5dded2c1b7fe444baca89b2b453233 -G https://mcs.us1.twilio.com/v1/Services/IS263cd2066c9c449c91d314427843ad9c/Media/ME96fccc1b72fd06f31675af43681fc335
-- IS263cd2066c9c449c91d314427843ad9c

-- curl -X GET "https://api.twilio.com/2010-04-01/Accounts/AC48feacc222bc35649aa57d5463165c1a/Messages/MM4b5e9cb7b521263741492afd002be65c/Media/ME96fccc1b72fd06f31675af43681fc335" \
-- -u AC48feacc222bc35649aa57d5463165c1a:7e5dded2c1b7fe444baca89b2b453233

-- MediaUrl0