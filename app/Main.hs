{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Data.Text (Text, isPrefixOf, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- Custom CORS policy
myCors :: Middleware
myCors = cors $ const $ Just corsResourcePolicy
  where
    corsResourcePolicy = CorsResourcePolicy
      { corsOrigins = Nothing  -- Allow all origins
      , corsMethods = ["GET", "POST"]  -- Methods allowed
      , corsRequestHeaders = ["Content-Type"]  -- Headers allowed
      , corsExposedHeaders = Nothing  -- Expose no additional headers
      , corsMaxAge = Nothing  -- Cache preflight request results for no time
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

main :: IO ()
main = do
    putStrLn "Starting server on port 3001..."
    run 3001 $ myCors app

app :: Application
app req respond
  | pathInfo req == ["send-image"] = do
      putStrLn "1. Received request to /send-image"
      body <- requestBody req  -- body is a lazy ByteString
      putStrLn ("2. Received body: " ++ show body)
      let response = echoJson body
      putStrLn ("3. Response: " ++ show response)
      respond $ responseLBS status200 [("Content-Type", "application/json")] response
  | otherwise = do
      putStrLn ("1. Unspecified Path: " ++ show (pathInfo req))
      respond $ responseLBS status200 [] "Server is running"

echoJson :: ByteString -> LBS.ByteString
echoJson bs = encode $ object ["echoedUrl" .= decodeUtf8 bs]
-- echoJson bs =
--     let urlText = decodeUtf8 bs -- Convert ByteString to Text for easier manipulation
--     in if "blob:" `isPrefixOf` urlText
--        then encode $ object ["echoedUrl" .= urlText]
--        else encode $ object [] -- Use 'object []' to create an empty JSON object
