{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import GHC.Generics (Generic)
import Data.Aeson (encode, decode, FromJSON, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Data.Text (Text, isPrefixOf, pack)
import Text.HTML.TagSoup
import Data.Maybe (catMaybes)

data ImageRequest = ImageRequest
    { row :: Text
    , dataId :: Text
    } deriving (Show, Generic, FromJSON)

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
      -- putStrLn ("2. Received body: " ++ show body)
      let response = echoJson body
      putStrLn ("3. Response: " ++ show response)
      respond $ responseLBS status200 [("Content-Type", "application/json")] response
  | otherwise = do
      putStrLn ("1. Unspecified Path: " ++ show (pathInfo req))
      respond $ responseLBS status200 [] "Server is running"

echoJson :: ByteString -> LBS.ByteString
echoJson bs = case decode (LBS.fromStrict bs) :: Maybe ImageRequest of
    Just req ->
        let tags = parseTags $ row req
            imgSrcs = [srcValue | TagOpen "img" attrs <- tags, ("src", srcValue) <- attrs, "blob:" `isPrefixOf` srcValue]
        in if null imgSrcs
           then encode $ object ["dataId" .= (dataId req)]  -- No 'blob:' images found, return an empty JSON object
           else encode $ object ["imageUrls" .= imgSrcs, "dataId" .= (dataId req)]  -- Encode the list of 'blob:' image URLs
    Nothing -> encode $ object ["error" .= ("Failed to decode JSON" :: Text)]

extractImageSources :: Text -> [Text]
extractImageSources html = 
    let tags = parseTags html
        imgs = filter (isTagOpenName "img") tags
        srcs = [fromAttrib "src" tag | tag@(TagOpen "img" _) <- tags] -- Extract src directly
    in srcs  -- No need for catMaybes or maybeTagText
