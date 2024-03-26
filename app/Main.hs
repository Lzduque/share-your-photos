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
import qualified Data.Text as T
import Text.HTML.TagSoup
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.IORef (newIORef, IORef, readIORef, modifyIORef)

data ImageRequest = ImageRequest
    { row :: T.Text
    , dataId :: T.Text
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
    imageSetRef <- newIORef Set.empty  -- Initialize the IORef to an empty Set
    putStrLn "Starting server on port 3001..."
    run 3001 $ myCors (app imageSetRef)

app :: IORef (Set.Set T.Text) -> Application
app imageSetRef req respond
  | pathInfo req == ["send-image"] = do
      putStrLn "1. Received request to /send-image"
      body <- requestBody req  -- body is a lazy ByteString
      -- putStrLn ("2. Received body: " ++ show body)
      response <- echoJson imageSetRef body
      putStrLn ("3. Response: " ++ show response)
      respond $ responseLBS status200 [("Content-Type", "application/json")] response
  | otherwise = do
      putStrLn ("1. Unspecified Path: " ++ show (pathInfo req))
      respond $ responseLBS status200 [] "Server is running"

echoJson :: IORef (Set.Set T.Text) -> ByteString -> IO LBS.ByteString
echoJson imageSetRef bs = case decode (LBS.fromStrict bs) :: Maybe ImageRequest of
    Just req -> do
        let tags = parseTags $ row req
        putStrLn ("2. tags: " ++ show tags)
        let imgSrcs = Set.fromList [srcValue | TagOpen "img" attrs <- tags, ("src", srcValue) <- attrs, "blob:" `T.isPrefixOf` srcValue]
        -- putStrLn ("2. imgSrcs: " ++ show imgSrcs)
        alreadyStored <- readIORef imageSetRef
        putStrLn ("2. alreadyStored: " ++ show alreadyStored)
        let newSrcs = Set.difference imgSrcs alreadyStored  -- Determine new sources that aren't already stored
        -- putStrLn ("2. newSrcs: " ++ show newSrcs)
        modifyIORef imageSetRef (`Set.union` newSrcs)  -- Add new sources to the global Set
        if Set.null newSrcs
           then do
               putStrLn $ "2. No new images. Data ID: " ++ T.unpack (dataId req)
               return $ encode $ object ["dataId" .= (dataId req)]
           else do
               putStrLn $ "2. New images: " ++ show (Set.toList newSrcs) ++ " " ++ T.unpack (dataId req)
               return $ encode $ object ["imageUrls" .= Set.toList newSrcs, "dataId" .= (dataId req)]
    Nothing -> do
        putStrLn "2. Failed to decode JSON"
        return $ encode $ object ["error" .= ("Failed to decode JSON" :: T.Text)]

-- extractImageSources :: T.Text -> [T.Text]
-- extractImageSources html = 
--     let tags = parseTags html
--         imgs = filter (isTagOpenName "img") tags
--         srcs = [fromAttrib "src" tag | tag@(TagOpen "img" _) <- tags] -- Extract src directly
--     in srcs  -- No need for catMaybes or maybeTagT.Text
