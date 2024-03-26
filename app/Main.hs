{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, object, (.=), encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text as T
import Text.HTML.TagSoup
import qualified Data.Set as Set
import Data.IORef (newIORef, IORef, readIORef, modifyIORef)
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Header 

data ImageRequest = ImageRequest
    { row :: T.Text
    } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
    imageSetRef <- newIORef Set.empty  -- Initialize the IORef to an empty Set
    putStrLn "Starting server on port 3001..."
    scotty 3001 $ app imageSetRef

app :: IORef (Set.Set T.Text) -> ScottyM ()
app imageSetRef = do
    -- Add OPTIONS route for preflight requests
    options (regex ".*") $ do
        setHeader "Access-Control-Allow-Origin" "*"
        setHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
        setHeader "Access-Control-Allow-Headers" "Content-Type"

    post "/send-image" $ do
        setHeader "Access-Control-Allow-Origin" "*"
        b <- body
        liftIO $ putStrLn $ "Received body: " ++ show b  -- Debug print
        response <- liftIO $ echoJson imageSetRef b
        setHeader "Content-Type" "application/json"
        raw response

echoJson :: IORef (Set.Set T.Text) -> ByteString -> IO ByteString
echoJson imageSetRef bs = do
    let req = decode bs :: Maybe ImageRequest  -- Use bs directly
    case req of
        Just r -> do
            liftIO $ putStrLn $ "Decoded request: " ++ show r  -- Debug print
            let tags = parseTags $ row r
            liftIO $ putStrLn $ "Parsed tags: " ++ show tags  -- Debug print
            let imgSrcs = Set.fromList [srcValue | TagOpen "img" attrs <- tags, ("src", srcValue) <- attrs, "blob:" `T.isPrefixOf` srcValue]
            liftIO $ putStrLn $ "Image sources: " ++ show imgSrcs  -- Debug print
            alreadyStored <- readIORef imageSetRef
            liftIO $ putStrLn $ "Already stored: " ++ show alreadyStored  -- Debug print
            let newSrcs = Set.difference imgSrcs alreadyStored
            modifyIORef imageSetRef (`Set.union` newSrcs)
            liftIO $ putStrLn $ "New sources: " ++ show newSrcs  -- Debug print
            if Set.null newSrcs
                then do
                    liftIO $ putStrLn "No new images"  -- Debug print
                    return $ encode $ object []
                else do
                    liftIO $ putStrLn $ "Adding new images: " ++ show (Set.toList newSrcs)  -- Debug print
                    return $ encode $ object ["imageUrls" .= Set.toList newSrcs]
        Nothing -> do
            liftIO $ putStrLn "Failed to decode JSON"  -- Debug print
            return $ encode $ object ["error" .= ("Failed to decode JSON" :: T.Text)]
