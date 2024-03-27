{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import qualified Data.Set as Set
import qualified Data.IORef as IORef
import qualified Web.Scotty as Scotty
import qualified Control.Monad.IO.Class as MIO

data ImageRequest = ImageRequest
    { row :: T.Text
    } deriving (Show, Generic, A.FromJSON)

main :: IO ()
main = do
    imageSetRef <- IORef.newIORef Set.empty  -- Initialize the IORef to an empty Set
    putStrLn "Starting server on port 3001..."
    Scotty.scotty 3001 $ app imageSetRef

app :: IORef.IORef (Set.Set T.Text) -> Scotty.ScottyM ()
app imageSetRef = do
    -- Add OPTIONS route for preflight requests
    Scotty.options (Scotty.regex ".*") $ do
        Scotty.setHeader "Access-Control-Allow-Origin" "*"
        Scotty.setHeader "Access-Control-Allow-Methods" "POST, OPTIONS"
        Scotty.setHeader "Access-Control-Allow-Headers" "Content-Type"

    Scotty.post "/send-image" $ do
        Scotty.setHeader "Access-Control-Allow-Origin" "*"
        b <- Scotty.body
        MIO.liftIO $ putStrLn $ "1. Received body: " ++ show b  -- Debug print
        response <- MIO.liftIO $ extractImages imageSetRef b
        Scotty.setHeader "Content-Type" "application/json"
        Scotty.raw response

extractImages :: IORef.IORef (Set.Set T.Text) -> BS.ByteString -> IO BS.ByteString
extractImages imageSetRef bs = do
    let req = A.decode bs :: Maybe ImageRequest  -- Use bs directly
    case req of
        Just r -> do
            -- MIO.liftIO $ putStrLn $ "a 2. Decoded request: " ++ show r  -- Debug print
            let tags = TS.parseTags $ row r
            MIO.liftIO $ putStrLn $ "a 3. Parsed tags: " ++ show tags  -- Debug print
            let imgSrcs = Set.fromList [srcValue | TS.TagOpen "img" attrs <- tags, ("src", srcValue) <- attrs, "blob:" `T.isPrefixOf` srcValue]
            -- MIO.liftIO $ putStrLn $ "a 4. Image sources: " ++ show imgSrcs  -- Debug print
            alreadyStored <- IORef.readIORef imageSetRef
            -- MIO.liftIO $ putStrLn $ "a 5. Already stored: " ++ show alreadyStored  -- Debug print
            let newSrcs = Set.difference imgSrcs alreadyStored
            IORef.modifyIORef imageSetRef (`Set.union` newSrcs)
            MIO.liftIO $ putStrLn $ "a 6. New sources: " ++ show newSrcs  -- Debug print
            if Set.null newSrcs
                then do
                    MIO.liftIO $ putStrLn "a 7 a. No new images"  -- Debug print
                    return $ A.encode $ A.object []
                else do
                    MIO.liftIO $ putStrLn $ "a 7 b. Adding new images: " ++ show (Set.toList newSrcs)  -- Debug print
                    return $ A.encode $ A.object ["imageUrls" A..= Set.toList newSrcs]
        Nothing -> do
            MIO.liftIO $ putStrLn "b 1. Failed to decode JSON"  -- Debug print
            return $ A.encode $ A.object ["error" A..= ("Failed to decode JSON" :: T.Text)]
