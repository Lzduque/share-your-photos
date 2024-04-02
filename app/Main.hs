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
import qualified Data.List as L
import qualified Data.Maybe as M

data ImageRequest = ImageRequest
    { content :: T.Text
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
        -- MIO.liftIO $ putStrLn $ "1. Received body: " ++ show b  -- Debug print
        response <- MIO.liftIO $ extractImages imageSetRef b
        Scotty.setHeader "Content-Type" "application/json"
        Scotty.raw response

extractImages :: IORef.IORef (Set.Set T.Text) -> BS.ByteString -> IO BS.ByteString
extractImages imageSetRef bs = do
    let req = A.decode bs :: Maybe ImageRequest  -- Use bs directly
    case req of
        Just r -> do
            -- MIO.liftIO $ putStrLn $ "a 2. Decoded request: " ++ show r  -- Debug print
            let tags = TS.parseTags $ content r
            -- MIO.liftIO $ putStrLn $ "a 3. Parsed tags: " ++ show tags  -- Debug print
            -- let rows = [divContent | divTag@(TS.TagOpen "div" attrs) <- tags, ("role", "row") `elem` attrs, 
            --               let divContent = takeWhile (/= TS.TagClose "div") (dropWhile (/= divTag) tags)]
            -- MIO.liftIO $ putStrLn $ "a 4. rows: " ++ show rows  -- Debug print
            -- imgSrcsInRowsResults <- mapM findFirstBlobImgSrc rows  -- Run each IO action in the list and collect the results
            -- MIO.liftIO $ putStrLn $ "a 5. imgSrcsInRows: " ++ show imgSrcsInRowsResults  -- Now you can use 'show' because the results are not in the IO monad anymore
            let imgSrcs = Set.fromList [srcValue | TS.TagOpen "img" attrs <- tags, ("src", srcValue) <- attrs, "blob:" `T.isPrefixOf` srcValue]
            alreadyStored <- IORef.readIORef imageSetRef
            let newSrcs = Set.difference imgSrcs alreadyStored
            IORef.modifyIORef imageSetRef (`Set.union` newSrcs)
            -- MIO.liftIO $ putStrLn $ "a 6. New sources: " ++ show newSrcs  -- Debug print
            if Set.null newSrcs
                then do
                    -- MIO.liftIO $ putStrLn "a 7 a. No new images"  -- Debug print
                    return $ A.encode $ A.object []
                else do
                    -- MIO.liftIO $ putStrLn $ "a 7 b. Adding new images: " ++ show (Set.toList newSrcs)  -- Debug print
                    return $ A.encode $ A.object ["imageUrls" A..= Set.toList newSrcs]
        Nothing -> do
            -- MIO.liftIO $ putStrLn "b 1. Failed to decode JSON"  -- Debug print
            return $ A.encode $ A.object ["error" A..= ("Failed to decode JSON" :: T.Text)]

findFirstBlobImgSrc :: [TS.Tag T.Text] -> IO (Maybe T.Text)
findFirstBlobImgSrc rowTags = do
    let imgTags = filter isImgTag rowTags
    putStrLn $ "5 a. Image Tags: " ++ show imgTags  -- Debug print

    let blobImgSrcs = M.mapMaybe extractSrc imgTags
    putStrLn $ "5 b. Blob Image Sources: " ++ show blobImgSrcs  -- Debug print

    let result = L.find isBlobImgSrc blobImgSrcs
    putStrLn $ "5 c. Result: " ++ show result  -- Debug print
    return result  -- Lift the Maybe result into IO

  where
    isImgTag :: TS.Tag T.Text -> Bool
    isImgTag (TS.TagOpen tag _) = tag == "img"
    isImgTag _ = False

    isBlobImgSrc :: T.Text -> Bool
    isBlobImgSrc src = "blob:" `T.isPrefixOf` src

    extractSrc :: TS.Tag T.Text -> Maybe T.Text
    extractSrc (TS.TagOpen _ attrs) =
        let srcAttr = lookup "src" attrs  -- srcAttr is already Maybe T.Text, no need to pack
        in srcAttr  -- Simply return srcAttr
    extractSrc _ = Nothing
