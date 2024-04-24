{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Control
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.IORef as IORef
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LTE
import qualified GHC.Generics as Generics
import System.IO
import qualified Text.HTML.Scalpel as Scalpel
import qualified Text.StringLike as StringLike
import qualified Web.Scotty as Scotty

data ImageRequest = ImageRequest
  { content :: T.Text
  }
  deriving (Show, Generics.Generic, A.FromJSON)

main :: IO ()
main = do
  imageSetRef <- IORef.newIORef Set.empty -- Initialize the IORef to an empty Set
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

getRowDivs :: StringLike.StringLike str => Scalpel.Scraper str [str]
getRowDivs =
  Scalpel.chroots ("div" Scalpel.@: ["role" Scalpel.@= "row"]) $ do
    contents <- Scalpel.html Scalpel.anySelector
    return contents

altTextAndImages :: Scalpel.Scraper String [String]
altTextAndImages =
  -- 1. First narrow the current context to each img tag.
  Scalpel.chroots "img" $ do
    -- 2. Use Any to access all the relevant content from the the currently
    -- selected img tag.
    srcUrl <- Scalpel.attr "src" Scalpel.anySelector
    Control.guard ("blob:" `T.isPrefixOf` (StringLike.fromString srcUrl))
    -- 3. Combine the retrieved content into the desired final result.
    return (srcUrl)

cleanUp :: Maybe [String] -> Bool
cleanUp (Just []) = False
cleanUp _ = True

extractImages :: IORef.IORef (Set.Set T.Text) -> BS.ByteString -> IO BS.ByteString
extractImages imageSetRef bs = do
  let req = A.decode bs :: Maybe ImageRequest -- Use bs directly
  case req of
    Just r -> do
      let htmlContent = T.unpack $ content r :: String
      -- putStrLn $ "\n  1. htmlContent: " ++ show htmlContent  -- Debug print -- this is not showing up the new blobs, only the first data:image, by testing I think the front is not sending the server this part of the code.

      -- ------NEW CODE
      -- htmlContent' <- readFromFile "app/test_file.txt" -- get data from testing file, not extension
      -- putStrLn $ "\n  1. htmlContent: " ++ show htmlContent'  -- Debug print
      let divs' = Scalpel.scrapeStringLike htmlContent getRowDivs :: Maybe [String]
      -- putStrLn $ "\n  2. divs': " ++ show divs' -- Debug print
      let x = M.fromMaybe [] divs' -- this is not showing up the new blobs, only the first data:image
      -- putStrLn $ "\n  3. xs: " ++ (L.intercalate "\n  x: " . map show $ x) -- Debug print
      let sources = map (\y -> Scalpel.scrapeStringLike y altTextAndImages) x
      -- putStrLn $ "\n  4. sources: " ++ (L.intercalate "\n  source: " . map show $ sources) -- Debug print
      let blobs = M.catMaybes $ filter cleanUp sources
      -- putStrLn $ "\n  5. blobs: " ++ (L.intercalate "\n  blob: " . map show $ blobs) -- Debug print
      -- the blob unit (map through it) will have an array of blobs from the samer role=row -> get the last one only!
      let lastBlob = map last blobs

      -- ------NEW CODE ENDS
      alreadyStored <- IORef.readIORef imageSetRef
      let newSrcs = Set.difference (Set.fromList (map T.pack lastBlob)) alreadyStored
      IORef.modifyIORef imageSetRef (`Set.union` newSrcs)
      -- putStrLn $ "--- New sources: " ++ show newSrcs -- Debug print
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

readFromFile :: FilePath -> IO String
readFromFile filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle
