{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Web.Scotty

shareyourphotos :: IO ()
shareyourphotos =
  scotty 3000 $
    get "/" $
      html "<h1>Share Your Photos</h1>"
