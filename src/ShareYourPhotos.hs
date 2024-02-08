{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 qualified as H
import Web.Scotty (get, html, scotty)

shareyourphotos :: IO ()
shareyourphotos =
  scotty 3000 $
    get "/" $
      html $
        renderHtml $
          H.html $
            H.body $ do
              H.h1 "Share Your Photos"
