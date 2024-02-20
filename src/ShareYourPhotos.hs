{-# LANGUAGE OverloadedStrings #-}

module ShareYourPhotos where

-- import Data.Maybe qualified as Maybe
-- import Data.Text.Lazy.Encoding qualified as TLE
-- import Data.Text.Lazy.IO qualified as TIO
-- import Network.HTTP.Types qualified as HTTP
-- import System.Environment qualified as Env
-- import Web.Scotty qualified as S

import Test.WebDriver

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

main :: IO ()
main = runSession firefoxConfig $ do                      -- starts a WebDriver session with the given firefox config, then
                                                          -- runs the supplied commands

  openPage "http://google.com"                            -- tells the browser to open the URL http://google.com

  searchInput <- findElem ( ByCSS "input[type='text']" )  -- asks the browser to find an element on the page with the given
                                                          -- CSS selector then stores the resulting element in the varialbe 
                                                          -- named`searchInput`
                                                          
  sendKeys "Hello, World!" searchInput                    -- type into the element, as though a user had issued the
                                                          -- keystrokes `Hello, World!`
                                                          
  submit searchInput                                      -- submit the input form (technically not required with Google
                                                          -- but included for example purposes)

  closeSession                                            -- finally, close the WebDriver session and its associated
                                                          -- browser process
