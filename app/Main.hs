{-# LANGUAGE OverloadedStrings #-}
module Main where

import View (renderFilesToHtml)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze.Html.Renderer.Text as RT


import Web.Scotty


home :: ScottyM ()
home = do
  get "/" serveFiles



serveFiles :: ActionM ()
serveFiles = do
  html $ "hello"

main :: IO ()
main = do
  files <- renderFilesToHtml
  putStrLn $ T.unpack $ RT.renderHtml files

-- main = scotty 3000 $
--   home
