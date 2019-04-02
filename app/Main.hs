{-# LANGUAGE OverloadedStrings #-}
module Main where

import View (renderFilesToHtml)
import qualified Data.Text.Lazy as T


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
  putStrLn $ T.unpack files

-- main = scotty 3000 $
--   home
