{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified View (renderFilesToHtml)


import Web.Scotty


home :: ScottyM ()
home = do
  get "/" serveFiles



serveFiles :: ActionM ()
serveFiles = do
  html $ "hello"

main :: IO ()
main = scotty 3000 $
  home
