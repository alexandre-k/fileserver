{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cont (forM)
import Data.Monoid (mconcat)
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import System.Directory (getCurrentDirectory, listDirectory)
import qualified Text.Blaze as TB
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as HRT
import qualified Text.Blaze.Html.Renderer.String as HRS
import qualified Text.Blaze.Internal as BI
import Web.Scotty


home :: ScottyM ()
home = do
  get "/" serveFiles


showAllFiles :: T.Text -> [T.Text] -> H.Html
showAllFiles dirname contents = H.toHtml $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Local file server"
    H.body $ do
      H.h1 "Files in:" <> H.toHtml dirname
      H.ul $ mconcat $ fmap H.li (fmap H.toHtml contents)

serveFiles :: ActionM ()
serveFiles = do
  currentDir <- liftIO getCurrentDirectory
  allFilesCurrentDir <- liftIO $ listDirectory currentDir
  html $ HRT.renderHtml $ showAllFiles (T.pack currentDir) (map T.pack allFilesCurrentDir)

main :: IO ()
main = scotty 3000 $
  home
