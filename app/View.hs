{-# LANGUAGE OverloadedStrings #-}
module View where

import qualified Controller (getLocalFiles)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze as TB
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as HRT
import qualified Text.Blaze.Html.Renderer.String as HRS
import qualified Text.Blaze.Internal as BI


showAllFiles :: T.Text -> [T.Text] -> H.Html
showAllFiles dirname contents = H.toHtml $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Local file server"
    H.body $ do
      H.h1 "Files in:" <> H.toHtml dirname
      createLinks (dirname, contents)


renderFilesToHtml :: H.Html
renderFilesToHtml = do
  (title, contents) <- getLocalFiles
  HRT.renderHtml $ showAllFiles (T.pack title) (map T.pack contents)

createLinks :: (T.Text, [T.Text]) -> H.Html
createLinks dirname contents = H.ul $ mconcat $ fmap (H.li . H.a) (fmap H.toHtml contents)
