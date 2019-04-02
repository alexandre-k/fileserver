{-# LANGUAGE OverloadedStrings #-}
module View
  (renderFilesToHtml
  )where

import Control.Monad.IO.Class
import Controller (getLocalFiles)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import qualified Text.Blaze as TB
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Text as HRT
import qualified Text.Blaze.Html.Renderer.String as HRS
import qualified Text.Blaze.Internal as BI


showAllFiles :: T.Text -> T.Text -> T.Text
showAllFiles dirname contents = HRT.renderHtml $ H.toHtml $ do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Local file server"
    H.body $ do
      H.h1 "Files in:" <> H.toHtml dirname
      H.ul $ H.li . H.a (H.!) HA.href "/home/user/file.txt" $ "/home/user/file.txt"
      -- H.ul $ H.li $ H.toHtml $ createLinks (dirname, contents)


renderFilesToHtml :: IO (T.Text)
renderFilesToHtml = do
  (title, contents) <- getLocalFiles
  return $ showAllFiles title contents

createLinks :: (T.Text, T.Text) -> T.Text
createLinks (dirname, contents) = contents
-- createLinks :: (T.Text, [T.Text]) -> H.Html
-- createLinks dirname contents = H.ul $ mconcat $ fmap (H.li . H.a) (fmap H.toHtml contents)
