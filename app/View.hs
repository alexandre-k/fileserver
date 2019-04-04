{-# LANGUAGE OverloadedStrings #-}
module View
  (renderFilesToHtml
  )where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Controller (getLocalFiles)
import Data.Monoid (mconcat)
import Data.Text as T
import Text.Blaze as TB
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text as HRT
import Text.Blaze.Html.Renderer.String as HRS
import Text.Blaze.Internal as BI


showAllFiles :: T.Text -> [T.Text] -> H.Html
showAllFiles dirname contents = do
  H.docTypeHtml $ do
    H.head $ do
      H.title "Local file server"
    H.body $ do
      H.h1 "Files in:" <> H.toHtml dirname
      H.ul $ mapM_ (H.li . linkToDir . H.toHtml) contents
      where
        linkToDir = flink (BI.textValue dirname)
      -- H.ul $ H.li $ H.toHtml $ createLinks (dirname, contents)


renderFilesToHtml :: IO (H.Html)
renderFilesToHtml = do
  (title, contents) <- getLocalFiles
  return $ showAllFiles title contents

flink :: AttributeValue -> H.Html -> H.Html
flink url name = H.a ! HA.href url $ name
-- createLinks :: (T.Text, [T.Text]) -> H.Html
-- createLinks dirname contents = H.ul $ mconcat $ fmap (H.li . H.a) (fmap H.toHtml contents)
