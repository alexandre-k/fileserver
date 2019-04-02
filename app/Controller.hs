{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Cont (forM)
import Data.Monoid (mconcat)
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import System.Directory (getCurrentDirectory, listDirectory)


getLocalFiles :: IO (T.Text, T.Text)
getLocalFiles = do
  currentDir <- getCurrentDirectory
  allFilesCurrentDir <- listDirectory currentDir
  return (T.pack $ show currentDir, T.pack $ show allFilesCurrentDir)
