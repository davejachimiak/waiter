module Waiter.FileMatcher (fileDoesMatch) where

import Text.Regex (mkRegex, matchRegex)
import qualified Filesystem.Path.CurrentOS as OS (FilePath, encodeString) 
import System.FSNotify (Event(..))

fileDoesMatch regex (Added filePath _) = fileDoesMatch' regex filePath
fileDoesMatch regex (Modified filePath _) = fileDoesMatch' regex filePath
fileDoesMatch regex (Removed filePath _) = fileDoesMatch' regex filePath

fileDoesMatch' :: String -> OS.FilePath -> Bool
fileDoesMatch' regex filePath =
    let fileName = OS.encodeString filePath

    in case matchRegex (mkRegex regex) fileName of
        Just _ -> True
        Nothing -> False
