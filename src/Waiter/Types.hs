module Waiter.Types where

import Text.Regex (mkRegex, matchRegex)
import qualified Filesystem.Path.CurrentOS as OS (FilePath, encodeString) 
import System.FSNotify (Event(..))
import System.Process.Internals (ProcessHandle(..))

data CommandLine = CommandLine
    { serverCommand :: String
    , buildCommand :: String
    , fileRegex :: String
    , dir :: String }

instance Eq ProcessHandle where
    (ProcessHandle handle1 _) == (ProcessHandle handle2 _) = handle1 == handle2

class FileDoesMatch a where
    fileDoesMatch :: String -> a -> Bool

instance FileDoesMatch Event where
    fileDoesMatch regex (Added filePath _) = fileDoesMatch' regex filePath
    fileDoesMatch regex (Modified filePath _) = fileDoesMatch' regex filePath
    fileDoesMatch regex (Removed filePath _) = fileDoesMatch' regex filePath

fileDoesMatch' :: String -> OS.FilePath -> Bool
fileDoesMatch' regex filePath =
    let fileName = OS.encodeString filePath

    in case matchRegex (mkRegex regex) fileName of
        Just _ -> True
        Nothing -> False
