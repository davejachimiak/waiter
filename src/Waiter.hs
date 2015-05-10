module Waiter (run) where

import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path.CurrentOS as OS (FilePath) 
import System.FSNotify (Event(..), withManager, watchTree)
import Control.Monad (forever)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent (threadDelay)
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (isJust)

import Waiter.Types
import Waiter.BuildAndServe (buildAndServe)

run :: CommandLine -> IO ()
run commandLine = do
    (currentBuild, serverProcess) <- createStates

    buildAndServe commandLine currentBuild serverProcess ProgramStart

    withManager $ \mgr -> do
        watchTree
            mgr
            (decodeString $ dir commandLine)
            (fileDoesMatch $ fileRegex commandLine)
            $ buildAndServe commandLine currentBuild serverProcess

        forever getLine

createStates :: IO (CurrentBuild, ServerProcess)
createStates = do
    currentBuild <- newEmptyMVar
    serverProcess <- newEmptyMVar
    
    return (currentBuild, serverProcess)

fileDoesMatch :: String -> Event -> Bool
fileDoesMatch regex event = 
    isJust $ matchRegex (mkRegex regex) (eventFilepath event)

eventFilepath :: Event -> String
eventFilepath (Added filePath _) = encodeString filePath
eventFilepath (Modified filePath _) = encodeString filePath
eventFilepath (Removed filePath _) = encodeString filePath
