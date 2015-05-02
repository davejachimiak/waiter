module Waiter (run) where

import Filesystem.Path.CurrentOS (decodeString)
import qualified Filesystem.Path.CurrentOS as OS (FilePath, encodeString) 
import System.Exit (ExitCode(..))
import System.FSNotify (Event(..), withManager, watchTree, Event)
import System.Process (waitForProcess, terminateProcess, spawnCommand)
import System.Process.Internals (ProcessHandle(..))
import Control.Monad (forever, unless, void)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Regex (mkRegex, matchRegex)
import Data.List (delete)
import Data.Maybe (isJust)

import Waiter.Types

run :: CommandLine -> IO ()
run commandLine = do
    let regexToWatch = fileRegex commandLine
        dirToWatch = decodeString $ dir commandLine

    currentBuild <- newEmptyMVar
    blockState <- newMVar False
    serverProcess <- newEmptyMVar

    buildAndServe commandLine currentBuild serverProcess

    withManager $ \mgr -> do
        watchTree
            mgr
            dirToWatch
            (fileDoesMatch regexToWatch)
            $ blockBuildAndServe commandLine blockState currentBuild serverProcess

        forever getLine

buildAndServe :: CommandLine
              -> CurrentBuild
              -> ServerProcess
              -> IO ()
buildAndServe commandLine currentBuild serverProcess = do
    maybeTerminateProcessFromMVar currentBuild

    startBuild (buildCommand commandLine) currentBuild
        >>= waitForProcess
        >>= runServer commandLine serverProcess

startBuild :: String -> CurrentBuild -> IO ProcessHandle
startBuild buildCommand currentBuild = do
    build <- spawnCommand buildCommand
    putMVar currentBuild build
    return build

runServer :: CommandLine -> ServerProcess -> ExitCode -> IO ()
runServer commandLine serverProcess ExitSuccess = do
    maybeTerminateProcessFromMVar serverProcess
    startServer commandLine serverProcess
runServer _ _ _ = return ()

blockBuildAndServe :: CommandLine
                   -> BlockState
                   -> CurrentBuild
                   -> ServerProcess
                   -> Event
                   -> IO ()
blockBuildAndServe commandLine blockState currentBuild serverProcess _ = do
    doBlock <- readMVar blockState

    unless doBlock
        $ blockBatchEvents blockState
        >> buildAndServe commandLine currentBuild serverProcess

blockBatchEvents :: BlockState -> IO Bool
blockBatchEvents blockState = do
    swapMVar blockState True
    threadDelay 100000 -- microseconds: 0.1 seconds
    swapMVar blockState False

startServer :: CommandLine -> ServerProcess -> IO ()
startServer commandLine serverProcess = do
    newServerProcess <- spawnCommand $ serverCommand commandLine
    putMVar serverProcess newServerProcess
    return ()

maybeTerminateProcessFromMVar :: MVar ProcessHandle -> IO ()
maybeTerminateProcessFromMVar processMVar = do
    process <- tryTakeMVar processMVar

    case process of
        Just process -> terminateProcess process
        Nothing -> return ()

fileDoesMatch :: String -> Event -> Bool
fileDoesMatch regex (Added filePath _) = fileDoesMatch' regex filePath
fileDoesMatch regex (Modified filePath _) = fileDoesMatch' regex filePath
fileDoesMatch regex (Removed filePath _) = fileDoesMatch' regex filePath

fileDoesMatch' :: String -> OS.FilePath -> Bool
fileDoesMatch' regex filePath =
    isJust $ matchRegex (mkRegex regex) (OS.encodeString filePath)
