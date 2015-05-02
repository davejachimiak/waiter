module Waiter (run) where

import Filesystem.Path.CurrentOS (decodeString)
import System.Exit (ExitCode(..))
import System.FSNotify (withManager, watchTree, Event)
import System.Process (waitForProcess, terminateProcess, spawnCommand)
import System.Process.Internals (ProcessHandle(..))
import Control.Monad (forever, unless, void)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Text.Regex (mkRegex, matchRegex)
import Data.List (delete)

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
              -> MVar ProcessHandle
              -> MVar ProcessHandle
              -> IO ()
buildAndServe commandLine currentBuild serverProcess = do
    maybeTerminateProcessFromMVar currentBuild
    build <- startBuild (buildCommand commandLine) currentBuild
    exitCode <- waitForProcess build

    case exitCode of
        ExitSuccess -> do
            maybeTerminateProcessFromMVar serverProcess
            startServer commandLine serverProcess
        ExitFailure _ -> return ()

startBuild :: String -> MVar ProcessHandle -> IO ProcessHandle
startBuild buildCommand currentBuild = do
    build <- spawnCommand buildCommand
    putMVar currentBuild build
    return build

blockBuildAndServe :: CommandLine
                   -> MVar Bool
                   -> MVar ProcessHandle
                   -> MVar ProcessHandle
                   -> Event
                   -> IO ()
blockBuildAndServe commandLine blockState currentBuild serverProcess _ = do
    doBlock <- readMVar blockState

    unless doBlock
        $ blockBatchEvents blockState
        >> buildAndServe commandLine currentBuild serverProcess

blockBatchEvents :: MVar Bool -> IO Bool
blockBatchEvents blockState = do
    swapMVar blockState True
    threadDelay 100000 -- microseconds: 0.1 seconds
    swapMVar blockState False

startServer :: CommandLine -> MVar ProcessHandle -> IO ()
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
